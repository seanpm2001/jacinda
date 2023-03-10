{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Ty ( TyM
                  , Err (..)
                  , runTyM
                  , tyProgram
                  -- * For debugging
                  , tyOf
                  ) where

import           Control.Exception          (Exception)
import           Control.Monad              (forM)
import           Control.Monad.Except       (liftEither, throwError)
import           Control.Monad.State.Strict (StateT, gets, modify, runState, runStateT)
import           Data.Bifunctor             (first, second)
import           Data.Foldable              (traverse_)
import           Data.Functor               (void, ($>))
import qualified Data.IntMap                as IM
import           Data.Maybe                 (fromMaybe)
import           Data.Semigroup             ((<>))
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import           Data.Typeable              (Typeable)
import qualified Data.Vector                as V
import qualified Data.Vector.Ext            as V
import           Intern.Name
import           Intern.Unique
import           Jacinda.AST
import           Jacinda.Ty.Const
import           Prettyprinter              (Doc, Pretty (..), squotes, vsep, (<+>))

data Err a = UF a (T ()) (T ())
           | Doesn'tSatisfy a (T ()) C
           | IllScoped a (Name a)
           | Ambiguous (T K) (E ())
           | Expected K K
           | IllScopedTyVar (TyName ())

instance Pretty a => Pretty (Err a) where
    pretty (UF l ty ty')           = pretty l <+> "could not unify type" <+> squotes (pretty ty) <+> "with" <+> squotes (pretty ty')
    pretty (Doesn'tSatisfy l ty c) = pretty l <+> squotes (pretty ty) <+> "is not a member of class" <+> pretty c
    pretty (IllScoped l n)         = pretty l <+> squotes (pretty n) <+> "is not in scope."
    pretty (Ambiguous ty e)        = "type" <+> squotes (pretty ty) <+> "of" <+> squotes (pretty e) <+> "is ambiguous"
    pretty (Expected k0 k1)        = "Found kind" <+> pretty k0 <> ", expected kind" <+> pretty k1
    pretty (IllScopedTyVar n)      = "Type variable" <+> squotes (pretty n) <+> "is not in scope."

instance Pretty a => Show (Err a) where
    show = show . pretty

instance (Typeable a, Pretty a) => Exception (Err a) where

-- solve, unify etc. THEN check that all constraints are satisfied?
-- (after accumulating classVar membership...)
data TyState a = TyState { maxU        :: !Int
                         , kindEnv     :: IM.IntMap K
                         , classVars   :: IM.IntMap (S.Set (C, a))
                         , varEnv      :: IM.IntMap (T K)
                         , constraints :: S.Set (a, T K, T K)
                         }

prettyConstraints :: S.Set (b, T a, T a) -> Doc ann
prettyConstraints cs = vsep (prettyEq . go <$> S.toList cs) where
    go (_, x, y) = (x, y)

prettyEq :: (T a, T a) -> Doc ann
prettyEq (ty, ty') = pretty ty <+> "≡" <+> pretty ty'

mapMaxU :: (Int -> Int) -> TyState a -> TyState a
mapMaxU f (TyState u k c v cs) = TyState (f u) k c v cs

setMaxU :: Int -> TyState a -> TyState a
-- setMaxU i = mapMaxU (const i)
setMaxU i (TyState _ k c v cs) = TyState i k c v cs

mapCV :: (IM.IntMap (S.Set (C, a)) -> IM.IntMap (S.Set (C, a))) -> TyState a -> TyState a
mapCV f (TyState u k cvs v cs) = TyState u k (f cvs) v cs

addVarEnv :: Int -> T K -> TyState a -> TyState a
addVarEnv i ty (TyState u k cvs v cs) = TyState u k cvs (IM.insert i ty v) cs

addKindEnv :: Int -> K -> TyState a -> TyState a
addKindEnv i k (TyState u ks cvs v cs) = TyState u (IM.insert i k ks) cvs v cs

addConstraint :: Ord a => (a, T K, T K) -> TyState a -> TyState a
addConstraint c (TyState u k cvs v cs) = TyState u k cvs v (S.insert c cs)

type TyM a = StateT (TyState a) (Either (Err a))

runTyM :: Int -> TyM a b -> Either (Err a) (b, Int)
runTyM i = fmap (second maxU) . flip runStateT (TyState i IM.empty IM.empty IM.empty S.empty)

type Subst a = IM.IntMap (T a)

aT :: Subst a -> T a -> T a
aT um ty'@(TyVar _ (Name _ (Unique i) _)) =
    case IM.lookup i um of
        Just ty@TyVar{} -> aT (IM.delete i um) ty -- prevent cyclic lookups
        -- TODO: does this need a case for TyApp -> aT?
        Just ty         -> ty
        Nothing         -> ty'
aT _ ty'@TyB{} = ty'
aT um (TyApp l ty ty') = TyApp l (aT um ty) (aT um ty')
aT um (TyArr l ty ty') = TyArr l (aT um ty) (aT um ty')
aT um (TyTup l tys)    = TyTup l (aT um <$> tys)

-- | Perform substitutions before handing off to 'unifyMatch'
unifyPrep :: Subst a
          -> [(l, T a, T a)]
          -> TyM l (IM.IntMap (T a))
unifyPrep _ [] = pure mempty
unifyPrep um ((l, ty, ty'):tys) =
    let ty'' = aT um ty
        ty''' = aT um ty'
    in unifyMatch um $ (l, ty'', ty'''):tys

mguPrep :: l -> Subst a -> T a -> T a -> Either (Err l) (Subst a)
mguPrep l s t0 t1 =
    let t0' = aT s t0; t1' = aT s t1 in mgu l s t0' t1'

mgu :: l -> Subst a -> T a -> T a -> Either (Err l) (Subst a)
mgu _ s (TyB _ b) (TyB _ b') | b == b' = Right s
mgu _ s ty (TyVar _ (Name _ (Unique k) _)) = Right $ IM.insert k ty s
mgu _ s (TyVar _ (Name _ (Unique k) _)) ty = Right $ IM.insert k ty s
mgu l s (TyArr _ t0 t1) (TyArr _ t0' t1')  = do {s0 <- mguPrep l s t0 t0'; mguPrep l s0 t1 t1'}
mgu l s (TyApp _ t0 t1) (TyApp _ t0' t1')  = do {s0 <- mguPrep l s t0 t0'; mguPrep l s0 t1 t1'}
mgu l s (TyTup _ ts) (TyTup _ ts') | length ts == length ts' = zS (mguPrep l) s ts ts'
mgu l _ t t' = Left $ UF l (void t) (void t')

zS _ s [] _           = pure s
zS _ s _ []           = pure s
zS op s (x:xs) (y:ys) = do{next <- op s x y; zS op next xs ys}

unifyMatch :: Subst a -> [(l, T a, T a)] -> TyM l (IM.IntMap (T a))
unifyMatch _ [] = pure IM.empty
unifyMatch um ((_, TyB _ b, TyB _ b'):tys) | b == b' = unifyPrep um tys
unifyMatch um ((_, TyVar _ n, ty@(TyVar _ n')):tys)
    | n == n' = unifyPrep um tys
unifyMatch um ((_, ty, TyVar  _ (Name _ (Unique k) _)):tys) = IM.insert k ty <$> unifyPrep (IM.insert k ty um) tys
unifyMatch um ((_, TyVar _ (Name _ (Unique k) _), ty):tys) = IM.insert k ty <$> unifyPrep (IM.insert k ty um) tys
unifyMatch um ((l, TyApp _ ty ty', TyApp _ ty'' ty'''):tys) = unifyPrep um ((l, ty, ty'') : (l, ty', ty''') : tys)
unifyMatch um ((l, TyArr _ ty ty', TyArr _ ty'' ty'''):tys) = unifyPrep um ((l, ty, ty'') : (l, ty', ty''') : tys)
unifyMatch um ((l, TyTup _ tys, TyTup _ tys'):tyss)
    | length tys == length tys' = unifyPrep um (zip3 (repeat l) tys tys' ++ tyss)
unifyMatch _ ((l, ty, ty'):_) = throwError (UF l (void ty) (void ty'))

unify :: [(l, T a, T a)] -> TyM l (IM.IntMap (T a))
unify = unifyPrep IM.empty

unifyM :: S.Set (l, T a, T a) -> TyM l (IM.IntMap (T a))
unifyM s = {-# SCC "unifyM" #-} unify (S.toList s)

substInt :: IM.IntMap (T a) -> Int -> Maybe (T a)
substInt tys k =
    case IM.lookup k tys of
        Just ty'@TyVar{}       -> Just $ substConstraints (IM.delete k tys) ty' -- TODO: this is to prevent cyclic lookups: is it right?
        Just (TyApp l ty0 ty1) -> Just $ let tys' = IM.delete k tys in TyApp l (substConstraints tys' ty0) (substConstraints tys' ty1)
        Just (TyArr l ty0 ty1) -> Just $ let tys' = IM.delete k tys in TyArr l (substConstraints tys' ty0) (substConstraints tys' ty1)
        Just (TyTup l tysϵ)    -> Just $ let tys' = IM.delete k tys in TyTup l (substConstraints tys' <$> tysϵ)
        Just ty'               -> Just ty'
        Nothing                -> Nothing

substConstraints :: IM.IntMap (T a) -> T a -> T a
substConstraints _ ty@TyB{}                             = ty
substConstraints tys ty@(TyVar _ (Name _ (Unique k) _)) = fromMaybe ty (substInt tys k)
substConstraints tys (TyTup l tysϵ)                     = TyTup l (substConstraints tys <$> tysϵ)
substConstraints tys (TyApp l ty ty')                   =
    TyApp l (substConstraints tys ty) (substConstraints tys ty')
substConstraints tys (TyArr l ty ty')                   =
    TyArr l (substConstraints tys ty) (substConstraints tys ty')

freshName :: T.Text -> K -> TyM a (Name K)
freshName n k = do
    st <- gets maxU
    Name n (Unique $ st+1) k
        <$ modify (mapMaxU (+1))

namek :: Name K -> TyM a (Name K)
namek n =
    modify (addKindEnv (unUnique$unique n) (loc n)) $> n

higherOrder :: T.Text -> TyM a (Name K)
higherOrder t = freshName t (KArr Star Star) >>= namek

-- of kind 'Star'
dummyName :: T.Text -> TyM a (Name K)
dummyName n = freshName n Star >>= namek

addC :: Ord a => Name b -> (C, a) -> IM.IntMap (S.Set (C, a)) -> IM.IntMap (S.Set (C, a))
addC (Name _ (Unique i) _) c = IM.alter (Just . go) i where
    go Nothing   = S.singleton c
    go (Just cs) = S.insert c cs

-- | arguments assumed to have kind 'Star'
tyArr :: T K -> T K -> T K
tyArr = TyArr Star

var :: Name K -> T K
var = TyVar Star

-- assumes they have been renamed...
pushConstraint :: Ord a => a -> T K -> T K -> TyM a ()
pushConstraint l ty ty' =
    modify (addConstraint (l, ty, ty'))

isStar :: K -> TyM a ()
isStar Star = pure ()
isStar k    = throwError $ Expected k Star

liftCloneTy :: T a -> TyM b (T a)
liftCloneTy ty = do
    i <- gets maxU
    let (ty', (j, iMaps)) = cloneTy i ty
    -- FIXME: clone/propagate constraints
    ty' <$ modify (setMaxU j)

cloneTy :: Int -> T a -> (T a, (Int, IM.IntMap Unique))
cloneTy i ty = flip runState (i, IM.empty) $ cloneTyM ty
    where cloneTyM (TyVar l (Name n (Unique j) l')) = do
                st <- gets snd
                case IM.lookup j st of
                    Just k -> pure (TyVar l (Name n k l'))
                    Nothing -> do
                        k <- gets fst
                        let j' = Unique $ k+1
                        TyVar l (Name n j' l') <$ modify (\(u, s) -> (u+1, IM.insert j j' s))
          cloneTyM (TyArr l tyϵ ty')               = TyArr l <$> cloneTyM tyϵ <*> cloneTyM ty'
          cloneTyM (TyApp l tyϵ ty')               = TyApp l <$> cloneTyM tyϵ <*> cloneTyM ty'
          cloneTyM (TyTup l tys)                   = TyTup l <$> traverse cloneTyM tys
          cloneTyM tyϵ@TyB{}                       = pure tyϵ

kind :: T K -> TyM a ()
kind (TyB Star TyStr)                  = pure ()
kind (TyB Star TyInteger)              = pure ()
kind (TyB Star TyFloat)                = pure ()
kind (TyB (KArr Star Star) TyStream)   = pure ()
kind (TyB (KArr Star Star) TyOption)   = pure ()
kind (TyB Star TyBool)                 = pure ()
kind (TyB (KArr Star Star) TyVec)      = pure ()
kind (TyB Star TyUnit)                 = pure ()
kind (TyB k TyStr)                     = throwError $ Expected Star k
kind (TyB k TyInteger)                 = throwError $ Expected Star k
kind (TyB k TyFloat)                   = throwError $ Expected Star k
kind (TyB k TyUnit)                    = throwError $ Expected Star k
kind (TyB k TyBool)                    = throwError $ Expected Star k
kind (TyB k TyOption)                  = throwError $ Expected (KArr Star Star) k
kind (TyB k TyStream)                  = throwError $ Expected (KArr Star Star) k
kind (TyB k TyVec)                     = throwError $ Expected (KArr Star Star) k
kind (TyVar _ n@(Name _ (Unique i) _)) = do
    preK <- gets (IM.lookup i . kindEnv)
    case preK of
        Just{}  -> pure ()
        Nothing -> throwError $ IllScopedTyVar (void n)
kind (TyTup Star tys) =
    traverse_  isStar (fmap tLoc tys)
kind (TyTup k _) = throwError $ Expected Star k
kind (TyArr Star ty0 ty1) =
    isStar (tLoc ty0) *>
    isStar (tLoc ty1)
kind (TyArr k _ _) = throwError $ Expected Star k
kind (TyApp k1 ty0 ty1) = do
    case tLoc ty0 of
        (KArr k0 k1') | k0 == tLoc ty1 && k1' == k1 -> pure ()
                      | k0 == tLoc ty1 -> throwError $ Expected k1' k1
                      | otherwise        -> throwError $ Expected (tLoc ty1) k0
        k0                               -> throwError $ Expected (KArr Star Star) k0

checkType :: Ord a => T K -> (C, a) -> TyM a ()
checkType TyVar{} _                            = pure () -- TODO: I think this is right
checkType (TyB _ TyR) (IsSemigroup, _)         = pure ()
checkType (TyB _ TyStr) (IsSemigroup, _)       = pure ()
checkType (TyB _ TyInteger) (IsSemigroup, _)   = pure ()
checkType (TyB _ TyInteger) (IsNum, _)         = pure ()
checkType (TyB _ TyInteger) (IsOrd, _)         = pure ()
checkType (TyB _ TyInteger) (IsEq, _)          = pure ()
checkType (TyB _ TyInteger) (IsParse, _)   = pure ()
checkType (TyB _ TyFloat) (IsParse, _)     = pure ()
checkType ty (IsParse, l)                  = throwError $ Doesn'tSatisfy l (void ty) IsParse
checkType (TyB _ TyFloat) (IsSemigroup, _)     = pure ()
checkType (TyB _ TyFloat) (IsNum, _)           = pure ()
checkType (TyB _ TyFloat) (IsOrd, _)           = pure ()
checkType (TyB _ TyFloat) (IsEq, _)            = pure ()
checkType (TyB _ TyBool) (IsEq, _)             = pure ()
checkType (TyB _ TyStr) (IsEq, _)              = pure ()
checkType ty@(TyB _ TyStr) (c@IsOrd, l)        = throwError $ Doesn'tSatisfy l (void ty) c
checkType (TyTup _ tys) (IsEq, l)              = traverse_ (`checkType` (IsEq, l)) tys
checkType (TyTup _ tys) (IsOrd, l)             = traverse_ (`checkType` (IsOrd, l)) tys
checkType (TyApp _ (TyB _ TyVec) ty) (IsEq, l) = checkType ty (IsEq, l)
checkType ty@TyTup{} (c@IsNum, l)              = throwError $ Doesn'tSatisfy l (void ty) c
checkType ty@(TyB _ TyStr) (c@IsNum, l)        = throwError $ Doesn'tSatisfy l (void ty) c
checkType ty@(TyB _ TyBool) (c@IsNum, l)       = throwError $ Doesn'tSatisfy l (void ty) c
checkType ty@TyArr{} (c, l)                    = throwError $ Doesn'tSatisfy l (void ty) c
checkType (TyB _ TyVec) (Functor, _)           = pure ()
checkType (TyB _ TyStream) (Functor, _)        = pure ()
checkType (TyB _ TyOption) (Functor, _)        = pure ()
checkType (TyB _ TyStream) (Witherable, _)     = pure ()
checkType ty (c@Witherable, l)                 = throwError $ Doesn'tSatisfy l (void ty) c
checkType ty (c@Functor, l)                    = throwError $ Doesn'tSatisfy l (void ty) c
checkType (TyB _ TyVec) (Foldable, _)          = pure ()
checkType (TyB _ TyStream) (Foldable, _)       = pure ()
checkType ty (c@Foldable, l)                   = throwError $ Doesn'tSatisfy l (void ty) c
checkType (TyB _ TyStr) (IsPrintf, _)          = pure ()
checkType (TyB _ TyFloat) (IsPrintf, _)        = pure ()
checkType (TyB _ TyInteger) (IsPrintf, _)      = pure ()
checkType (TyB _ TyBool) (IsPrintf, _)         = pure ()
checkType (TyTup _ tys) (IsPrintf, l)          = traverse_ (`checkType` (IsPrintf, l)) tys
checkType ty (c@IsPrintf, l)                   = throwError $ Doesn'tSatisfy l (void ty) c
checkType ty@(TyTup _ tys) (c@(HasField i ty'), l) | length tys >= i = pushConstraint l ty' (tys !! (i-1))
                                                   | otherwise = throwError $ Doesn'tSatisfy l (void ty) c
checkType ty (c@HasField{}, l)                 = throwError $ Doesn'tSatisfy l (void ty) c

substC :: IM.IntMap (T K) -- ^ Unification result
       -> C
       -> C
substC um (HasField i ty) = HasField i (substConstraints um ty)
substC _ c                = c

checkClass :: Ord a
           => IM.IntMap (T K) -- ^ Unification result
           -> Int
           -> S.Set (C, a)
           -> TyM a ()
checkClass tys i cs = {-# SCC "checkClass" #-}
    case substInt tys i of
        Just ty -> traverse_ (checkType ty) (first (substC tys) <$> S.toList cs)
        Nothing -> pure () -- FIXME: we need to check that the var is well-kinded for constraint

lookupVar :: Name a -> TyM a (T K)
lookupVar n@(Name _ (Unique i) l) = do
    st <- gets varEnv
    case IM.lookup i st of
        Just ty -> pure ty -- liftCloneTy ty
        Nothing -> throwError $ IllScoped l n

tyOf :: Ord a => E a -> TyM a (T K)
tyOf = fmap eLoc . tyE

tyDS :: Ord a => Subst K -> D a -> TyM a (D (T K), Subst K)
tyDS s (SetFS bs) = pure (SetFS bs, s)
tyDS s FlushDecl  = pure (FlushDecl, s)
tyDS s (FunDecl n@(Name _ (Unique i) _) [] e) = do
    (e', s') <- tyES s e
    let t=eLoc e'
    modify (addVarEnv i t) $> (FunDecl (n$>t) [] e', s')
tyDS _ FunDecl{}   = error "Internal error. Should have been desugared by now."

isAmbiguous :: T K -> Bool
isAmbiguous TyVar{}          = True
isAmbiguous (TyArr _ ty ty') = isAmbiguous ty || isAmbiguous ty'
isAmbiguous (TyApp _ ty ty') = isAmbiguous ty || isAmbiguous ty'
isAmbiguous (TyTup _ tys)    = any isAmbiguous tys
isAmbiguous TyB{}            = False

checkAmb :: E (T K) -> TyM a ()
checkAmb e@(BBuiltin ty _) | isAmbiguous ty = throwError $ Ambiguous ty (void e)
checkAmb TBuiltin{} = pure () -- don't fail on ternary builtins, we don't need it anyway... better error messages
checkAmb e@(UBuiltin ty _) | isAmbiguous ty = throwError $ Ambiguous ty (void e)
checkAmb (Implicit _ e') = checkAmb e'
checkAmb (Guarded _ p e') = checkAmb p *> checkAmb e'
checkAmb (EApp _ e' e'') = checkAmb e' *> checkAmb e'' -- more precise errors, don't fail yet!
checkAmb (Tup _ es) = traverse_ checkAmb es
checkAmb e@(Arr ty _) | isAmbiguous ty = throwError $ Ambiguous ty (void e)
checkAmb e@(Var ty _) | isAmbiguous ty = throwError $ Ambiguous ty (void e)
checkAmb (Let _ bs e) = traverse_ checkAmb [e, snd bs]
checkAmb (Lam _ _ e) = checkAmb e -- I think
checkAmb _ = pure ()

tS _ s []     = pure ([], s)
tS f s (t:ts) = do{(x, next) <- f s t; first (x:) <$> tS f next ts}

tyProgram :: Ord a => Program a -> TyM a (Program (T K))
tyProgram (Program ds e) = do
    (ds', s0) <- tS tyDS mempty ds
    (e', s1) <- tyES s0 e
    toCheck <- gets (IM.toList . classVars)
    traverse_ (uncurry (checkClass s1)) toCheck
    let res = {-# SCC "substConstraints" #-} fmap (substConstraints s1) (Program ds' e')
    checkAmb (expr res) $> res

tyNumOp :: Ord a => a -> TyM a (T K)
tyNumOp l = do
    m <- dummyName "m"
    modify (mapCV (addC m (IsNum, l)))
    let m' = var m
    pure $ tyArr m' (tyArr m' m')

tySemiOp :: Ord a => a -> TyM a (T K)
tySemiOp l = do
    m <- dummyName "m"
    modify (mapCV (addC m (IsSemigroup, l)))
    let m' = var m
    pure $ tyArr m' (tyArr m' m')

tyOrd :: Ord a => a -> TyM a (T K)
tyOrd l = do
    a <- dummyName "a"
    modify (mapCV (addC a (IsOrd, l)))
    let a' = var a
    pure $ tyArr a' (tyArr a' tyB)

tyEq :: Ord a => a -> TyM a (T K)
tyEq l = do
    a <- dummyName "a"
    modify (mapCV (addC a (IsEq, l)))
    let a' = var a
    pure $ tyArr a' (tyArr a' tyB)

-- min/max
tyM :: Ord a => a -> TyM a (T K)
tyM l = do
    a <- dummyName "a"
    modify (mapCV (addC a (IsOrd, l)))
    let a' = var a
    pure $ tyArr a' (tyArr a' a')

desugar :: a
desugar = error "Should have been de-sugared in an earlier stage!"

tyE :: Ord a => E a -> TyM a (E (T K))
tyE e = do
    (e', s) <- tyES mempty e
    cvs <- gets (IM.toList . classVars)
    traverse_ (uncurry (checkClass s)) cvs
    pure (fmap (substConstraints s) e')

tyES :: Ord a => Subst K -> E a -> TyM a (E (T K), Subst K)
tyES s (BoolLit _ b)      = pure (BoolLit tyB b, s)
tyES s (IntLit _ i)       = pure (IntLit tyI i, s)
tyES s (FloatLit _ f)     = pure (FloatLit tyF f, s)
tyES s (StrLit _ str)     = pure (StrLit tyStr str, s)
tyES s (RegexLit _ rr)    = pure (RegexLit tyR rr, s)
tyES s (Column _ i)       = pure (Column (tyStream tyStr) i, s)
tyES s (IParseCol _ i)    = pure (IParseCol (tyStream tyI) i, s)
tyES s (FParseCol _ i)    = pure (FParseCol (tyStream tyF) i, s)
tyES s (Field _ i)        = pure (Field tyStr i, s)
tyES s LastField{}        = pure (LastField tyStr, s)
tyES s AllField{}         = pure (AllField tyStr, s)
tyES s AllColumn{}        = pure (AllColumn (tyStream tyStr), s)
tyES s (NBuiltin _ Ix)    = pure (NBuiltin tyI Ix, s)
tyES s (NBuiltin _ Fp)    = pure (NBuiltin tyStr Fp, s)
tyES s (NBuiltin _ Nf)    = pure (NBuiltin tyI Nf, s)
tyES s (BBuiltin l Plus)  = do {t <- tySemiOp l; pure (BBuiltin t Plus, s)}
tyES s (BBuiltin l Minus) = do {t <- tyNumOp l; pure (BBuiltin t Minus, s)}
tyES s (BBuiltin l Times) = do {t <- tyNumOp l; pure (BBuiltin t Times, s)}
tyES s (BBuiltin l Exp)   = do {t <- tyNumOp l; pure (BBuiltin t Exp, s)}
tyES s (BBuiltin l Gt)    = do {t <- tyOrd l; pure (BBuiltin t Gt, s)}
tyES s (BBuiltin l Lt)    = do {t <- tyOrd l; pure (BBuiltin t Lt, s)}
tyES s (BBuiltin l Geq)   = do {t <- tyOrd l; pure (BBuiltin t Geq, s)}
tyES s (BBuiltin l Leq)   = do {t <- tyOrd l; pure (BBuiltin t Leq, s)}
tyES s (BBuiltin l Eq)    = do {t <- tyOrd l; pure (BBuiltin t Eq, s)}
tyES s (BBuiltin l Neq)   = do {t <- tyOrd l; pure (BBuiltin t Neq, s)}
tyES s (BBuiltin l Min)   = do {t <- tyM l; pure (BBuiltin t Min, s)}
tyES s (BBuiltin l Max)   = do {t <- tyM l; pure (BBuiltin t Max, s)}
tyES s (BBuiltin _ Split) = pure (BBuiltin (tyArr tyStr (tyArr tyR (tyV tyStr))) Split, s)
tyES s (BBuiltin _ Splitc) = pure (BBuiltin (tyArr tyStr (tyArr tyStr (tyV tyStr))) Splitc, s)
tyES s (BBuiltin _ Matches) = pure (BBuiltin (tyArr tyStr (tyArr tyR tyB)) Matches, s)
tyES s (BBuiltin _ NotMatches) = pure (BBuiltin (tyArr tyStr (tyArr tyR tyB)) NotMatches, s)
tyES s (UBuiltin _ Tally) = pure (UBuiltin (tyArr tyStr tyI) Tally, s)
tyES s (BBuiltin _ Div) = pure (BBuiltin (tyArr tyF (tyArr tyF tyF)) Div, s)
tyES s (UBuiltin _ Not) = pure (UBuiltin (tyArr tyB tyB) Not, s)
tyES s (BBuiltin _ And) = pure (BBuiltin (tyArr tyB (tyArr tyB tyB)) And, s)
tyES s (BBuiltin _ Or) = pure (BBuiltin (tyArr tyB (tyArr tyB tyB)) Or, s)
tyES s (BBuiltin _ Match) = pure (BBuiltin (tyArr tyStr (tyArr tyR (tyOpt $ TyTup Star [tyI, tyI]))) Match, s)
tyES s (TBuiltin _ Substr) = pure (TBuiltin (tyArr tyStr (tyArr tyI (tyArr tyI tyStr))) Substr, s)
tyES s (UBuiltin _ IParse) = pure (UBuiltin (tyArr tyStr tyI) IParse, s)
tyES s (UBuiltin _ FParse) = pure (UBuiltin (tyArr tyStr tyF) FParse, s)
tyES s (UBuiltin _ Floor) = pure (UBuiltin (tyArr tyF tyI) Floor, s)
tyES s (UBuiltin _ Ceiling) = pure (UBuiltin (tyArr tyF tyI) Ceiling, s)
tyES s (UBuiltin _ TallyList) = do {a <- var <$> dummyName "a"; pure (UBuiltin (tyArr a tyI) TallyList, s)}
tyES s (UBuiltin l Negate) = do {a <- dummyName "a"; modify (mapCV (addC a (IsNum, l))); let a'=var a in pure (UBuiltin (tyArr a' a') Negate, s)}
tyES s (UBuiltin _ Some) = do {a <- var <$> dummyName "a"; pure (UBuiltin (tyArr a (tyOpt a)) Some, s)}
tyES s (NBuiltin _ None) = do {a <- dummyName "a"; pure (NBuiltin (tyOpt (var a)) None, s)}
tyES s (ParseCol l i) = do {a <- dummyName "a"; modify (mapCV (addC a (IsParse, l))); pure (ParseCol (tyStream (var a)) i, s)}
tyES s (UBuiltin l Parse) = do {a <- dummyName "a"; modify (mapCV (addC a (IsParse, l))); pure (UBuiltin (tyArr tyStr (var a)) Parse, s)}
tyES s (BBuiltin l Sprintf) = do {a <- dummyName "a"; modify (mapCV (addC a (IsPrintf, l))); pure (BBuiltin (tyArr tyStr (tyArr (var a) tyStr)) Sprintf, s)}
tyES s (BBuiltin l DedupOn) = do {a <- var <$> dummyName "a"; b <- dummyName "b"; modify (mapCV (addC b (IsEq, l))); let b'=var b in pure (BBuiltin (tyArr (tyArr a b') (tyArr (tyStream a) (tyStream b'))) DedupOn, s)}
tyES s (UBuiltin _ (At i)) = do {a <- var <$> dummyName "a"; pure (UBuiltin (tyArr (tyV a) a) (At i), s)}
tyES s (UBuiltin l (Select i)) = do {a <- dummyName "a"; b <- var <$> dummyName "b"; modify (mapCV (addC a (HasField i b, l))); pure (UBuiltin (tyArr (var a) b) (Select i), s)}
tyES s (UBuiltin l Dedup) = do {a <- dummyName "a"; modify (mapCV (addC a (IsEq, l))); let sA=tyStream (var a) in pure (UBuiltin (tyArr sA sA) Dedup, s)}
tyES s (UBuiltin _ Const) = do {a <- var <$> dummyName "a"; b <- var <$> dummyName "b"; pure (UBuiltin (tyArr a (tyArr b a)) Const, s)}
tyES s (UBuiltin l CatMaybes) = do {a <- dummyName "a"; f <- higherOrder "f"; modify (mapCV (addC f (Witherable, l))); let a'=var a; f'=var f in pure (UBuiltin (tyArr (hkt f' (tyOpt a')) (hkt f' a')) CatMaybes, s)}
tyES s (BBuiltin l Filter) = do {a <- dummyName "a"; f <- higherOrder "f"; modify (mapCV (addC f (Witherable, l))); let a'=var a; f'=var f; w=hkt f' a' in pure (BBuiltin (tyArr (tyArr a' tyB) (tyArr w w)) Filter, s)}
tyES s (BBuiltin l MapMaybe) = do
    a <- var <$> dummyName "a"; b <- var <$> dummyName "b"
    f <- higherOrder "f"
    modify (mapCV (addC f (Witherable, l)))
    let f'=var f
    pure (BBuiltin (tyArr (tyArr a (tyOpt b)) (tyArr (hkt f' a) (hkt f' b))) MapMaybe, s)
tyES s (BBuiltin l Map) = do
    a <- var <$> dummyName "a"; b <- var <$> dummyName "b"
    f <- higherOrder "f"
    let f'=var f
    modify (mapCV (addC f (Functor, l)))
    pure (BBuiltin (tyArr (tyArr a b) (tyArr (hkt f' a) (hkt f' b))) Map, s)
tyES s (TBuiltin l Fold) = do
    a <- var <$> dummyName "a"; b <- var <$> dummyName "b"
    f <- higherOrder "f"
    let f'=var f
    modify (mapCV (addC f (Foldable, l)))
    pure (TBuiltin (tyArr (tyArr b (tyArr a b)) (tyArr b (tyArr (hkt f' a) b))) Fold, s)
tyES s (BBuiltin l Fold1) = do
    a <- var <$> dummyName "a"
    f <- higherOrder "f"
    let f'=var f
    modify (mapCV (addC f (Foldable, l)))
    pure (BBuiltin (tyArr (tyArr a (tyArr a a)) (tyArr (hkt f' a) a)) Fold1, s)
tyES s (TBuiltin _ Captures) = pure (TBuiltin (tyArr tyStr (tyArr tyI (tyArr tyR (tyOpt tyStr)))) Captures, s)
tyES s (BBuiltin _ Prior) = do
    a <- var <$> dummyName "a"; b <- var <$> dummyName "b"
    pure (BBuiltin (tyArr (tyArr a (tyArr a b)) (tyArr (tyStream a) (tyStream b))) Prior, s)
tyES s (TBuiltin _ ZipW) = do
    a <- var <$> dummyName "a"; b <- var <$> dummyName "b"; c <- var <$> dummyName "c"
    pure (TBuiltin (tyArr (tyArr a (tyArr b c)) (tyArr (tyStream a) (tyArr (tyStream b) (tyStream c)))) ZipW, s)
tyES s (TBuiltin _ Scan) = do
    a <- var <$> dummyName "a"; b <- var <$> dummyName "b"
    pure (TBuiltin (tyArr (tyArr b (tyArr a b)) (tyArr b (tyArr (tyStream a) (tyStream b)))) Scan, s)
tyES s (TBuiltin _ Option) = do
    a <- var <$> dummyName "a"; b <- var <$> dummyName "b"
    pure (TBuiltin (tyArr b (tyArr (tyArr a b) (tyArr (tyOpt a) b))) Option, s)
tyES s (TBuiltin _ AllCaptures) = pure (TBuiltin (tyArr tyStr (tyArr tyI (tyArr tyR (tyV tyStr)))) AllCaptures, s)
tyES s (Implicit _ e) = do {(e',s') <- tyES s e; pure (Implicit (tyStream (eLoc e')) e', s')}
tyES s (Guarded l e se) = do
    (se', s0) <- tyES s se
    (e', s1) <- tyES s0 e
    s2 <- liftEither $ mguPrep l s1 tyB (eLoc e')
    pure (Guarded (tyStream (eLoc se')) e' se', s2)
tyES s (EApp l e0 e1)     = do
    a <- dummyName "a"; b <- dummyName "b"
    let a'=var a; b'=var b; e0Ty=tyArr a' b'
    (e0', s0) <- tyES s e0
    (e1', s1) <- tyES s0 e1
    s2 <- liftEither $ mguPrep l s1 (eLoc e0') e0Ty
    s3 <- liftEither $ mguPrep l s2 (eLoc e1') a'
    pure (EApp b' e0' e1', s3)
tyES s (Lam _ n@(Name _ (Unique i) _) e) = do
    a <- var <$> dummyName "a"
    modify (addVarEnv i a)
    (e', s') <- tyES s e
    pure (Lam (tyArr a (eLoc e')) (n$>a) e', s')
tyES s (Let _ (n@(Name _ (Unique i) _), eϵ) e) = do
    (eϵ', s0) <- tyES s eϵ
    let bTy=eLoc eϵ'
    modify (addVarEnv i bTy)
    (e', s1) <- tyES s0 e
    pure (Let (eLoc e') (n$>bTy, eϵ') e', s1)
tyES s (Tup _ es) = do {(es', s') <- tS tyES s es; pure (Tup (TyTup Star (fmap eLoc es')) es', s')}
tyES s (Var _ n) = do {t <- lookupVar n; pure (Var t (n$>t), s)}
tyES s (OptionVal _ (Just e)) = do {(e', s') <- tyES s e; pure (OptionVal (tyOpt (eLoc e')) (Just e'), s')}
tyES s (OptionVal _ Nothing) = do {a <- var <$> dummyName "a"; pure (OptionVal (tyOpt a) Nothing, s)}
tyES s (Arr l v) | V.null v = do
    a <- var <$> dummyName "a"
    pure (Arr (tyV a) V.empty, s)
tyES s (Cond l p e0 e1) = do
    (p', s0) <- tyES s p
    (e0', s1) <- tyES s0 e0
    (e1', s2) <- tyES s1 e1
    let t=eLoc e0'
    s3 <- liftEither $ mguPrep l s2 tyB (eLoc p')
    s4 <- liftEither $ mguPrep l s3 t (eLoc e1')
    pure (Cond t p' e0' e1', s4)

tyE0 :: Ord a => E a -> TyM a (E (T K))
tyE0 (BoolLit _ b)           = pure $ BoolLit tyB b
tyE0 (IntLit _ i)            = pure $ IntLit tyI i
tyE0 (FloatLit _ f)          = pure $ FloatLit tyF f
tyE0 (StrLit _ str)          = pure $ StrLit tyStr str
tyE0 (RegexLit _ rr)         = pure $ RegexLit tyR rr
tyE0 (Column _ i)            = pure $ Column (tyStream tyStr) i
tyE0 (IParseCol _ i)         = pure $ IParseCol (tyStream tyI) i
tyE0 (FParseCol _ i)         = pure $ FParseCol (tyStream tyF) i
tyE0 (Field _ i)             = pure $ Field tyStr i
tyE0 (LastField _)           = pure $ LastField tyStr
tyE0 AllField{}              = pure $ AllField tyStr
tyE0 AllColumn{}             = pure $ AllColumn (tyStream tyStr)
tyE0 (NBuiltin _ Ix)         = pure $ NBuiltin tyI Ix
tyE0 (NBuiltin _ Fp)         = pure $ NBuiltin tyStr Fp
tyE0 (NBuiltin _ Nf)         = pure $ NBuiltin tyI Nf
tyE0 (BBuiltin l Plus)       = BBuiltin <$> tySemiOp l <*> pure Plus
tyE0 (BBuiltin l Minus)      = BBuiltin <$> tyNumOp l <*> pure Minus
tyE0 (BBuiltin l Times)      = BBuiltin <$> tyNumOp l <*> pure Times
tyE0 (BBuiltin l Exp)        = BBuiltin <$> tyNumOp l <*> pure Exp
tyE0 (BBuiltin l Gt)         = BBuiltin <$> tyOrd l <*> pure Gt
tyE0 (BBuiltin l Lt)         = BBuiltin <$> tyOrd l <*> pure Lt
tyE0 (BBuiltin l Geq)        = BBuiltin <$> tyOrd l <*> pure Geq
tyE0 (BBuiltin l Leq)        = BBuiltin <$> tyOrd l <*> pure Leq
tyE0 (BBuiltin l Eq)         = BBuiltin <$> tyEq l <*> pure Eq
tyE0 (BBuiltin l Neq)        = BBuiltin <$> tyEq l <*> pure Neq
tyE0 (BBuiltin l Min)        = BBuiltin <$> tyM l <*> pure Min
tyE0 (BBuiltin l Max)        = BBuiltin <$> tyM l <*> pure Max
tyE0 (BBuiltin _ Split)      = pure $ BBuiltin (tyArr tyStr (tyArr tyR (tyV tyStr))) Split
tyE0 (BBuiltin _ Splitc)     = pure $ BBuiltin (tyArr tyStr (tyArr tyStr (tyV tyStr))) Splitc
tyE0 (BBuiltin _ Matches)    = pure $ BBuiltin (tyArr tyStr (tyArr tyR tyB)) Matches
tyE0 (BBuiltin _ NotMatches) = pure $ BBuiltin (tyArr tyStr (tyArr tyR tyB)) NotMatches
tyE0 (UBuiltin _ Tally)      = pure $ UBuiltin (tyArr tyStr tyI) Tally
tyE0 (BBuiltin _ Div)        = pure $ BBuiltin (tyArr tyF (tyArr tyF tyF)) Div
tyE0 (UBuiltin _ Not)        = pure $ UBuiltin (tyArr tyB tyB) Not
tyE0 (BBuiltin _ And)        = pure $ BBuiltin (tyArr tyB (tyArr tyB tyB)) And
tyE0 (BBuiltin _ Or)         = pure $ BBuiltin (tyArr tyB (tyArr tyB tyB)) Or
tyE0 (BBuiltin _ Match)      = pure $ BBuiltin (tyArr tyStr (tyArr tyR (tyOpt $ TyTup Star [tyI, tyI]))) Match
tyE0 (TBuiltin _ Substr)     = pure $ TBuiltin (tyArr tyStr (tyArr tyI (tyArr tyI tyStr))) Substr
tyE0 (UBuiltin _ IParse)     = pure $ UBuiltin (tyArr tyStr tyI) IParse
tyE0 (UBuiltin _ FParse)     = pure $ UBuiltin (tyArr tyStr tyF) FParse
tyE0 (UBuiltin _ Floor)      = pure $ UBuiltin (tyArr tyF tyI) Floor
tyE0 (UBuiltin _ Ceiling)    = pure $ UBuiltin (tyArr tyF tyI) Ceiling
tyE0 (UBuiltin _ TallyList) = do
    a <- dummyName "a"
    let a' = var a
    pure $ UBuiltin (tyArr a' tyI) TallyList
tyE0 (UBuiltin l Negate) = do
    a <- dummyName "a"
    modify (mapCV (addC a (IsNum, l)))
    let a' = var a
    pure $ UBuiltin (tyArr a' a') Negate
tyE0 (UBuiltin _ Some) = do
    a <- dummyName "a"
    let a' = var a
    pure $ UBuiltin (tyArr a' (tyOpt a')) Some
tyE0 (NBuiltin _ None) = do
    a <- dummyName "a"
    pure $ NBuiltin (tyOpt $ var a) None
tyE0 (ParseCol l i) = do
    a <- dummyName "a"
    let a' = var a
    modify (mapCV (addC a (IsParse, l)))
    pure $ ParseCol (tyStream a') i
tyE0 (UBuiltin l Parse) = do
    a <- dummyName "a"
    let a' = var a
    modify (mapCV (addC a (IsParse, l)))
    pure $ UBuiltin (tyArr tyStr a') Parse
tyE0 (BBuiltin l Sprintf) = do
    a <- dummyName "a"
    let a' = var a
    modify (mapCV (addC a (IsPrintf, l)))
    pure $ BBuiltin (tyArr tyStr (tyArr a' tyStr)) Sprintf
tyE0 (BBuiltin l DedupOn) = do
    a <- dummyName "a"
    b <- dummyName "b"
    let a' = var a
        b' = var b
    modify (mapCV (addC b (IsEq, l)))
    pure $ BBuiltin (tyArr (tyArr a' b') (tyArr (tyStream a') (tyStream a'))) DedupOn
tyE0 (UBuiltin _ (At i)) = do
    a <- dummyName "a"
    let a' = var a
        tyVϵ = tyV a'
    pure $ UBuiltin (tyArr tyVϵ a') (At i)
tyE0 (UBuiltin l (Select i)) = do
    a <- dummyName "a"
    b <- dummyName "b"
    let a' = var a
        b' = var b
    modify (mapCV (addC a (HasField i b', l)))
    pure $ UBuiltin (tyArr a' b') (Select i)
tyE0 (UBuiltin l Dedup) = do
    a <- dummyName "a"
    let a' = var a
        fTy = tyArr (tyStream a') (tyStream a')
    modify (mapCV (addC a (IsEq, l)))
    pure $ UBuiltin fTy Dedup
tyE0 (UBuiltin _ Const) = do
    a <- dummyName "a"
    b <- dummyName "b"
    let a' = var a
        b' = var b
        fTy = tyArr a' (tyArr b' a')
    pure $ UBuiltin fTy Const
tyE0 (UBuiltin l CatMaybes) = do
    a <- dummyName "a"
    f <- higherOrder "f"
    let a' = var a
        f' = var f
        fTy = tyArr (hkt f' $ tyOpt a') (hkt f' a')
    modify (mapCV (addC f (Witherable, l)))
    pure $ UBuiltin fTy CatMaybes
tyE0 (BBuiltin l Filter) = do
    a <- dummyName "a"
    f <- higherOrder "f"
    let a' = var a
        f' = var f
        fTy = tyArr (tyArr a' tyB) (tyArr (hkt f' a') (hkt f' a'))
    modify (mapCV (addC f (Witherable , l)))
    pure $ BBuiltin fTy Filter
tyE0 (BBuiltin l MapMaybe) = do
    a <- dummyName "a"
    b <- dummyName "b"
    f <- higherOrder "f"
    let a' = var a
        b' = var b
        f' = var f
        fTy = tyArr (tyArr a' (tyOpt b')) (tyArr (hkt f' a') (hkt f' b'))
    modify (mapCV (addC f (Witherable, l)))
    pure $ BBuiltin fTy MapMaybe
tyE0 (BBuiltin l Map) = do
    a <- dummyName "a"
    b <- dummyName "b"
    f <- higherOrder "f"
    let a' = var a
        b' = var b
        f' = var f
        fTy = tyArr (tyArr a' b') (tyArr (hkt f' a') (hkt f' b'))
    modify (mapCV (addC f (Functor, l)))
    pure $ BBuiltin fTy Map
tyE0 (TBuiltin l Fold) = do
    b <- dummyName "b"
    a <- dummyName "a"
    f <- higherOrder "f"
    let b' = var b
        a' = var a
        f' = var f
        fTy = tyArr (tyArr b' (tyArr a' b')) (tyArr b' (tyArr (hkt f' a') b'))
    modify (mapCV (addC f (Foldable, l)))
    pure $ TBuiltin fTy Fold
tyE0 (BBuiltin l Fold1) = do
    a <- dummyName "a"
    f <- higherOrder "f"
    let a' = var a
        f' = var f
        fTy = tyArr (tyArr a' (tyArr a' a')) (tyArr (hkt f' a') a')
    modify (mapCV (addC f (Foldable, l)))
    pure $ BBuiltin fTy Fold1
tyE0 (TBuiltin _ Captures) =
    pure $ TBuiltin (tyArr tyStr (tyArr tyI (tyArr tyR (tyOpt tyStr)))) Captures
-- (a -> a -> a) -> Stream a -> Stream a
tyE0 (BBuiltin _ Prior) = do
    a <- dummyName "a"
    b <- dummyName "b"
    let a' = var a
        b' = var b
        fTy = tyArr (tyArr a' (tyArr a' b')) (tyArr (tyStream a') (tyStream b'))
    pure $ BBuiltin fTy Prior
-- (a -> b -> c) -> Stream a -> Stream b -> Stream c
tyE0 (TBuiltin _ ZipW) = do
    a <- dummyName "a"
    b <- dummyName "b"
    c <- dummyName "c"
    let a' = var a
        b' = var b
        c' = var c
        fTy = tyArr (tyArr a' (tyArr b' c')) (tyArr (tyStream a') (tyArr (tyStream b') (tyStream c')))
    pure $ TBuiltin fTy ZipW
-- (b -> a -> b) -> b -> Stream a -> Stream b
tyE0 (TBuiltin _ Scan) = do
    b <- dummyName "b"
    a <- dummyName "a"
    let b' = var b
        a' = var a
        fTy = tyArr (tyArr b' (tyArr a' b')) (tyArr b' (tyArr (tyStream a') (tyStream b')))
    pure $ TBuiltin fTy Scan
tyE0 (TBuiltin _ Option) = do
    b <- dummyName "b"
    a <- dummyName "a"
    let b' = var b
        a' = var a
        fTy = tyArr b' (tyArr (tyArr a' b') (tyArr (tyOpt a') b'))
    pure $ TBuiltin fTy Option
tyE0 (TBuiltin _ AllCaptures) =
    pure $ TBuiltin (tyArr tyStr (tyArr tyI (tyArr tyR (tyV tyStr)))) AllCaptures
tyE0 (Implicit _ e) = do
    e' <- tyE0 e
    pure $ Implicit (tyStream (eLoc e')) e'
tyE0 (Guarded l e streamE) = do
    streamE' <- tyE0 streamE
    e' <- tyE0 e
    pushConstraint l tyB (eLoc e')
    pure $ Guarded (tyStream (eLoc streamE')) e' streamE'
tyE0 (EApp _ e0 e1) = do
    e0' <- tyE0 e0
    e1' <- tyE0 e1
    a <- dummyName "a"
    b <- dummyName "b"
    let a' = var a
        b' = var b
        fTy = tyArr a' b'
    pushConstraint (eLoc e0) fTy (eLoc e0')
    pushConstraint (eLoc e1) a' (eLoc e1')
    pure $ EApp b' e0' e1'
tyE0 (Lam _ n@(Name _ (Unique i) _) e) = do
    a <- dummyName "a"
    let a' = var a
    modify (addVarEnv i a')
    e' <- tyE0 e
    pure $ Lam (tyArr a' (eLoc e')) (n $> a') e'
tyE0 (Let _ (n@(Name _ (Unique i) _), eϵ) e) = do
    eϵ' <- tyE0 eϵ
    let bTy = eLoc eϵ'
    modify (addVarEnv i bTy)
    e' <- tyE0 e
    pure $ Let (eLoc e') (n $> bTy, eϵ') e'
tyE0 (Tup _ es) = do
    es' <- traverse tyE0 es
    pure $ Tup (TyTup Star (eLoc <$> es')) es'
tyE0 (Var _ n) = do
    ty <- lookupVar n
    pure (Var ty (n $> ty))
tyE0 Dfn{} = desugar
tyE0 (ResVar _ X) = desugar
tyE0 (ResVar _ Y) = desugar
tyE0 RegexCompiled{} = error "Regex should not be compiled at this stage."
tyE0 Paren{} = desugar
tyE0 (OptionVal _ (Just e)) = do
    e' <- tyE0 e
    pure $ OptionVal (tyOpt $ eLoc e') (Just e')
tyE0 (OptionVal _ Nothing) = do
    a <- dummyName "a"
    let a' = var a
    pure $ OptionVal (tyOpt a') Nothing
tyE0 (Arr l v) | V.null v = do
    a <- dummyName "a"
    let a' = var a
    pure $ Arr (tyV a') V.empty
               | otherwise = do
    v' <- traverse tyE0 v
    let x = V.head v'
    V.priorM_ (\y y' -> pushConstraint l (eLoc y) (eLoc y')) v'
    pure $ Arr (eLoc x) v'
tyE0 (Anchor l es) = do
    es' <- forM es $ \e -> do
        e' <- tyE0 e
        a <- dummyName "a"
        let a' = var a
        pushConstraint l (tyStream a') (eLoc e') $> e'
    pure $ Anchor (TyB Star TyUnit) es'
tyE0 (Cond l p e0 e1) = do
    p' <- tyE0 p
    e0' <- tyE0 e0
    e1' <- tyE0 e1
    let ty0 = eLoc e0'
    pushConstraint l tyB (eLoc p')
    pushConstraint (eLoc e0) ty0 (eLoc e1')
    pure $ Cond ty0 p' e0' e1'
