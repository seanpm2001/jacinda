module Jacinda.Backend.T ( LineCtx, run, eB ) where

import           A
import           A.I
import           C
import           Control.Exception                 (Exception, throw)
import           Control.Monad                     (zipWithM, (<=<))
import           Control.Monad.Trans.State.Strict  (evalState, runState, state)
import           Data.Bifunctor                    (first, second)
import qualified Data.ByteString                   as BS
import           Data.ByteString.Builder           (hPutBuilder)
import           Data.ByteString.Builder.RealFloat (doubleDec)
import           Data.Foldable                     (fold, traverse_)
import           Data.Function                     ((&))
import qualified Data.IntMap.Strict                as IM
import qualified Data.IntSet                       as IS
import           Data.List                         (foldl', scanl')
import qualified Data.Map                          as M
import           Data.Maybe                        (fromMaybe)
import qualified Data.Set                          as S
import qualified Data.Text                         as T
import qualified Data.Vector                       as V
import           Data.Vector.Ext                   (scanlM')
import           Data.Word                         (Word8)
import           Jacinda.Backend.Const
import           Jacinda.Backend.Printf
import           Jacinda.Regex
import           Nm
import           NumParse
import           Prettyprinter                     (hardline, pretty)
import           Prettyprinter.Render.Text         (hPutDoc)
import           Regex.Rure                        (RureMatch (RureMatch), RurePtr)
import           System.IO                         (Handle, hFlush)
import           Ty.Const
import           U

infixl 4 <$!>

data EvalErr = EmptyFold
             | IndexOutOfBounds Int
             | NoSuchField Int BS.ByteString
             | InternalCoercionError (E T) TB
             | ExpectedTup (E T) | ExpectedRec (E T)
             | InternalTmp Tmp
             | InternalNm (Nm T)
             | InternalArityOrEta Int (E T)
             | InternalUnexpectedStream (E T)
             | StreamFunc (E T)
             deriving (Show)

instance Exception EvalErr where

data StreamError = NakedField deriving (Show)

instance Exception StreamError where

type Env = IM.IntMap (Maybe (E T)); type I=Int
data Σ = Σ !I !Env (IM.IntMap (S.Set BS.ByteString)) (IM.IntMap IS.IntSet) (IM.IntMap (S.Set Double)) IS.IntSet
type Tmp = Int
type Β = IM.IntMap (E T)

mE :: (Env -> Env) -> Σ -> Σ
mE f (Σ i e d di df b) = Σ i (f e) d di df b
gE (Σ _ e _ _ _ _) = e

at :: V.Vector a -> Int -> a
v `at` ix = case v V.!? (ix-1) of {Just x -> x; Nothing -> throw $ IndexOutOfBounds ix}

fieldOf :: V.Vector BS.ByteString -> BS.ByteString -> Int -> BS.ByteString
fieldOf fs b n = case fs V.!? (n-1) of {Just x -> x; Nothing -> throw $ NoSuchField n b}

parseAsEInt :: BS.ByteString -> E T
parseAsEInt = mkI.readDigits

parseAsF :: BS.ByteString -> E T
parseAsF = mkF.readFloat

(!) :: Env -> Tmp -> Maybe (E T)
(!) m r = IM.findWithDefault (throw$InternalTmp r) r m

foldSeq x = foldr seq () x `seq` x

nI :: UM Int
nI = state (\i -> (i, i+1))

nN :: T -> UM (Nm T)
nN t = do {u <- nI; pure (Nm "fold_hole" (U u) t)}

pSF :: Handle -> Bool -> (Maybe Tmp, [Tmp]) -> [Env] -> IO ()
pSF h flush (Just t, tt) [e] = do
    traverse_ (traverse_ (pS h flush).(e !)) tt
    traverse_ (pS h flush) (e!t)
pSF h flush c@(_, tt) (e:es) = do
    traverse_ (traverse_ (pS h flush)) [e!t|t <- tt]
    pSF h flush c es
pSF _ _ _ [] = pure ()

uStream = flip evalState

run :: Handle -> Bool -> Int -> E T -> [LineCtx] -> IO ()
run _ _ _ e _ | ty@TyArr{} <- eLoc e = error ("Found function type: " ++ show ty)
run h flush j e ctxs | TyB TyUnit <- eLoc e =  (\(s, f, env) -> pSF h flush (s,f) env) $ uStream j $ do
    (res, tt, iEnv, μ) <- unit e
    u <- nI
    let outs=μ<$>ctxs; es'=scanl' (&) (Σ u iEnv IM.empty IM.empty IM.empty IS.empty) outs
    pure (res, tt, gE<$>es')
run h flush j e ctxs | TyB TyStream:$_ <- eLoc e = traverse_ (traverse_ (pS h flush)).uStream j $ do
    t <- nI
    (iEnv, μ) <- ctx e t
    u <- nI
    let outs=μ<$>ctxs; es={-# SCC "scanMain" #-} scanl' (&) (Σ u iEnv IM.empty IM.empty IM.empty IS.empty) outs
    pure ((! t).gE<$>es)
run h _ j e ctxs = pDocLn h $ uStream j $ do
    (iEnv, g, e0) <- collect e
    u <- nI
    let updates=g<$>ctxs
        finEnv=foldl' (&) (Σ u iEnv IM.empty IM.empty IM.empty IS.empty) updates
    e0@>(fromMaybe (throw EmptyFold)<$>gE finEnv)

unit :: E T -> UM (Maybe Tmp, [Tmp], Env, LineCtx -> Σ -> Σ)
unit (Anchor _ es) = do
    tt <- traverse (\_ -> nI) es
    (iEnvs, μs) <- unzip <$> zipWithM ctx es tt
    pure (Nothing, tt, fold iEnvs, ts μs)
unit (EApp _ (EApp _ (BB _ Report) es) e) = do
    t <- nI; r <- nI
    (iEnv, μ) <- ctx es t
    (rEnv, g) <- φ e r
    pure (Just r, [t], iEnv<>rEnv, μ@.g)
unit e = error ("Internal error. '" ++ show e ++ "' assigned unit type?")

pS h p e = if p then pDocLn h e *> hFlush h else pDocLn h e

pDocLn :: Handle -> E T -> IO ()
pDocLn h (Lit _ (FLit f)) = hPutBuilder h (doubleDec f <> "\n")
pDocLn h e                = hPutDoc h (pretty e <> hardline)

collect :: E T -> UM (Env, LineCtx -> Σ -> Σ, E T)
collect e@(EApp ty (EApp _ (EApp _ (TB _ Fold) _) _) _) = do
    v <- nN ty
    (iEnv, g) <- φ e (unU$unique v)
    pure (iEnv, g, F v)
collect e@(EApp ty (EApp _ (BB _ Fold1) _) _) = do
    v <- nN ty
    (iEnv, g) <- φ e (unU$unique v)
    pure (iEnv, g, F v)
collect (Tup ty es) = do
    (seedEnvs, updates, es') <- unzip3 <$> traverse collect es
    pure (fold seedEnvs, ts updates, Tup ty es')
collect (Rec ty rs) = do
    (seedEnvs, updates, es') <- unzip3 <$> traverse collect es
    pure (fold seedEnvs, ts updates, Rec ty (zip ns es'))
  where
    (ns,es)=unzip rs
collect (EApp ty0 (EApp ty1 op@BB{} e0) e1) = do
    (env1, f1, e1') <- collect e1
    (env0, f0, e0') <- collect e0
    pure (env0<>env1, f1@.f0, EApp ty0 (EApp ty1 op e0') e1')
collect (EApp ty0 (EApp ty1 (EApp ty2 op@TB{} e0) e1) e2) = do
    (env2, f2, e2') <- collect e2
    (env1, f1, e1') <- collect e1
    (env0, f0, e0') <- collect e0
    pure (env0<>env1<>env2, f2@.f1@.f0, EApp ty0 (EApp ty1 (EApp ty2 op e0') e1') e2')
collect (EApp ty f@UB{} e) = do
    (env, fϵ, eϵ) <- collect e
    pure (env, fϵ, EApp ty f eϵ)
collect e@Lit{} = pure (IM.empty, const id, e)
collect (Cond t p e0 e1) = do
    (envp, pg, p') <- collect p
    (env0, f0, e0') <- collect e0
    (env1, f1, e1') <- collect e1
    pure (envp<>env0<>env1, pg@.f0@.f1, Cond t p' e0' e1')
collect (Lam t n e) = do
    (env,f,e') <- collect e
    pure (env,f,Lam t n e')
collect (OptionVal t (Just e)) = do
    (env, f, e') <- collect e
    pure (env, f, OptionVal t (Just e'))
collect (OptionVal t Nothing) = pure (mempty, const id, OptionVal t Nothing)
collect e@Column{} = throw $ InternalUnexpectedStream e
collect e@IParseCol{} = throw $ InternalUnexpectedStream e
collect e@FParseCol{} = throw $ InternalUnexpectedStream e
collect e@ParseCol{} = throw $ InternalUnexpectedStream e
collect e@AllColumn{} = throw $ InternalUnexpectedStream e
collect e@ParseAllCol{} = throw $ InternalUnexpectedStream e
collect e@IParseAllCol{} = throw $ InternalUnexpectedStream e
collect e@FParseAllCol{} = throw $ InternalUnexpectedStream e
collect e@Guarded{} = throw $ InternalUnexpectedStream e
collect e@Implicit{} = throw $ InternalUnexpectedStream e
collect Field{} = throw NakedField
collect LastField{} = throw NakedField
collect AllField{} = throw NakedField
collect FieldList{} = throw NakedField

f @. g = \l -> f l.g l

ts :: [LineCtx -> Σ -> Σ] -> LineCtx -> Σ -> Σ
ts = foldl' (@.) (\_ -> id)

φ :: E T -> Tmp -> UM (Env, LineCtx -> Σ -> Σ)
φ (EApp _ (EApp _ (EApp _ (TB _ Fold) op) seed) xs) tgt = do
    t <- nI
    seed' <- seed @> mempty
    let iEnv=IM.singleton tgt (Just$!seed')
    (env, f) <- ctx xs t
    let g=wF op t tgt
    pure (env<>iEnv, (g.).f)
φ (EApp _ (EApp _ (BB _ Fold1) op) xs) tgt = do
    let iEnv=IM.singleton tgt Nothing
    t <- nI
    (env, f) <- ctx xs t
    let g=wF op t tgt
    pure (env<>iEnv, (g.).f)

{-# SCC κ #-}
κ :: E T -> LineCtx -> E T
κ AllField{} ~(b, _, _)   = mkStr b
κ (Field _ i) ~(_, bs, _) = mkStr $ bs `at` i
κ LastField{} ~(_, bs, _) = mkStr $ V.last bs
κ FieldList{} ~(_, bs, _) = vS bs
κ (EApp ty e0 e1) line    = EApp ty (e0 `κ` line) (e1 `κ` line)
κ (NB _ Ix) ~(_, _, fp)   = mkI fp
κ (NB _ Nf) ~(_, bs, _)   = mkI$!fromIntegral (length bs)
κ e@BB{} _                = e
κ e@UB{} _                = e
κ e@TB{} _                = e
κ e@NB{} _                = e
κ e@Lit{} _               = e
κ e@RC{} _                = e
κ e@Var{} _               = e
κ e@F{} _                 = e
κ (Lam t n e) line        = Lam t n (κ e line)
κ (Tup ty es) line        = Tup ty ((`κ` line)<$>es)
κ (Arr ty es) line        = Arr ty ((`κ` line)<$>es)
κ (Rec ty es) line        = Rec ty (second (`κ` line)<$>es)
κ (OptionVal t e) line    = OptionVal t ((`κ` line)<$!>e)
κ (Cond ty p e0 e1) line  = Cond ty (p `κ` line) (e0 `κ` line) (e1 `κ` line)
κ e@ParseCol{} _          = badctx e
κ e@IParseCol{} _         = badctx e
κ e@FParseCol{} _         = badctx e
κ e@Column{} _            = badctx e
κ e@AllColumn{} _         = badctx e
κ e@Guarded{} _           = badctx e
κ e@Implicit{} _          = badctx e
κ e@IParseAllCol{} _      = badctx e
κ e@FParseAllCol{} _      = badctx e
κ e@Anchor{} _            = badctx e
κ e@ParseAllCol{} _       = badctx e
κ Dfn{} _                 = desugar
κ ResVar{} _              = desugar
κ Paren{} _               = desugar
κ RwB{} _                 = desugar
κ RwT{} _                 = desugar

ni t=IM.singleton t Nothing
na=IM.alter go where go Nothing = Just Nothing; go x@Just{} = x

ctx :: E T -> Tmp -> UM (Env, LineCtx -> Σ -> Σ)
ctx AllColumn{} res                                      = pure (ni res, \ ~(b, _, _) -> mE$res~!mkStr b)
ctx (ParseAllCol (_:$TyB TyI)) res                       = pure (ni res, \ ~(b, _, _) -> mE$res~!parseAsEInt b)
ctx (ParseAllCol (_:$TyB TyFloat)) res                   = pure (ni res, \ ~(b, _, _) -> mE$res~!parseAsF b)
ctx FParseAllCol{} res                                   = pure (ni res, \ ~(b, _, _) -> mE$res~!parseAsF b)
ctx IParseAllCol{} res                                   = pure (ni res, \ ~(b, _, _) -> mE$res~!parseAsEInt b)
ctx (Column _ i) res                                     = pure (ni res, \ ~(b, bs, _) -> mE$res~!mkStr (fieldOf bs b i))
ctx (FParseCol _ i) res                                  = pure (ni res, \ ~(b, bs, _) -> mE$res~!parseAsF (fieldOf bs b i))
ctx (IParseCol _ i) res                                  = pure (ni res, \ ~(b, bs, _) -> mE$res~!parseAsEInt (fieldOf bs b i))
ctx (ParseCol (_:$TyB TyFloat) i) res                    = pure (ni res, \ ~(b, bs, _) -> mE$res~!parseAsF (fieldOf bs b i))
ctx (ParseCol (_:$TyB TyI) i) res                        = pure (ni res, \ ~(b, bs, _) -> mE$res~!parseAsEInt (fieldOf bs b i))
ctx (EApp _ (EApp _ (BB _ Map) f) xs) o                  = do {t <- nI; (env, sb) <- ctx xs t; pure (na o env, \l->wM f t o.sb l)}
ctx (EApp _ (EApp _ (BB _ MapMaybe) f) xs) o             = do {t <- nI; (env, sb) <- ctx xs t; pure (na o env, \l->wMM f t o.sb l)}
ctx (EApp _ (UB _ CatMaybes) xs) o                       = do {t <- nI; (env, sb) <- ctx xs t; pure (na o env, \l->wCM t o.sb l)}
ctx (EApp _ (EApp _ (BB _ Filter) p) xs) o               = do {t <- nI; (env, sb) <- ctx xs t; pure (na o env, \l->wP p t o.sb l)}
ctx (Guarded _ p e) o                                    = pure (ni o, wG (p, e) o)
ctx (Implicit _ e) o                                     = pure (ni o, wI e o)
ctx (EApp _ (EApp _ (EApp _ (TB _ Scan) op) seed) xs) o  = do {t <- nI; (env, sb) <- ctx xs t; seed' <- seed@>mempty; pure (env&o~!seed', \l->wF op t o.sb l)}
ctx (EApp _ (EApp _ (EApp _ (TB _ ZipW) op) xs) ys) o    = do {t0 <- nI; t1 <- nI; (env0, sb0) <- ctx xs t0; (env1, sb1) <- ctx ys t1; pure (na o (env0<>env1), \l->wZ op t0 t1 o.sb0 l.sb1 l)}
ctx (EApp _ (EApp _ (BB _ Prior) op) xs) o               = do {t <- nI; (env, sb) <- ctx xs t; pt <- nI; pure (na o (pt\~env), \l -> wΠ op pt t o.sb l)}
ctx (EApp (_:$TyB ty) (UB _ Dedup) xs) o                 = do {k <- nI; t <- nI; (env, sb) <- ctx xs t; pure (na o env, \l->wD ty k t o.sb l)}
ctx (EApp _ (EApp _ (BB _ DedupOn) f) xs) o              = do {k <- nI; t <- nI; (env, sb) <- ctx xs t; pure (na o env, \l->wDOp f k t o.sb l)}
ctx (EApp _ (EApp _ (EApp _ (TB _ Bookend) e0) e1) xs) o = do {k <- nI; t <- nI; (env, sb) <- ctx xs t; r0 <- e0@>mempty; r1<- e1@>mempty; pure (na o env, \l->wB (r0,r1) k t o.sb l)}
ctx e _ | TyB TyStream:$_ <- eLoc e = error ("?? uh-oh. " ++ show e)
        | otherwise = error "Internal error. ctx expects a stream."

type LineCtx = (BS.ByteString, V.Vector BS.ByteString, Integer) -- line number

asS :: E T -> BS.ByteString
asS (Lit _ (StrLit s)) = s; asS e = throw (InternalCoercionError e TyStr)

asI :: E T -> Integer
asI (Lit _ (ILit i)) = i; asI e = throw (InternalCoercionError e TyI)

asF :: E T -> Double
asF (Lit _ (FLit x)) = x; asF e = throw (InternalCoercionError e TyFloat)

asR :: E T -> RurePtr
asR (RC r) = r; asR e = throw (InternalCoercionError e TyR)

asM :: E T -> Maybe (E T)
asM (OptionVal _ e) = e; asM e = throw (InternalCoercionError e TyOption)

asB :: E T -> Bool
asB (Lit _ (BLit b)) = b; asB e = throw (InternalCoercionError e TyBool)

asV :: E T -> V.Vector (E T)
asV (Arr _ v) = v; asV e = throw (InternalCoercionError e TyVec)

asT :: E T -> [E T]
asT (Tup _ es) = es; asT e = throw (ExpectedTup e)

asRec :: E T -> M.Map T.Text (E T)
asRec (Rec _ rs) = M.fromList (first name<$>rs); asRec e = throw (ExpectedRec e)

vS :: V.Vector BS.ByteString -> E T
vS = Arr (tyV tyStr).fmap mkStr

the :: BS.ByteString -> Word8
the bs = case BS.uncons bs of
    Nothing                -> error "Empty splitc char!"
    Just (c,b) | BS.null b -> c
    Just _                 -> error "Splitc takes only one char!"

rureMatchTup :: RureMatch -> E T
rureMatchTup (RureMatch s e) = Tup (TyTup [tyI,tyI]) (mkI.fromIntegral<$>[s,e])

asTup :: Maybe RureMatch -> E T
asTup Nothing  = OptionVal undefined Nothing
asTup (Just m) = OptionVal undefined (Just (rureMatchTup m))

{-# SCC (!>) #-}
(!>) :: Β -> Nm T -> E T
(!>) m n = IM.findWithDefault (throw$InternalNm n) (unU$unique n) m

a2e :: Β -> E T -> E T -> E T -> UM (E T)
a2e b op e0 e1 = (@>b) =<< a2 op e0 e1

a1e :: Β -> E T -> E T -> UM (E T)
a1e b f x = (@>b) =<< a1 f x

eB :: Int ->E T -> E T
eB j = flip evalState j.((@>mempty) <=< lβ)

a1 :: E T -> E T -> UM (E T)
a1 f x | TyArr _ cod <- eLoc f = lβ (EApp cod f x)

a2 :: E T -> E T -> E T -> UM (E T)
a2 op x0 x1 | TyArr _ t@(TyArr _ t') <- eLoc op = lβ (EApp t' (EApp t op x0) x1)

num :: Num a => BBin -> Maybe (a -> a -> a)
num Plus = Just (+); num Minus = Just (-); num Times = Just (*); num _ = Nothing

binRel :: Ord a => BBin -> Maybe (a -> a -> Bool)
binRel Lt = Just (<); binRel Gt = Just (>); binRel Eq = Just (==)
binRel Neq = Just (/=); binRel Geq = Just (>=); binRel Leq = Just (<=)
binRel _   = Nothing

($@) :: E T -> Int -> (E T, Int)
e $@ j = e@!(j,mempty)

(@!) :: E T -> (Int, Β) -> (E T, Int)
(@!) e (j,ϵ) = runState (e@>ϵ) j

{-# SCC (@>) #-}
(@>) :: E T -> Β -> UM (E T)
e@Lit{} @> _     = pure e
e@RC{} @> _      = pure e
(F n) @> b       = pure $ b!>n
e@(Var _ n) @> b = pure $ case IM.lookup (unU$unique n) b of {Just y -> y; Nothing -> e}
(EApp _ (EApp _ (BB (TyArr (TyB TyI) _) Max) x0) x1) @> b = do
    x0' <- asI<$>(x0@>b); x1' <- asI<$>(x1@>b)
    pure $ mkI (max x0' x1')
(EApp _ (EApp _ (BB (TyArr (TyB TyI) _) Min) x0) x1) @> b = do
    x0' <- asI <$> (x0@>b); x1' <- asI <$> (x1@>b)
    pure $ mkI (min x0' x1')
(EApp _ (EApp _ (BB (TyArr (TyB TyFloat) _) Max) x0) x1) @> b = do
    x0' <- asF<$>(x0@>b); x1' <- asF<$>(x1@>b)
    pure $ mkF (max x0' x1')
(EApp _ (EApp _ (BB (TyArr (TyB TyFloat) _) Min) x0) x1) @> b = do
    x0' <- asF<$>(x0@>b); x1' <- asF<$>(x1@>b)
    pure $ mkF (min x0' x1')
(EApp _ (EApp _ (BB (TyArr (TyB TyStr) _) Max) x0) x1) @> b = do
    x0' <- asS<$>(x0@>b); x1' <- asS<$>(x1@>b)
    pure $ mkStr (max x0' x1')
(EApp _ (EApp _ (BB (TyArr (TyB TyStr) _) Min) x0) x1) @> b = do
    x0' <- asS<$>(x0@>b); x1'<-asS<$>(x1@>b)
    pure $ mkStr (min x0' x1')
(EApp _ (EApp _ (BB (TyArr (TyB TyI) _) op) x0) x1) @> b | Just op' <- num op = do
    x0e <- asI<$>(x0@>b); x1e <- asI<$>(x1@>b)
    pure $ mkI (op' x0e x1e)
(EApp _ (EApp _ (BB (TyArr (TyB TyFloat) _) op) x0) x1) @> b | Just op' <- num op = do
    x0e <- asF<$>(x0@>b); x1e <- asF<$>(x1@>b)
    pure $ mkF (op' x0e x1e)
(EApp _ (EApp _ (BB _ Div) x0) x1) @> b = do
    x0e <- x0@>b; x1e <- x1@>b
    pure (mkF (asF x0e/asF x1e))
(EApp _ (EApp _ (BB (TyArr (TyB TyI) _) op) x0) x1) @> b | Just rel <- binRel op = do
    x0e<-asI<$>(x0@>b); x1e<-asI<$>(x1@>b)
    pure (mkB (rel x0e x1e))
(EApp _ (EApp _ (BB (TyArr (TyB TyFloat) _) op) x0) x1) @> b | Just rel <- binRel op = do
    x0e <- asF<$>(x0@>b); x1e <- asF<$>(x1@>b)
    pure (mkB (rel x0e x1e))
(EApp _ (EApp _ (BB (TyArr (TyB TyStr) _) op) x0) x1) @> b | Just rel <- binRel op = do
    x0e <- asS<$>(x0@>b); x1e <- asS<$>(x1@>b)
    pure (mkB (rel x0e x1e))
(EApp _ (EApp _ (BB (TyArr (TyB TyOption:$t@(TyB TyStr)) _) Eq) x0) x1) @> b = do
    x0e <- asM<$>(x0@>b); x1e <- asM<$>(x1@>b)
    case (x0e,x1e) of
        (Nothing, Nothing)   -> pure (mkB True)
        (Just e0b, Just e1b) -> (EApp tyB (EApp (t~>tyB) (BB (TyArr t (t~>t~>tyB)) Eq) e0b) e1b) @> b
        _                    -> pure (mkB False)
(EApp _ (EApp _ (BB (TyArr (TyB TyStr) _) Plus) x0) x1) @> b = do
    x0e <- x0@>b; x1e <- x1@>b
    pure (mkStr (asS x0e<>asS x1e))
(EApp _ (EApp _ (BB _ And) x0) x1) @> b = do
    x0e <- x0@>b; x1e <- x1@>b
    pure (mkB (asB x0e&&asB x1e))
(EApp _ (EApp _ (BB _ Or) x0) x1) @> b = do
    x0e <- x0@>b; x1e <- x1@>b
    pure (mkB (asB x0e||asB x1e))
(EApp _ (EApp _ (UB _ Const) x) _) @> b = x@>b
(EApp _ (EApp _ (BB _ Match) s) r) @> b = do
    s' <- s@>b; r' <- r@>b
    pure (asTup (find' (asR r') (asS s')))
(EApp _ (EApp _ (BB _ Matches) s) r) @> b = do
    se <- s@>b; re <- r@>b
    pure (mkB (isMatch' (asR re) (asS se)))
(EApp _ (EApp _ (BB _ NotMatches) s) r) @> b = do
    se <- s@>b; re <- r@>b
    pure (mkB (not$isMatch' (asR re) (asS se)))
(EApp ty (EApp _ (BB _ MMatch) s) r) @> b = do
    se <- s@>b; re <- r@>b
    pure (if isMatch' (asR re) (asS se) then OptionVal ty (Just$!se) else OptionVal ty Nothing)
(EApp ty (EApp _ (BB _ Take) n) x) @> b = do
    n' <- asI<$>(n@>b); x' <- asV<$>(x@>b)
    pure $ Arr ty (V.take (fromIntegral n') x')
(EApp ty (EApp _ (BB _ Drop) n) x) @> b = do
    n' <- asI<$>(n@>b); x' <- asV<$>(x@>b)
    pure $ Arr ty (V.drop (fromIntegral n') x')
(Tup ty es) @> b = Tup ty.foldSeq <$> traverse (@>b) es
(Rec ty es) @> b = Rec ty.foldSeq <$> traverse (secondM (@>b)) es
(EApp _ (UB _ Head) x) @> b = do
    x' <- x@>b
    pure $ V.head (asV x')
(EApp ty (UB _ Tail) x) @> b = do
    x' <- x@>b
    pure $ Arr ty (V.tail (asV x'))
(EApp _ (UB _ Last) x) @> b = do
    x' <- x@>b
    pure $ V.last (asV x')
(EApp ty (UB _ Init) x) @> b = do
    x' <- x@>b
    pure $ Arr ty (V.init (asV x'))
(EApp _ (UB _ Tally) e) @> b = do
    e' <- e@>b
    pure $ let r=fromIntegral (BS.length$asS e') in mkI r
(EApp _ (UB _ TallyList) e) @> b = do
    e' <- e@>b
    pure $ let r=fromIntegral (V.length$asV e') in mkI r
(EApp _ (EApp _ (BB _ Sprintf) fs) s) @> b = do
    fs' <- fs@>b; s' <- s@>b
    pure (mkStr (sprintf (asS fs') s'))
(Cond _ p e e') @> b = do {p' <- p@>b; if asB p' then e@>b else e'@>b}
(EApp ty (EApp _ (EApp _ (TB _ Captures) s) i) r) @> b = do
    s' <- s@>b; i' <- i@>b; r' <- r@>b
    pure $ OptionVal ty (mkStr <$> findCapture (asR r') (asS s') (fromIntegral$asI i'))
(EApp ty (EApp _ (EApp _ (TB _ AllCaptures) s) i) r) @> b = do
    s' <- s@>b; i' <- i@>b; r' <- r@>b
    pure $ Arr ty (V.fromList (mkStr <$> captures' (asR r') (asS s') (fromIntegral$asI i')))
(EApp _ (EApp _ (EApp _ (TB _ Ixes) s) i) r) @> b = do
    s' <- s@>b; i' <- i@>b; r' <- r@>b
    pure (Arr (tyV (TyTup [tyI,tyI])) (V.fromList (rureMatchTup<$>capturesIx (asR r') (asS s') (fromIntegral (asI i')))))
(NB (TyB TyStr) MZ) @> _ = pure $ mkStr BS.empty
(NB ty@(TyB TyVec:$_) MZ) @> _ = pure $ Arr ty V.empty
(EApp _ (UB _ Not) e) @> b = do {e' <- e@> b; pure$mkB (not (asB e'))}
(EApp _ (EApp _ (BB _ Split) s) r) @> b = do
    s' <- s@>b; r' <- r@>b
    pure $ vS (splitBy (asR r') (asS s'))
(EApp _ (EApp _ (BB _ Splitc) s) c) @> b = do
    s' <- s@>b; c' <- c@>b
    pure $ vS (V.fromList (BS.split (the$asS c') (asS s')))
(EApp _ (UB _ FParse) x) @> b = do {x' <- x@>b; pure (parseAsF (asS x'))}
(EApp _ (UB _ IParse) x) @> b = do {x' <- x@>b; pure (parseAsEInt (asS x'))}
(EApp (TyB TyI) (UB _ Parse) x) @> b = do {x' <- x@>b; pure (parseAsEInt (asS x'))}
(EApp (TyB TyFloat) (UB _ Parse) x) @> b = do {x' <- x@>b; pure (parseAsF (asS x'))}
(EApp _ (UB _ (At i)) v) @> b = do {v' <- v@>b; pure (asV v' `at` i)}
(EApp _ (UB _ (Select i)) x) @> b = do {x' <- x@>b; pure (asT x' !! (i-1))}
(EApp _ (UB _ (SelR n)) x) @> b = do {x' <- x@>b; pure (asRec x' M.! name n)}
(EApp _ (UB _ Floor) x) @> b = mkI.floor.asF<$>(x@>b)
(EApp _ (UB _ Ceiling) x) @> b = mkI.ceiling.asF<$>(x@>b)
(EApp (TyB TyI) (UB _ Negate) i) @> b = mkI.negate.asI<$>(i@>b)
(EApp (TyB TyFloat) (UB _ Negate) x) @> b = mkF.negate.asF<$>(x@>b)
(EApp ty (UB _ Some) e) @> b = OptionVal ty.Just<$>(e@>b)
(NB ty None) @> _ = pure $ OptionVal ty Nothing
(EApp _ (EApp _ (EApp _ (TB _ Substr) s) i0) i1) @> b = do
    i0' <- i0@>b; i1' <- i1@>b; s' <- s@>b
    pure $ mkStr (substr (asS s') (fromIntegral$asI i0') (fromIntegral$asI i1'))
(EApp _ (EApp _ (EApp _ (TB _ Sub1) r) s0) s1) @> b = do
    r' <- r@>b; s0' <- s0@>b; s1' <- s1@>b
    pure $ mkStr (sub1 (asR r') (asS s1') (asS s0'))
(EApp _ (EApp _ (EApp _ (TB _ Subs) r) s0) s1) @> b = do
    r' <- r@>b; s0' <- s0@>b; s1' <- s1@>b
    pure $ mkStr (subs (asR r') (asS s1') (asS s0'))
(EApp _ (EApp _ (EApp _ (TB _ Fold) op) seed) xs) @> b | TyB TyVec:$_ <- eLoc xs = do
    seed' <- seed@>b; xs' <- xs@>b
    V.foldM (a2e b op) seed' (asV xs')
(EApp _ (EApp _ (BB _ Rein) s) ss) @> b | TyB TyVec:$_ <- eLoc ss = do
    s' <- fmap asS (s@>b); ss' <- ss@>b
    pure $ mkStr (V.foldl' (\x y -> x <> s' <> y) mempty (asS<$>asV ss'))
(EApp yT@(TyB TyVec:$_) (EApp _ (EApp _ (TB _ ScanList) op) seed) xs) @> b | TyB TyVec:$_ <- eLoc xs = do
    xs' <- xs@>b; seed' <- seed@>b
    Arr yT <$> scanlM' (a2e b op) seed' (asV xs')
(EApp _ (EApp _ (BB _ Fold1) op) xs) @> b | TyB TyVec:$_ <- eLoc xs = do
    xs' <- xs@>b
    let xsV=asV xs'
        (seed, xs'') = case V.uncons xsV of
            Just v  -> v
            Nothing -> throw EmptyFold
    V.foldM (a2e b op) seed xs''
(EApp yT@(TyB TyVec:$_) (EApp _ (BB _ Filter) p) xs) @> b = do
    xs' <- xs@>b
    Arr yT <$> V.filterM (fmap asB.a1e b p) (asV xs')
(EApp yT@(TyB TyVec:$_) (EApp _ (BB _ Map) f) xs) @> b = do
    xs' <- xs@>b
    Arr yT <$> traverse (a1e b f) (asV xs')
(EApp yT@(TyB TyOption:$_) (EApp _ (BB _ Map) f) x) @> b = do
    x' <- x@>b
    OptionVal yT <$> traverse (a1e b f) (asM x')
(EApp yT@(TyB TyVec:$_) (EApp _ (BB _ MapMaybe) g) x) @> b = do
    x' <- x@>b
    Arr yT <$> V.mapMaybeM (fmap asM.a1e b g) (asV x')
(EApp yT@(TyB TyVec:$_) (UB _ CatMaybes) x) @> b = do
    x' <- x@>b
    pure $ Arr yT (V.catMaybes (asM<$>asV x'))
(EApp t (EApp _ (EApp _ (TB _ Option) x) g) y) @> b = do
    x' <- x@>b; y' <- y@>b
    case asM y' of
        Nothing -> pure x'
        Just yϵ -> (@>b) =<< lβ (EApp t g yϵ)
(Arr t es) @> b = Arr t <$> traverse (@>b) es
e@BB{} @> _ = pure e
e@TB{} @> _ = pure e
e@UB{} @> _ = pure e
(Lam t n e) @> b = Lam t n <$> (e@>b)
e @> _ | TyArr{} <- eLoc e = throw $ StreamFunc e
-- basically an option can evaluate to a function... so ((option ...) x)
-- needs to be reduced! but nothing will detect that...
-- (when can a builtin etc. return a FUNCTION? if...then...else could!)
--
-- Question: would (f x) ever need for x to be inspected in order for things
-- to proceed?? I think no...
--
-- thabove returns g'=(and another +) e=line (should be further reduced!)
-- but g'=(+) and e=... will trip up

me :: [(Nm T, E T)] -> Β
me xs = IM.fromList [(unU$unique nm, e) | (nm, e) <- xs]

ms :: Nm T -> E T -> Β
ms (Nm _ (U i) _) = IM.singleton i

wCM :: Tmp -> Tmp -> Σ -> Σ
wCM src tgt (Σ u env d di df b) =
    Σ u (case env!src of
        Just y  -> case asM y of {Nothing -> tgt\~env; Just yϵ -> env&tgt~!yϵ}
        Nothing -> tgt\~env) d di df b

{-# SCC wMM #-}
wMM :: E T -> Tmp -> Tmp -> Σ -> Σ
wMM (Lam _ n e) src tgt (Σ j env d di df b) =
    case env!src of
        Just x ->
            let be=ms n x; (y,k)=e@!(j,be)
            in Σ k (case asM y of
                Just yϵ -> env&tgt~!yϵ
                Nothing -> tgt\~env) d di df b
        Nothing -> Σ j (tgt\~env) d di df b
wMM e _ _ _ = throw$InternalArityOrEta 1 e

wZ :: E T -> Tmp -> Tmp -> Tmp -> Σ -> Σ
wZ (Lam _ n0 (Lam _ n1 e)) src0 src1 tgt (Σ j env d di df b) =
    (case (env!src0, env!src1) of
        (Just x, Just y) ->
            let be=me [(n0, x), (n1, y)]; (z,k)=e@!(j,be)
            in Σ k (env&tgt~!z)
        (Nothing, Nothing) -> Σ j (tgt\~env)) d di df b
wZ e _ _ _ _ = throw$InternalArityOrEta 2 e

wM :: E T -> Tmp -> Tmp -> Σ -> Σ
wM (Lam _ n e) src tgt (Σ j env d di df b) =
    case env!src of
        Just x ->
            let be=ms n x; (y,k)=e@!(j,be)
            in Σ k (env&tgt~!y) d di df b
        Nothing -> Σ j (tgt\~env) d di df b
wM e _ _ _ = throw$InternalArityOrEta 1 e

wI :: E T -> Tmp -> LineCtx -> Σ -> Σ
wI e tgt line (Σ j env d di df b) =
    let e'=e `κ` line; (e'',k)=e'$@j in Σ k (env&tgt~!e'') d di df b

wG :: (E T, E T) -> Tmp -> LineCtx -> Σ -> Σ
wG (p, e) tgt line (Σ j env d di df b) =
    let p'=p `κ` line; (p'',k)=p'$@j
    in (if asB p''
        then let e'=e `κ` line; (e'',u) =e'$@k in Σ u (env&tgt~!e'')
        else Σ k (tgt\~env)) d di df b

wDOp :: E T -> Int -> Tmp -> Tmp -> Σ -> Σ
wDOp (Lam (TyArr _ (TyB TyStr)) n e) key src tgt (Σ i env d di df b) =
    case env!src of
        Nothing -> Σ i (tgt\~env) d di df b
        Just xϵ ->
            case IM.lookup key d of
                Nothing -> Σ k (env&tgt~!y) (IM.insert key (S.singleton e') d) di df b
                Just ss -> (if e' `S.member` ss then Σ k (tgt\~env) d else Σ k (env&tgt~!y) (key!:e'$d)) di df b
              where
                (y,k)=e@!(i,be); be=ms n xϵ
                e'=asS y
wDOp (Lam (TyArr _ (TyB TyI)) n e) key src tgt (Σ i env d di df b) =
    case env!src of
        Nothing -> Σ i (tgt\~env) d di df b
        Just xϵ ->
            case IM.lookup key di of
                Nothing -> Σ k (env&tgt~!y) d (IM.insert key (IS.singleton e') di) df b
                Just ds -> (if e' `IS.member` ds then Σ k (tgt\~env) d di else Σ k (env&tgt~!y) d (IM.alter go key di)) df b

              where
                (y,k)=e@!(i,be); be=ms n xϵ
                e'=fromIntegral$asI y

                go Nothing  = Just$!IS.singleton e'
                go (Just s) = Just$!IS.insert e' s
wDOp (Lam (TyArr _ (TyB TyFloat)) n e) key src tgt (Σ i env d di df b) =
    case env!src of
        Nothing -> Σ i (tgt\~env) d di df b
        Just xϵ ->
            case IM.lookup key df of
                Nothing -> Σ k (env&tgt~!y) d di (IM.insert key (S.singleton e') df) b
                Just ds -> if e' `S.member` ds then Σ k (tgt\~env) d di df b else Σ k (env&tgt~!y) d di (key!:e'$df) b
              where
                (y,k)=e@!(i,be); be=ms n xϵ
                e'=asF y
wDOp e _ _ _ _ = throw $ InternalArityOrEta 1 e

(\~) k = IM.insert k Nothing
(~!) k x = IM.insert k (Just$!x)

(!:) k e = IM.alter (\x -> Just$!case x of Nothing -> S.singleton e; Just s -> S.insert e s) k

wB :: (E T, E T) -> Int -> Tmp -> Tmp -> Σ -> Σ
wB (e0, e1) key src tgt (Σ i env d di df b) =
    case env!src of
        Nothing -> Σ i (tgt\~env) d di df b
        Just xϵ -> let xS=asS xϵ in if key `IS.member` b
            then if isMatch' r1 xS then Σ i (env&tgt~!xϵ) d di df (IS.delete key b) else Σ i (env&tgt~!xϵ) d di df b
            else if isMatch' r0 xS then Σ i (env&tgt~!xϵ) d di df (IS.insert key b) else Σ i (tgt\~env) d di df b
  where
    r0=asR e0; r1=asR e1

{-# SCC wD #-}
wD :: TB -> Int -> Tmp -> Tmp -> Σ -> Σ
wD TyStr key src tgt (Σ i env d di df b) =
    case env!src of
        Nothing -> Σ i (tgt\~env) d di df b
        Just e ->
            case IM.lookup key d of
                Nothing -> Σ i (env&tgt~!e) (IM.insert key (S.singleton e') d) di df b
                Just ds -> (if e' `S.member` ds then Σ i (tgt\~env) d else Σ i (env&tgt~!e) (key!:e'$d)) di df b
              where
                e'=asS e
wD TyI key src tgt (Σ i env d di df b) =
    case env!src of
        Nothing -> Σ i (tgt\~env) d di df b
        Just e ->
            case IM.lookup key di of
                Nothing -> Σ i (env&tgt~!e) d (IM.insert key (IS.singleton e') di) df b
                Just ds -> (if e' `IS.member` ds then Σ i (tgt\~env) d di else Σ i (env&tgt~!e) d (IM.alter go key di)) df b
              where
                e'=fromIntegral$asI e

                go Nothing  = Just$!IS.singleton e'
                go (Just s) = Just$!IS.insert e' s
wD TyFloat key src tgt (Σ i env d di df b) =
    case env!src of
        Nothing -> Σ i (tgt\~env) d di df b
        Just e ->
            case IM.lookup key df of
                Nothing -> Σ i (env&tgt~!e) d di (IM.insert key (S.singleton e') df) b
                Just ds -> (if e' `S.member` ds then Σ i (tgt\~env) d di df else Σ i (env&tgt~!e) d di (key!:e'$df)) b
              where
                e'=asF e


wP :: E T -> Tmp -> Tmp -> Σ -> Σ
wP (Lam _ n e) src tgt (Σ j env d di df b) =
    case env!src of
        Just x ->
            let be=ms n x; (p,k)=e@!(j,be)
            in Σ k (IM.insert tgt (if asB p then Just$!x else Nothing) env) d di df b
        Nothing -> Σ j (tgt\~env) d di df b
wP e _ _ _ = throw $ InternalArityOrEta 1 e

wΠ :: E T -> Tmp -> Tmp -> Tmp -> Σ -> Σ
wΠ (Lam _ nn (Lam _ nprev e)) pt src tgt (Σ j env d di df b) =
    (case (env!pt, env!src) of
        (Just prev, Just x) ->
            let be=me [(nprev, prev), (nn, x)]
                (res,u)=e@!(j,be)
            in Σ u (IM.insert pt (Just$!x) (IM.insert tgt (Just$!res) env))
        (Nothing, Nothing) -> Σ j (tgt\~env)
        (Nothing, Just x) -> Σ j (pt~!x$tgt\~env)
        (Just{}, Nothing) -> Σ j (tgt\~env)) d di df b
wΠ e _ _ _ _ = throw $ InternalArityOrEta 2 e

{-# SCC wF #-}
wF :: E T -> Tmp -> Tmp -> Σ -> Σ
wF (Lam _ nacc (Lam _ nn e)) src tgt (Σ j env d di df b) =
    (case (env!tgt, env!src) of
        (Just acc, Just x) ->
            let be=me [(nacc, acc), (nn, x)]
                (res, u)=e@!(j, be)
            in Σ u (env&tgt~!res)
        (Just acc, Nothing) -> Σ j (env&tgt~!acc)
        (Nothing, Nothing) -> Σ j (tgt\~env)
        (Nothing, Just x) -> Σ j (env&tgt~!x)) d di df b
wF e _ _ _ = throw $ InternalArityOrEta 2 e

badctx e = error ("Internal error: κ called on" ++ show e)
desugar = error "Internal error. Should have been desugared by now."

(<$!>) f x = fmap (f$!) x
