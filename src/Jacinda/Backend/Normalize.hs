-- TODO: test this module?
module Jacinda.Backend.Normalize ( eNorm
                                 , compileR
                                 ) where

import           Control.Recursion (cata, embed)
import qualified Data.ByteString   as BS
import           Jacinda.AST
import           Jacinda.Regex
import           Jacinda.Ty.Const

-- fill in regex with compiled.
compileR :: E (T K)
         -> E (T K)
compileR = cata a where
    a (RegexLitF _ rr) = RegexCompiled (compileDefault rr)
    a x                = embed x

-- will need a state/context at some point (let &c.)
eNorm :: E (T K)
      -> E (T K)
eNorm e@Field{}       = e
eNorm e@IParseField{} = e
eNorm e@FParseField{} = e
eNorm e@IntLit{}      = e
eNorm e@FloatLit{}    = e
eNorm e@BoolLit{}     = e
eNorm e@StrLit{}      = e
eNorm e@RegexLit{}    = e
eNorm e@RegexCompiled{} = e
eNorm e@UBuiltin{}    = e
eNorm e@Column{}      = e
eNorm e@AllColumn{}   = e
eNorm e@IParseCol{}   = e
eNorm e@FParseCol{}   = e
eNorm e@AllField{}    = e
eNorm e@Guarded{}     = e
eNorm e@Lam{}         = e
eNorm e@BBuiltin{}    = e
eNorm e@TBuiltin{}    = e
eNorm e@Tup{}         = e
eNorm e@Ix{}          = e
eNorm e@(EApp _ BBuiltin{} _) = e
eNorm e0@(EApp _ (EApp _ (BBuiltin _ Matches) e) e') =
    let eI = eNorm e
        eI' = eNorm e'
    in case (eI, eI') of
        (RegexCompiled re, StrLit _ str) -> BoolLit tyBool (isMatch' re str)
        (StrLit _ str, RegexCompiled re) -> BoolLit tyBool (isMatch' re str)
        _                                -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin _ NotMatches) e) e') =
    let eI = eNorm e
        eI' = eNorm e'
    in case (eI, eI') of
        (RegexCompiled re, StrLit _ str) -> BoolLit tyBool (not $ isMatch' re str)
        (StrLit _ str, RegexCompiled re) -> BoolLit tyBool (not $ isMatch' re str)
        _                                -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Plus) e) e') =
    let eI = eNorm e
        eI' = eNorm e'
    in case (eI, eI') of
        (IntLit _ i, IntLit _ j) -> IntLit tyI (i+j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Max) e) e') =
    let eI = eNorm e
        eI' = eNorm e'
    in case (eI, eI') of
        (IntLit _ i, IntLit _ j) -> IntLit tyI (max i j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Min) e) e') =
    let eI = eNorm e
        eI' = eNorm e'
    in case (eI, eI') of
        (IntLit _ i, IntLit _ j) -> IntLit tyI (min i j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Max) e) e') =
    let eI = eNorm e
        eI' = eNorm e'
    in case (eI, eI') of
        (FloatLit _ x, FloatLit _ y) -> FloatLit tyF (max x y)
        _                            -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Min) e) e') =
    let eI = eNorm e
        eI' = eNorm e'
    in case (eI, eI') of
        (FloatLit _ x, FloatLit _ y) -> FloatLit tyF (min x y)
        _                            -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Minus) e) e') =
    let eI = eNorm e
        eI' = eNorm e'
    in case (eI, eI') of
        (IntLit _ i, IntLit _ j) -> IntLit tyI (i-j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Times) e) e') =
    let eI = eNorm e
        eI' = eNorm e'
    in case (eI, eI') of
        (IntLit _ i, IntLit _ j) -> IntLit tyI (i*j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Plus) e) e') =
    let eI = eNorm e
        eI' = eNorm e'
    in case (eI, eI') of
        (FloatLit _ i, FloatLit _ j) -> FloatLit tyF (i+j)
        _                            -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Minus) e) e') =
    let eI = eNorm e
        eI' = eNorm e'
    in case (eI, eI') of
        (FloatLit _ i, FloatLit _ j) -> FloatLit tyF (i-j)
        _                            -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Times) e) e') =
    let eI = eNorm e
        eI' = eNorm e'
    in case (eI, eI') of
        (FloatLit _ i, FloatLit _ j) -> FloatLit tyF (i*j)
        _                            -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Div) e) e') =
    let eI = eNorm e
        eI' = eNorm e'
    in case (eI, eI') of
        (FloatLit _ i, FloatLit _ j) -> FloatLit tyF (i/j)
        _                            -> e0
eNorm e0@(EApp _ (UBuiltin _ Tally) e) =
    let eI = eNorm e
    in case eI of
        StrLit _ str -> IntLit tyI (fromIntegral $ BS.length str)
        _            -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Lt) e) e') =
    let eI = eNorm e
        eI' = eNorm e'
    in case (eI, eI') of
        (IntLit _ i, IntLit _ j) -> BoolLit tyBool (i < j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Gt) e) e') =
    let eI = eNorm e
        eI' = eNorm e'
    in case (eI, eI') of
        (IntLit _ i, IntLit _ j) -> BoolLit tyBool (i > j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Eq) e) e') =
    let eI = eNorm e
        eI' = eNorm e'
    in case (eI, eI') of
        (IntLit _ i, IntLit _ j) -> BoolLit tyBool (i == j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyStr) _) Eq) e) e') =
    let eI = eNorm e
        eI' = eNorm e'
    in case (eI, eI') of
        (StrLit _ i, StrLit _ j) -> BoolLit tyBool (i == j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Neq) e) e') =
    let eI = eNorm e
        eI' = eNorm e'
    in case (eI, eI') of
        (IntLit _ i, IntLit _ j) -> BoolLit tyBool (i /= j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyStr) _) Neq) e) e') =
    let eI = eNorm e
        eI' = eNorm e'
    in case (eI, eI') of
        (StrLit _ i, StrLit _ j) -> BoolLit tyBool (i /= j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin _ And) e) e') =
    let eI = eNorm e
        eI' = eNorm e'
    in case (eI, eI') of
        (BoolLit _ b, BoolLit _ b') -> BoolLit tyBool (b && b')
        _                           -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin _ Or) e) e') =
    let eI = eNorm e
        eI' = eNorm e'
    in case (eI, eI') of
        (BoolLit _ b, BoolLit _ b') -> BoolLit tyBool (b || b')
        _                           -> e0
eNorm (EApp _ (EApp _ (UBuiltin _ Const) e) _) = e
eNorm e@(EApp _ (UBuiltin _ Const) _) = e
