module File ( tcIO
            , runStdin, runOnFile
            , exprEval
            ) where

import           A
import           A.E
import           A.I
import           Control.Applicative              ((<|>))
import           Control.Exception                (Exception, throw, throwIO)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.State.Strict (StateT, get, put, runState, runStateT)
import           Data.Bifunctor                   (second)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BSL
import qualified Data.ByteString.Lazy.Char8       as ASCIIL
import           Data.Foldable                    (fold, traverse_)
import           Data.Functor                     (($>))
import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)
import qualified Data.Text.IO                     as TIO
import           Data.Tuple                       (swap)
import qualified Data.Vector                      as V
import           Include
import           Jacinda.Backend.Const
import           Jacinda.Backend.T
import           Jacinda.Check.Field
import           Jacinda.Regex
import           L
import           Parser
import           Parser.Rw
import           Prettyprinter                    (Pretty (pretty))
import           R
import           Regex.Rure                       (RurePtr)
import           System.IO                        (Handle, stdin, stdout)
import           Text.CSV.Lazy.ByteString         (CSVField (..), parseCSV)
import           Ty

csvCtx :: BSL.ByteString -> [LineCtx]
csvCtx = go Nothing . parseCSV where
    go _ []                  = []
    go _ (Left err:_)        = error (show err)
    -- TODO: re-csv it?
    go (Just n) (Right r:rs) = let fs=mB<$>r in (fold fs, V.fromListN n fs, fromIntegral (line r)):go (Just n) rs
    go Nothing (Right r:rs)  = let fs=mB<$>r; n=length fs in (fold fs, V.fromListN n fs, fromIntegral (line r)):go (Just n) rs
    mB f@CSVField{} = BSL.toStrict (csvFieldContent f)
    mB f            = error (show f)
    line (f@CSVField{}:_) = csvRowNum f
    line (f:_)            = error (show f)
    line []               = error "empty record in csv"

parseLib :: [FilePath] -> FilePath -> StateT AlexUserState IO [D AlexPosn]
parseLib incls fp = do
    contents <- liftIO $ TIO.readFile =<< resolveImport incls fp
    st <- get
    case parseLibWithCtx contents st of
        Left err              -> liftIO (throwIO (FPos fp<$>err))
        Right (st', ([], ds)) -> put st' $> (rwD <$> ds)
        Right (st', (is, ds)) -> do {put st'; dss <- traverse (parseLib incls) is; pure (concat dss ++ fmap rwD ds)}

parseP :: [FilePath] -> FilePath -> T.Text -> [(T.Text, Value)] -> StateT AlexUserState IO (Program AlexPosn)
parseP incls fn src var = do
    st <- get
    case parseWithCtx src var st of
        Left err -> liftIO $ throwIO (FPos fn<$>err)
        Right (st', (is, Program ds e)) -> do
            put st'
            dss <- traverse (parseLib incls) is
            pure $ Program (concat dss ++ fmap rwD ds) (rwE e)

-- | Parse + rename
parsePWithMax :: [FilePath] -> FilePath -> T.Text -> [(T.Text, T.Text)] -> IO (Program AlexPosn, Int)
parsePWithMax incls fn src vars = uncurry rP.swap.second fst3 <$> runStateT (parseP incls fn src vars) alexInitUserState
    where fst3 (x,_,_) = x

parseWithMax' :: T.Text -> Either (ParseError AlexPosn) (Program AlexPosn, Int)
parseWithMax' = fmap (uncurry rP . second (rwP.snd)) . parseWithMax

type FileBS = BS.ByteString

data FPos = FPos { filen :: String, pos :: !AlexPosn }

instance Pretty FPos where pretty (FPos f l) = pretty f <> ":" <> pretty l

tcompile=compileDefault.encodeUtf8

compileR :: FileBS
         -> E T
         -> E T
compileR fp = r where
    r (RegexLit _ rrϵ)  = RC (compileDefault rrϵ)
    r (NB _ Fp)         = mkStr fp
    r e@Var{}           = e
    r e@UB{}            = e
    r e@NB{}            = e
    r e@Lit{}           = e
    r e@TB{}            = e
    r e@BB{}            = e
    r (Cond l p e0 e1)  = Cond l (r p) (r e0) (r e1)
    r (OptionVal l e)   = OptionVal l (r<$>e)
    r (EApp l e0 e1)    = EApp l (r e0) (r e1)
    r e@Column{}        = e
    r e@IParseCol{}     = e
    r e@IParseAllCol{}  = e
    r e@FParseAllCol{}  = e
    r e@ParseAllCol{}   = e
    r e@FParseCol{}     = e
    r e@ParseCol{}      = e
    r e@LastField{}     = e
    r e@Field{}         = e
    r e@FieldList{}     = e
    r e@AllField{}      = e
    r e@AllColumn{}     = e
    r (Guarded l p e)   = Guarded l (r p) (r e)
    r (Implicit l e)    = Implicit l (r e)
    r (Let l (n, eb) e) = Let l (n, r eb) (r e)
    r (Lam l n e)       = Lam l n (r e)
    r (Tup l es)        = Tup l (r<$>es)
    r (Rec l es)        = Rec l (second r<$>es)
    r (Arr l es)        = Arr l (r<$>es)
    r (Anchor l es)     = Anchor l (r<$>es)
    r F{}               = error "impossible"
    r Dfn{}             = desugar
    r Paren{}           = desugar
    r ResVar{}          = desugar
    r RwB{}             = desugar
    r RwT{}             = desugar
    r RC{}              = error "???"

exprEval :: T.Text -> E T
exprEval src =
    case parseWithMax' src of
        Left err -> throw err
        Right (ast, m) ->
            let (typed, i) = yeet $ runTyM m (tyP ast)
                (inlined, j) = ib i typed
            in eB j (compileR (error "nf not defined.") inlined)

compileFS :: Maybe T.Text -> RurePtr
compileFS = maybe defaultRurePtr tcompile

runOnBytes :: [FilePath]
           -> FilePath -- ^ Data file name, for @nf@
           -> FilePath -- ^ For error locations
           -> T.Text -- ^ Program
           -> [(T.Text, Value)]
           -> Mode
           -> BSL.ByteString
           -> Handle -- ^ Out handle
           -> IO ()
runOnBytes incls fp fn src vars mode contents h = do
    incls' <- defaultIncludes <*> pure incls
    (ast, m) <- parsePWithMax incls' fn src vars
    (typed, i) <- yIO fn $ runTyM m (tyP ast)
    let (eI, j) = ib i typed
    m'Throw $ cF eI
    let (e', k) = runState (eta eI) j
        cont=run h (flushD typed) k (compileR (encodeUtf8 $ T.pack fp) e')
    case (mode, getS ast) of
        (AWK cliFS cliRS cliH, AWK afs ars ah) ->
            let r=compileFS (cliFS <|> afs)
                bs=case (cliRS <|> ars, cliH||ah) of
                    (Nothing, _)     -> fmap BSL.toStrict (ASCIIL.lines contents)
                    (Just rs, False) -> lazySplit (tcompile rs) contents
                    (Just rs, True)  -> lazySplitH (tcompile rs) contents
                ctxs=zipWith (\ ~(x,y) z -> (x,y,z)) [(b, splitBy r b) | b <- bs] [1..]
            in cont ctxs
        (CSV, _) -> let ctxs = csvCtx contents in cont ctxs
        (_, CSV) -> let ctxs = csvCtx contents in cont ctxs

runStdin :: [FilePath]
         -> FilePath -- ^ For error location
         -> T.Text -- ^ Program
         -> [(T.Text, Value)]
         -> Mode
         -> IO ()
runStdin is src fn vars m = do {b <- BSL.hGetContents stdin; runOnBytes is "(stdin)" src fn vars m b stdout}

runOnFile :: [FilePath]
          -> FilePath
          -> T.Text
          -> [(T.Text, Value)]
          -> Mode
          -> FilePath
          -> Handle -- ^ May need to be closed (lazy bytestring I/O)
          -> IO ()
runOnFile is fn e vs m fp h = do {b <- BSL.readFile fp; runOnBytes is fp fn e vs m b h}

tcIO :: [FilePath] -> FilePath -> T.Text -> IO ()
tcIO incls fn src = do
    incls' <- defaultIncludes <*> pure incls
    (ast, m) <- parsePWithMax incls' fn src []
    (pT, i) <- yIO fn $ runTyM m (tyP ast)
    let (eI, _) = ib i pT
    m'Throw $ cF eI

m'Throw :: Exception e => Maybe e -> IO ()
m'Throw = traverse_ throwIO

yIO fp = either (throwIO.(FPos fp<$>)) pure

yeet :: Exception e => Either e a -> a
yeet = either throw id

desugar :: a
desugar = error "Interal error: should have been desugared."
