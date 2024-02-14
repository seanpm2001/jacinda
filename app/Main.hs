{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Version        as V
import           File
import           Options.Applicative
import qualified Paths_jacinda       as P
import           System.IO           (stdin)

data Command = TypeCheck !FilePath ![FilePath]
             | Run !FilePath !(Maybe FilePath) ![FilePath]
             | Expr !T.Text !(Maybe FilePath) !(Maybe T.Text) !Bool !(Maybe T.Text) ![FilePath]
             | Eval !T.Text

jacFile :: Parser FilePath
jacFile = argument str
    (metavar "JACFILE"
    <> help "Source code"
    <> jacCompletions)

asv :: Parser Bool
asv = switch
    (long "asv"
    <> help "Read from ASV")

jacRs :: Parser (Maybe T.Text)
jacRs = optional $ option str
    (short 'R'
    <> metavar "REGEXP"
    <> help "Record separator")

jacFs :: Parser (Maybe T.Text)
jacFs = optional $ option str
    (short 'F'
    <> metavar "REGEXP"
    <> help "Field separator")

jacExpr :: Parser T.Text
jacExpr = argument str
    (metavar "EXPR"
    <> help "Jacinda expression")

inpFile :: Parser (Maybe FilePath)
inpFile = optional $ option str
    (short 'i'
    <> metavar "DATAFILE"
    <> help "Data file")

jacCompletions :: HasCompleter f => Mod f a
jacCompletions = completer . bashCompleter $ "file -X '!*.jac' -o plusdirs"

commandP :: Parser Command
commandP = hsubparser
    (command "tc" (info tcP (progDesc "Type-check file"))
    <> command "e" (info eP (progDesc "Evaluate an expression (no file context)"))
    <> command "run" (info runP (progDesc "Run from file")))
    <|> exprP
    where
        tcP = TypeCheck <$> jacFile <*> includes
        runP = Run <$> jacFile <*> inpFile <*> includes
        exprP = Expr <$> jacExpr <*> inpFile <*> jacFs <*> asv <*> jacRs <*> includes
        eP = Eval <$> jacExpr

includes :: Parser [FilePath]
includes = many $ strOption
    (metavar "DIR"
    <> long "include"
    <> short 'I'
    <> dirCompletions)

dirCompletions :: HasCompleter f => Mod f a
dirCompletions = completer . bashCompleter $ "directory"


wrapper :: ParserInfo Command
wrapper = info (helper <*> versionMod <*> commandP)
    (fullDesc
    <> progDesc "Jacinda language for functional stream processing, filtering, and reports"
    <> header "Jacinda - a functional complement to AWK")

versionMod :: Parser (a -> a)
versionMod = infoOption (V.showVersion P.version) (short 'V' <> long "version" <> help "Show version")

main :: IO ()
main = run =<< execParser wrapper

ap :: Bool -> Maybe T.Text -> Maybe T.Text
ap True Just{}  = errorWithoutStackTrace "--asv specified with field separator."
ap True Nothing = Just "\\x1f"
ap _ fs         = fs

run :: Command -> IO ()
run (TypeCheck fp is)              = tcIO is =<< TIO.readFile fp
run (Run fp Nothing is)            = do { contents <- TIO.readFile fp ; runOnHandle is contents Nothing Nothing stdin }
run (Run fp (Just dat) is)         = do { contents <- TIO.readFile fp ; runOnFile is contents Nothing Nothing dat }
run (Expr eb Nothing fs a rs is)   = runOnHandle is eb (ap a fs) rs stdin
run (Expr eb (Just fp) fs a rs is) = runOnFile is eb (ap a fs) rs fp
run (Eval e)                       = print (exprEval e)
