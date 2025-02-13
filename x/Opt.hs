{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           A                   (Mode (..))
import           Control.Applicative ((<|>))
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Version        as V
import           File
import           Options.Applicative (HasCompleter, Mod, Parser, argument, bashCompleter, command, completer, execParser, fullDesc, header, help, helper, hsubparser, info,
                                      infoOption, long, many, metavar, option, optional, progDesc, short, str, strOption, switch)
import           Parser              (Value)
import qualified Paths_jacinda       as P
import           System.Directory    (doesDirectoryExist)
import           System.IO           (stdout)

data Cmd = TC !FilePath ![FilePath]
         | Run !FilePath !(Maybe T.Text) !(Maybe T.Text) !(Maybe FilePath) ![FilePath] ![(T.Text, Value)] Bool
         | Expr !T.Text !(Maybe FilePath) !(Maybe T.Text) !Bool !Bool !Bool !(Maybe T.Text) ![FilePath] Bool
         | Eval !T.Text
         | Install

kv :: T.Text -> (T.Text, Value)
kv inp = case T.splitOn "=" inp of
    [k,v] -> (k, v)
    _     -> error "Values specified on the command-line must be of the form -Dvar=value"

defVar :: Parser [(T.Text, Value)]
defVar = many $ fmap kv (option str
    (short 'D'
    <> metavar "VALUE"
    <> help "Pass a value on the command-line"))

j'File :: Parser FilePath
j'File = argument str
    (metavar "JACFILE"
    <> help "Source code"
    <> jacCompletions)

begin, csv, asv, usv :: Parser Bool
csv = switch
    (long "csv"
    <> help "Process as CSV")

asv = switch
    (long "asv"
    <> help "Process as ASV")

usv = switch
    (long "usv"
    <> short 'u'
    <> help "Process as USV")

begin = switch
    (long "header"
    <> short 'b'
    <> help "Include match in record")

jacRs, jacFs :: Parser (Maybe T.Text)
jacRs = optional $ option str
    (short 'R'
    <> metavar "REGEXP"
    <> help "Record separator")

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

jacCompletions, dirCompletions :: HasCompleter f => Mod f a
jacCompletions = completer . bashCompleter $ "file -X '!*.jac' -o plusdirs"
dirCompletions = completer . bashCompleter $ "directory"

commandP :: Parser Cmd
commandP = hsubparser
    (command "tc" (info tcP (progDesc "Type-check file"))
    <> command "e" (info eP (progDesc "Evaluate an expression (no file context)"))
    <> command "run" (info runP (progDesc "Run from file"))
    <> command "install-dir" (info (pure Install) (progDesc "Show directory for bundled libraries")))
    <|> exprP
    where
        tcP = TC <$> j'File <*> incls; eP = Eval <$> jacExpr
        runP = Run <$> j'File <*> jacFs <*> jacRs <*> inpFile <*> incls <*> defVar <*> begin
        exprP = Expr <$> jacExpr <*> inpFile <*> jacFs <*> asv <*> usv <*> csv <*> jacRs <*> incls <*> begin

incls :: Parser [FilePath]
incls = many $ strOption
    (metavar "DIR"
    <> long "include"
    <> short 'I'
    <> dirCompletions)

wrapper = info (helper <*> versionMod <*> commandP)
    (fullDesc
    <> progDesc "Jacinda language for functional stream processing, filtering, and reports"
    <> header "Jacinda - a functional complement to AWK")

versionMod :: Parser (a -> a)
versionMod = infoOption (V.showVersion P.version) (short 'V' <> long "version" <> help "Show version")

main :: IO ()
main = run =<< execParser wrapper

ap :: Bool -> Bool -> Bool -> Maybe T.Text -> Maybe T.Text -> Bool -> Mode
ap True True _ _ _          = errorWithoutStackTrace "--asv and --usv both specified."
ap True _ True _ _          = errorWithoutStackTrace "--asv and --csv both specified."
ap _ True True _ _          = errorWithoutStackTrace "--usv and --csv both specified."
ap _ True _ Just{} _        = errorWithoutStackTrace "--usv and field separator both speficied."
ap _ True _ _ Just{}        = errorWithoutStackTrace "--usv and record separator both speficied."
ap True _ _ Just{} _        = errorWithoutStackTrace "--asv and field separator both speficied."
ap True _ _ _ Just{}        = errorWithoutStackTrace "--asv and record separator both speficied."
ap _ _ True Just{} _        = errorWithoutStackTrace "--csv and field separator both speficied."
ap _ _ True _ Just{}        = errorWithoutStackTrace "--csv and record separator both speficied."
ap _ _ True Nothing Nothing = const CSV
ap True _ _ Nothing Nothing = AWK (Just "\\x1f") (Just "\\x1e")
ap _ True _ Nothing Nothing = AWK (Just "␟") (Just "␞")
ap _ _ _ fs rs              = AWK fs rs

run :: Cmd -> IO ()
run (TC fp is)                         = tcIO is fp =<< TIO.readFile fp
run (Run fp fs rs Nothing is vs h)    = do { contents <- TIO.readFile fp ; runStdin is (Just fp) contents vs (AWK fs rs h) }
run (Run fp fs rs (Just dat) is vs h) = do { contents <- TIO.readFile fp ; runOnFile is (Just fp) contents vs (AWK fs rs h) dat stdout }
run (Expr eb f fs a u c rs is h)      =
    case f of
        Nothing -> runStdin is Nothing eb [] (m h)
        Just fp -> runOnFile is Nothing eb [] (m h) fp stdout
  where
    m = ap a u c fs rs
run (Eval e)                          = print (exprEval e)
run Install                           = putStrLn =<< getDataDir

getDataDir = do
    cabal <- P.getDataDir
    b <- doesDirectoryExist cabal
    pure $ if b then cabal else "/usr/local/share/jac"
