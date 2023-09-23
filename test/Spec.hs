{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           A
import           Control.Monad    ((<=<))
import qualified Data.ByteString  as BS
import           Data.Foldable    (toList)
import           Data.Functor     (void)
import qualified Data.Text        as T
import qualified Data.Text.IO     as TIO
import           File
import           Jacinda.Regex
import           Parser
import           Parser.Rw
import           Test.Tasty
import           Test.Tasty.HUnit
import           Ty.Const

main :: IO ()
main = defaultMain $
    testGroup "Jacinda interpreter"
        [ testCase "parses no error" (parseNoErr sumBytes)
        , testCase "parse as" (parseTo sumBytes sumBytesAST)
        , testCase "parse as" (parseTo "#`0>72" pAst)
        , parseFile "test/examples/ab.jac"
        , splitWhitespaceT "1 1.3\tj" ["1", "1.3", "j"]
        , splitWhitespaceT
            "drwxr-xr-x  12 vanessa  staff   384 Dec 26 19:43 _darcs"
            ["drwxr-xr-x","12","vanessa","staff","384","Dec","26","19:43","_darcs"]
        , splitWhitespaceT "      55 ./src/Jacinda/File.hs" ["55", "./src/Jacinda/File.hs"]
        , splitWhitespaceT "" []
        , splitWhitespaceT "5" ["5"]
        , testCase "type of" (tyOfT sumBytes (TyB TyInteger))
        , testCase "type of" (tyOfT krakRegex (TyApp (TyB TyStream) (TyB TyStr))) -- stream of str
        , testCase "type of" (tyOfT krakCol (TyApp (TyB TyStream) (TyB TyStr))) -- stream of str
        , testCase "type of (zip)" (tyOfT ",(-) $3:i $6:i" (tyStream tyI))
        , testCase "type of (filter)" (tyOfT "(>110) #. #\"$0" (tyStream tyI))
        , testCase "typechecks dfn" (tyOfT "[(+)|0 x] $1:i" tyI)
        , testCase "count bytes" (tyOfT "(+)|0 #¨$0" tyI)
        , testCase "running count (lines)" (tyOfT "(+)^0 [:1¨$0" (tyStream tyI))
        , testCase "type of (tally)" (tyOfT "#'hello world'" tyI)
        , testCase "typechecks dfn" (tyFile "test/examples/ab.jac")
        , testCase "parses parens" (tyFile "examples/lib.jac")
        , testCase "typechecks/parses correctly" (tyFile "test/examples/line.jac")
        , testCase "split eval" (evalTo "[x+' '+y]|> split '01-23-1987' /-/" "01 23 1987")
        , testCase "length eval" (evalTo "#*split '01-23-1987' /-/" "3")
        , testCase "captureE" (evalTo "'01-23-1987' ~* 3 /(\\d{2})-(\\d{2})-(\\d{4})/" "Some 1987")
        , testCase "if...then...else" (evalTo "if #t then 0 else 1" "0")
        ]

evalTo :: T.Text -> String -> Assertion
evalTo bsl expected =
    let actual = show (exprEval bsl)
        in actual @?= expected

pAst :: E ()
pAst =
    EApp ()
        (EApp ()
            (BB () Gt)
            (EApp ()
                (UB () Tally)
                (AllField ())))
        (Lit () (ILit 72))

splitWhitespaceT :: BS.ByteString -> [BS.ByteString] -> TestTree
splitWhitespaceT haystack expected =
    testCase "split col" $
        toList (splitBy defaultRurePtr haystack) @?= expected

-- example: ls -l | ja '(+)|0 $5:i'
sumBytes :: T.Text
sumBytes = "(+)|0 $5:i"

krakRegex :: T.Text
krakRegex = "{% /Krakatoa/}{`0}"

krakCol :: T.Text
krakCol = "{`3:i > 4}{`0}"

sumBytesAST :: E ()
sumBytesAST =
    EApp ()
        (EApp ()
            (EApp ()
                (TB () Fold)
                (BB () Plus))
            (Lit () (ILit 0)))
            (IParseCol () 5)

tyFile :: FilePath -> Assertion
tyFile = tcIO [] <=< TIO.readFile

tyOfT :: T.Text -> T -> Assertion
tyOfT src expected =
    tySrc src @?= expected

parseTo :: T.Text -> E () -> Assertion
parseTo src e =
    case rwP . snd <$> parse src of
        Left err     -> assertFailure (show err)
        Right actual -> void (expr actual) @?= e

parseFile :: FilePath -> TestTree
parseFile fp = testCase ("Parses " ++ fp) $ parseNoErr =<< TIO.readFile fp

parseNoErr :: T.Text -> Assertion
parseNoErr src =
    case parse src of
        Left err -> assertFailure (show err)
        Right{}  -> assertBool "success" True
