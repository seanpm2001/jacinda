{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           A
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
        [ testCase "parse as" (parseTo sumBytes sumBytesAST)
        , splitWhitespaceT "1 1.3\tj" ["1", "1.3", "j"]
        , splitWhitespaceT
            "drwxr-xr-x  12 vanessa  staff   384 Dec 26 19:43 _darcs"
            ["drwxr-xr-x","12","vanessa","staff","384","Dec","26","19:43","_darcs"]
        , splitWhitespaceT "      55 ./src/Jacinda/File.hs" ["55", "./src/Jacinda/File.hs"]
        , testCase "subs" $
            let actual = subs (compileDefault "zi") "vectorzm0zi13zi1zi0zmc80ea02f780be2984f831df2de071f6e6040c0f670b3dd2428e80f5d111d7f72_DataziVectorziGeneric_partition_closure" "."
            in actual @?= "vectorzm0.13.1.0zmc80ea02f780be2984f831df2de071f6e6040c0f670b3dd2428e80f5d111d7f72_Data.Vector.Generic_partition_closure"
        , splitWhitespaceT "" []
        , splitWhitespaceT "5" ["5"]
        , testCase "typechecks dfn" (tyOfT "[(+)|0 x] $1:i" tyI)
        , testCase "typechecks dfn" (tyFile "test/examples/ab.jac")
        , testCase "split eval" (evalTo "[x+' '+y]|> split '01-23-1987' /-/" "01 23 1987")
        , testCase "length eval" (evalTo "#*split '01-23-1987' /-/" "3")
        , testCase "captureE" (evalTo "'01-23-1987' ~* 3 /(\\d{2})-(\\d{2})-(\\d{4})/" "Some 1987")
        , testCase "conditional" (evalTo "if #t then 0 else 1" "0")
        ]

evalTo :: T.Text -> String -> Assertion
evalTo bsl expected =
    let actual = show (exprEval bsl)
        in actual @?= expected

splitWhitespaceT :: BS.ByteString -> [BS.ByteString] -> TestTree
splitWhitespaceT haystack expected =
    testCase "split col" $
        toList (splitBy defaultRurePtr haystack) @?= expected

-- example: ls -l | ja '(+)|0 $5:i'
sumBytes :: T.Text
sumBytes = "(+)|0 $5:i"

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
tyFile fp = tcIO [] fp =<< TIO.readFile fp

tyOfT :: T.Text -> T -> Assertion
tyOfT src expected =
    tySrc src @?= expected

parseTo :: T.Text -> E () -> Assertion
parseTo src e =
    case rwP . snd <$> parse src of
        Left err     -> assertFailure (show err)
        Right actual -> void (expr actual) @?= e
