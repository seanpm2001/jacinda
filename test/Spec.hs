{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           A
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable        (toList)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           File
import           Jacinda.Regex
import           System.IO.Temp       (withSystemTempFile)
import           Test.Tasty
import           Test.Tasty.Golden    (goldenVsString)
import           Test.Tasty.HUnit

harness :: String -- ^ Source file name
        -> Mode
        -> FilePath -- ^ Input
        -> FilePath -- ^ Golden output
        -> TestTree
harness src m fp o =
    goldenVsString src o $ do
        t <- TIO.readFile src
        withSystemTempFile "JAC_TEST" $ \oϵ h -> do
            runOnFile [] src t [] m fp h
            _ <- BS.hGetContents h
            BSL.readFile oϵ

main :: IO ()
main = defaultMain $
    testGroup "stream" [
        harness "examples/otool/rpath.jac" (AWK Nothing Nothing) "test/data/otool" "test/golden/rpath.out"
      , harness "examples/otool/dllibs.jac" (AWK Nothing Nothing) "test/data/otool" "test/golden/ldlib.out"
      , harness "test/examples/ghc-filt.jac" (AWK Nothing Nothing) "test/data/ghc" "test/golden/ghc.out"
      , testGroup "eval"
          [ splitWhitespaceT "1 1.3\tj" ["1", "1.3", "j"]
          , splitWhitespaceT
              "drwxr-xr-x  12 vanessa  staff   384 Dec 26 19:43 _darcs"
              ["drwxr-xr-x","12","vanessa","staff","384","Dec","26","19:43","_darcs"]
          , splitWhitespaceT "      55 ./src/Jacinda/File.hs" ["55", "./src/Jacinda/File.hs"]
          , testCase "subs" $
              let actual = subs (compileDefault "zi") "vectorzm0zi13zi1zi0zmc80ea02f780be2984f831df2de071f6e6040c0f670b3dd2428e80f5d111d7f72_DataziVectorziGeneric_partition_closure" "."
              in actual @?= "vectorzm0.13.1.0zmc80ea02f780be2984f831df2de071f6e6040c0f670b3dd2428e80f5d111d7f72_Data.Vector.Generic_partition_closure"
          , splitWhitespaceT "" []
          , splitWhitespaceT "5" ["5"]
          , testCase "split eval" (evalTo "[x+' '+y]|> split '01-23-1987' /-/" "01 23 1987")
          , testCase "length eval" (evalTo "#*split '01-23-1987' /-/" "3")
          , testCase "captureE" (evalTo "'01-23-1987' ~* 3 /(\\d{2})-(\\d{2})-(\\d{4})/" "Some 1987")
          , testCase "conditional" (evalTo "if #t then 0 else 1" "0")
          ]
    ]

evalTo :: T.Text -> String -> Assertion
evalTo bsl expected =
    let actual = show (exprEval bsl)
        in actual @?= expected

splitWhitespaceT :: BS.ByteString -> [BS.ByteString] -> TestTree
splitWhitespaceT haystack expected =
    testCase "split col" $
        toList (splitBy defaultRurePtr haystack) @?= expected
