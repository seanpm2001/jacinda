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
import           System.IO            (hClose)
import           System.IO.Temp       (withSystemTempFile)
import           Test.Tasty
import           Test.Tasty.Golden    (goldenVsString)
import           Test.Tasty.HUnit

eio :: FilePath -- ^ Source filename
    -> T.Text
    -> Mode
    -> FilePath -- ^ Input
    -> IO BSL.ByteString
eio src e m fp =
    withSystemTempFile "JAC_TEST" $ \oϵ h -> do
        runOnFile [] src e [] m fp h *> hClose h
        BSL.readFile oϵ

harnessF e m fp o = goldenVsString (T.unpack e) o $ eio undefined e m fp

harness :: FilePath -- ^ Source file
        -> Mode
        -> FilePath -- ^ Input file
        -> FilePath -- ^ Expected output
        -> TestTree
harness src m fp o =
    goldenVsString src o $ do {t <- TIO.readFile src; eio src t m fp}

main :: IO ()
main = defaultMain $
    testGroup "ja" [
        testGroup "stream"
          [ harness "examples/otool/rpath.jac" awk "test/data/otool" "test/golden/rpath.out"
          , harness "examples/otool/dllibs.jac" awk "test/data/otool" "test/golden/ldlib.out"
          , harness "test/examples/ghc-filt.jac" awk "test/data/ghc" "test/golden/ghc.out"
          , harness "examples/latestCabal.jac" awk "test/data/cabal-info" "test/golden/cabal-info.out"
          , harnessF ".?{|`1 ~* 1 /([^\\?]*)/}" awk "test/data/url" "test/golden/url.out"
          , harnessF "[x+' '+y]|>(sprintf'-L%s')¨.?{|`1 ~* 1 /([^']*site-packages)/}" awk "test/data/python-site" "test/golden/linker-flags.out"
          , harnessF "{%/hs-source-dirs/}{`2}" (AWK (Just "\\s*:\\s*") Nothing False) "jacinda.cabal" "test/golden/src-dirs.out"
          ]
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
