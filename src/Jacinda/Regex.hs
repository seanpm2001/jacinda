{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Regex ( lazySplit
                     , splitBy
                     , defaultRurePtr
                     , isMatch'
                     , find'
                     , compileDefault
                     , substr
                     , findCapture
                     , captures'
                     ) where

import           Control.Exception        (Exception, throwIO)
import           Control.Monad            ((<=<))
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy     as BSL
import           Data.List                (unsnoc)
import qualified Data.Vector              as V
import           Foreign.C.Types          (CSize)
import           Foreign.ForeignPtr       (plusForeignPtr)
import           Regex.Rure               (RureFlags, RureMatch (..), RurePtr, captures, compile, find, findCaptures, isMatch, matches', rureDefaultFlags, rureFlagDotNL)
import           System.IO.Unsafe         (unsafeDupablePerformIO, unsafePerformIO)

-- https://docs.rs/regex/latest/regex/#perl-character-classes-unicode-friendly
defaultFs :: BS.ByteString
defaultFs = "\\s+"

{-# NOINLINE defaultRurePtr #-}
defaultRurePtr :: RurePtr
defaultRurePtr = unsafePerformIO $ yIO =<< compile genFlags defaultFs

genFlags :: RureFlags
genFlags = rureDefaultFlags <> rureFlagDotNL -- in case they want to use a custom record separator

substr :: BS.ByteString -> Int -> Int -> BS.ByteString
substr (BS.BS fp l) begin endϵ | endϵ >= begin = BS.BS (fp `plusForeignPtr` begin) (min l endϵ-begin)
                               | otherwise = "error: invalid substring indices."

captures' :: RurePtr -> BS.ByteString -> CSize -> [BS.ByteString]
captures' re haystack@(BS.BS fp _) ix = unsafeDupablePerformIO $ fmap go <$> captures re haystack ix
    where go (RureMatch s e) =
            let e' = fromIntegral e
                s' = fromIntegral s
                in BS.BS (fp `plusForeignPtr` s') (e'-s')

{-# NOINLINE findCapture #-}
findCapture :: RurePtr -> BS.ByteString -> CSize -> Maybe BS.ByteString
findCapture re haystack@(BS.BS fp _) ix = unsafeDupablePerformIO $ fmap go <$> findCaptures re haystack ix 0
    where go (RureMatch s e) =
            let e' = fromIntegral e
                s' = fromIntegral s
                in BS.BS (fp `plusForeignPtr` s') (e'-s')

{-# NOINLINE find' #-}
find' :: RurePtr -> BS.ByteString -> Maybe RureMatch
find' re str = unsafeDupablePerformIO $ find re str 0

lazySplit :: RurePtr -> BSL.ByteString -> [BS.ByteString]
lazySplit rp bs = let c=BSL.toChunks bs in go Nothing c
        where go Nothing []      = []
              go Nothing (c:cs)  = let ss=splitByA rp c
                    in case unsnoc ss of
                        Just (iss,lss) -> iss++go (Just lss) cs
                        Nothing        -> go Nothing cs
              go (Just c) []     = let ss=splitByA rp c in ss
              go (Just e) (c:cs) = let ss=splitByA rp (e<>c)
                    in case unsnoc ss of
                        Just (iss,lss) -> iss++go (Just lss) cs
                        Nothing        -> go Nothing cs

splitBy :: RurePtr -> BS.ByteString -> V.Vector BS.ByteString
splitBy = (V.fromList .) . splitByA

{-# NOINLINE splitBy #-}
splitByA :: RurePtr
        -> BS.ByteString
        -> [BS.ByteString]
splitByA _ "" = mempty
splitByA re haystack@(BS.BS fp l) =
    [BS.BS (fp `plusForeignPtr` s) (e-s) | (s, e) <- slicePairs]
    where ixes = unsafeDupablePerformIO $ matches' re haystack
          slicePairs = case ixes of
                (RureMatch 0 i:rms) -> mkMiddle (fromIntegral i) rms
                rms                 -> mkMiddle 0 rms
          mkMiddle begin' []        = [(begin', l)]
          mkMiddle begin' (rm0:rms) = (begin', fromIntegral (start rm0)) : mkMiddle (fromIntegral $ end rm0) rms

isMatch' :: RurePtr
         -> BS.ByteString
         -> Bool
isMatch' re haystack = unsafeDupablePerformIO $ isMatch re haystack 0

compileDefault :: BS.ByteString -> RurePtr
compileDefault = unsafeDupablePerformIO . (yIO <=< compile genFlags)

newtype RureExe = RegexCompile String deriving (Show)

instance Exception RureExe where

yIO :: Either String a -> IO a
yIO = either (throwIO . RegexCompile) pure
