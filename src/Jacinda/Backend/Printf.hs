{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Backend.Printf ( sprintf
                              ) where

import           A
import qualified Data.ByteString                   as BS
import           Data.ByteString.Builder           (toLazyByteString)
import           Data.ByteString.Builder.RealFloat (doubleDec)
import qualified Data.ByteString.Lazy              as BSL
import           Data.Char                         (isDigit)
import qualified Data.Text                         as T
import           Data.Text.Encoding                (decodeUtf8, encodeUtf8)

sprintf :: BS.ByteString -- ^ Format string
        -> E a
        -> BS.ByteString
sprintf fmt e = encodeUtf8 (sprintf' (decodeUtf8 fmt) e)

pf :: Double -> T.Text
pf = decodeUtf8 . BSL.toStrict . toLazyByteString . doubleDec

pd n x = let (l,r) = T.break (=='.') (pf x) in l <> T.take (n+1) r

data PF = D !(Maybe Int) | I | B | S

{-# SCC next #-}
next :: T.Text -> (T.Text, Maybe (PF, T.Text))
next inp = case T.break (=='%') inp of
    (p, mfmt) | "%i" `T.isPrefixOf` mfmt -> (p, Just (I, T.drop 2 mfmt))
              | "%s" `T.isPrefixOf` mfmt -> (p, Just (S, T.drop 2 mfmt))
              | "%b" `T.isPrefixOf` mfmt -> (p, Just (B, T.drop 2 mfmt))
              | "%f." `T.isPrefixOf` mfmt -> let s=T.drop 3 mfmt in
                    case precision s of
                        Nothing    -> error "Missing digit in format specifier."
                        Just (n,f) -> (p, Just (D (Just n), f))
              | "%f" `T.isPrefixOf` mfmt -> (p, Just (D Nothing, T.drop 2 mfmt))
              | otherwise -> (p, Nothing)

precision :: T.Text -> Maybe (Int, T.Text)
precision t =
    let (n,f) = T.span isDigit t  in
    if T.null n then Nothing else Just (read (T.unpack n), f)

{-# SCC sp #-}
sp :: T.Text -> [E a] -> T.Text
sp t [] = t
sp t (e:es) =
    case (next t, e) of
        ((_, Nothing), _)                               -> error "Argument to sprintf has more values than expected by format string."
        ((p, Just (D Nothing, fmt)), Lit _ (FLit f))  -> p <> pf f <> sp fmt es
        ((p, Just (D (Just n), fmt)), Lit _ (FLit f)) -> p <> pd n f <> sp fmt es
        ((p, Just (B, fmt)), Lit _ (BLit b))            -> p <> showBool b <> sp fmt es
        ((p, Just (I, fmt)), Lit _ (ILit i))            -> p <> T.pack (show i) <> sp fmt es
        ((p, Just (S, fmt)), Lit _ (StrLit bs))         -> p <> decodeUtf8 bs <> sp fmt es
        ((_, Just{}), _)                                -> error "Argument type does not match sprintf format string."

  where
    showBool True = "#t"; showBool False = "#f"

sprintf' :: T.Text -> E a -> T.Text
sprintf' fmt e@Lit{}    = sp fmt [e]
sprintf' fmt (Tup _ es) = sp fmt es
