{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Backend.Printf ( sprintf
                              ) where

import           A
import qualified Data.ByteString                   as BS
import           Data.ByteString.Builder           (toLazyByteString)
import           Data.ByteString.Builder.RealFloat (doubleDec)
import qualified Data.ByteString.Lazy              as BSL
import qualified Data.Text                         as T
import           Data.Text.Encoding                (decodeUtf8, encodeUtf8)

sprintf :: BS.ByteString -- ^ Format string
        -> E a
        -> BS.ByteString
sprintf fmt e = encodeUtf8 (sprintf' (decodeUtf8 fmt) e)

pf :: Double -> T.Text
pf = decodeUtf8 . BSL.toStrict . toLazyByteString . doubleDec

data PF = D !(Maybe Int) | I | B | S

{-# SCC next #-}
next :: T.Text -> (T.Text, Maybe (PF, T.Text))
next inp = case T.break (=='%') inp of
    (p, mfmt) | "%i" `T.isPrefixOf` mfmt -> (p, Just (I, T.drop 2 mfmt))
              | "%s" `T.isPrefixOf` mfmt -> (p, Just (S, T.drop 2 mfmt))
              | "%b" `T.isPrefixOf` mfmt -> (p, Just (B, T.drop 2 mfmt))
              | "%f." `T.isPrefixOf` mfmt -> (p, Just (D (Just undefined), undefined))
              | "%f" `T.isPrefixOf` mfmt -> (p, Just (D Nothing, T.drop 2 mfmt))
              | otherwise -> (p, Nothing)

{-# SCC sp #-}
sp :: T.Text -> [E a] -> T.Text
sp t [] = t
sp t (e:es) =
    case (next t, e) of
        ((_, Nothing), _) -> error "Argument to sprintf has more values than expected by format string."
        ((p, Just ((D Nothing), fmt)), Lit _ (FLit f)) -> p <> pf f <> sp fmt es
        ((p, Just (B, fmt)), Lit _ (BLit b)) -> p <> showBool b <> sp fmt es
        ((p, Just (I, fmt)), Lit _ (ILit i)) -> p <> T.pack (show i) <> sp fmt es
        ((p, Just (S, fmt)), Lit _ (StrLit bs)) -> p <> decodeUtf8 bs <> sp fmt es
        ((_, Just{}), _) -> error "Argument type does not match sprintf format string."

  where
    showBool True = "#t"; showBool False = "#f"

sprintf' :: T.Text -> E a -> T.Text
sprintf' fmt e@Lit{} = sp fmt [e]
sprintf' fmt (Tup _ es) = sp fmt es
