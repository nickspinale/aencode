{-# LANGUAGE TemplateHaskell #-}

module Data.Aencode
    (
      BValue(..)
    , _BString
    , _BInt
    , _BList
    , _BDict
  --
    , parseBValue
    , onlyDo
    , onlyDo'
  --
    , writeBValue
  --
    , ToBencode
    , FromBencode
    , ToBencode'
    , FromBencode'
    , _BValue
    , _Translated
    , _Translated'
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (char, decimal, signed)
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import           Data.Monoid
import           Data.Function
import qualified Data.Map as M
import           Prelude hiding (take)

-- A bencoded value.
data BValue = BString B.ByteString
            | BInt Integer
            | BList [BValue]
            | BDict (M.Map B.ByteString BValue)
            deriving Show

----------------------------------------
-- PARSERS
----------------------------------------

-- Parse a Bencoded value
parseBValue :: Parser BValue
parseBValue =  BString <$> parseBString
           <|> BInt    <$> parseBInt
           <|> BList   <$> parseBList
           <|> BDict   <$> parseBDict

parseBString :: Parser B.ByteString
parseBString = decimal <* char ':' >>= take

parseBInt :: Parser Integer
parseBInt = char 'i' *> signed decimal <* char 'e'

parseBList :: Parser [BValue]
parseBList = char 'l' *> many' parseBValue <* char 'e'

parseBDict :: Parser (M.Map B.ByteString BValue)
parseBDict = char 'd' *> inner <* char 'e'
  where
    inner = do
        pairs <- many' $ (,) <$> parseBString <*> parseBValue
        if sorted pairs
         then return (M.fromAscList pairs)
         else empty
    sorted x@(_:y) = all (== LT) $ zipWith (compare `on` fst) x y
    sorted _ = True

----------------------------------------
-- OTHER PARSER STUFF
----------------------------------------

-- Not very efficient because toStrict's a bytestring
_BValue :: Prism' B.ByteString BValue
_BValue = prism' (L.toStrict . toLazyByteString . writeBValue) (onlyDo parseBValue)

_BValue' :: Prism' L.ByteString BValue
_BValue' = prism' (toLazyByteString . writeBValue) (onlyDo' parseBValue)

-- Parse exactly a _ (strict)
onlyDo :: Parser a -> B.ByteString -> Maybe a
onlyDo = ((maybeResult . (`feed` B.empty)) .) . parse . (<* endOfInput)

-- Parse exactly a _ (lazy)
onlyDo' :: Parser a -> L.ByteString -> Maybe a
onlyDo' = (A.maybeResult .) . A.parse . (<* endOfInput)

----------------------------------------
-- WRITERS
----------------------------------------

writeBValue :: BValue -> Builder
writeBValue (BString x) = writeBString x
writeBValue (BInt    x) = writeBInt    x
writeBValue (BList   x) = writeBList   x
writeBValue (BDict   x) = writeBDict   x

writeBString :: B.ByteString -> Builder
writeBString s = intDec (B.length s) <> char8 ':' <> byteString s

writeBInt :: Integer -> Builder
writeBInt = surround 'i' . integerDec

writeBList :: [BValue] -> Builder
writeBList = surround 'l' . mconcat . map writeBValue

writeBDict :: M.Map B.ByteString BValue -> Builder
writeBDict d = surround 'd' $ mconcat
                   [ writeBString k <> writeBValue v
                   | (k, v) <-  M.toAscList d
                   ]

surround :: Char -> Builder -> Builder
surround = (.) (<> char8 'e') . (<>) . char8

----------------------------------------
-- CLASSES
----------------------------------------

class ToBencode a where
    encode :: a -> BValue

class FromBencode a where
    decode :: BValue -> Maybe a

class ToBencode' a where
    encode' :: c -> a -> BValue

class FromBencode' a where
    decode' :: c -> BValue -> Maybe a

_Translated :: (FromBencode a, ToBencode a) => Prism' BValue a
_Translated = prism' encode decode

-- I'm not sure how to appropriately qualify c in this type signature,
-- but GHC can figure it out.
-- _Translated' :: (FromBencode' a, ToBencode' a) => c -> Prism' BValue a
_Translated' c = prism' (encode' c) (decode' c)

----------------------------------------
-- TEMPLATE HASKELL
----------------------------------------

makePrisms ''BValue
