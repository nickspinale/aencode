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
    , buildBValue
  --
    , _BValue
    , _BValue'
    , onlyDo
    , onlyDo'
  --
    , ToBencode
    , FromBencode
    , _Translated
    , ToBencode'
    , FromBencode'
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
-- WRITERS
----------------------------------------

buildBValue :: BValue -> Builder
buildBValue (BString x) = buildBString x
buildBValue (BInt    x) = buildBInt    x
buildBValue (BList   x) = buildBList   x
buildBValue (BDict   x) = buildBDict   x

buildBString :: B.ByteString -> Builder
buildBString s = intDec (B.length s) <> char8 ':' <> byteString s

buildBInt :: Integer -> Builder
buildBInt = surround 'i' . integerDec

buildBList :: [BValue] -> Builder
buildBList = surround 'l' . mconcat . map buildBValue

buildBDict :: M.Map B.ByteString BValue -> Builder
buildBDict d = surround 'd' $ mconcat
                   [ buildBString k <> buildBValue v
                   | (k, v) <-  M.toAscList d
                   ]

surround :: Char -> Builder -> Builder
surround = (.) (<> char8 'e') . (<>) . char8

----------------------------------------
-- OTHER PARSER STUFF
----------------------------------------

-- Not very efficient because it toStrict's a bytestring
_BValue :: Prism' B.ByteString BValue
_BValue = prism' (L.toStrict . toLazyByteString . buildBValue) (onlyDo parseBValue)

_BValue' :: Prism' L.ByteString BValue
_BValue' = prism' (toLazyByteString . buildBValue) (onlyDo' parseBValue)

-- Parse exactly a _ (strict)
onlyDo :: Parser a -> B.ByteString -> Maybe a
onlyDo = ((maybeResult . (`feed` B.empty)) .) . parse . (<* endOfInput)

-- Parse exactly a _ (lazy)
onlyDo' :: Parser a -> L.ByteString -> Maybe a
onlyDo' = (A.maybeResult .) . A.parse . (<* endOfInput)

----------------------------------------
-- CLASSES
----------------------------------------

-- Context-free

class ToBencode a where
    encode :: a -> BValue

class FromBencode a where
    decode :: BValue -> Maybe a

_Translated :: (FromBencode a, ToBencode a) => Prism' BValue a
_Translated = prism' encode decode

-- Not context-free

class ToBencode' a where
    encode' :: c -> a -> BValue

class FromBencode' a where
    decode' :: c -> BValue -> Maybe a

-- _Translated' :: (FromBencode' a, ToBencode' a) => c -> Prism' BValue a
_Translated' c = prism' (encode' c) (decode' c)

----------------------------------------
-- TEMPLATE HASKELL
----------------------------------------

makePrisms ''BValue
