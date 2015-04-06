{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Aencode
    ( BDict

    , BValue(..)
    , getBString
    , getBInt
    , getBList
    , getBDict

    , parseBValue
    , parseBString
    , parseBInt
    , parseBList
    , parseBDict

    , Stringable(..)
    , buildBValue
    , buildBString
    , buildBInt
    , buildBList
    , buildBDict

    , IBuilder
    , prefix
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (char, decimal, signed)
import qualified Data.Attoparsec.ByteString.Lazy as A
import           Data.Bits
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Function
import           Data.Monoid
import qualified Data.Map.Strict as M
import           Prelude hiding (take)

-- Keys are not generalized because bencoded dictionaries must be ordered
-- lexographically by key (and things like IBuilders wouldn't work).
-- I can't think of an instance where efficiency in building keys would
-- be super important (because they're usually short). If it turns out
-- I'm wrong, I'll create a superclss of Stringable, and a newtype
-- (for which the stringable operations would be derives) that
-- instantiates Ord only for types of this class.
type BDict a = M.Map B.ByteString (BValue a)

-- A bencoded value.
data BValue a = BString a
              | BInt Integer
              | BList [BValue a]
              | BDict (BDict a)
              deriving Show

instance Functor BValue where
    fmap f (BString x) = BString $             f x
    fmap _ (BInt    x) = BInt    $               x
    fmap f (BList   x) = BList   $ (fmap.fmap) f x
    fmap f (BDict   x) = BDict   $ (fmap.fmap) f x

getBString :: BValue a -> Maybe a
getBString (BString x) = Just x
getBString _ = Nothing

getBInt :: BValue a -> Maybe Integer
getBInt (BInt x) = Just x
getBInt _ = Nothing

getBList :: BValue a -> Maybe [BValue a]
getBList (BList x) = Just x
getBList _ = Nothing

getBDict :: BValue a -> Maybe (BDict a)
getBDict (BDict x) = Just x
getBDict _ = Nothing

----------------------------------------
-- PARSERS
----------------------------------------

-- Parse a Bencoded value
parseBValue :: Parser (BValue B.ByteString)
parseBValue =  BString <$> parseBString
           <|> BInt    <$> parseBInt
           <|> BList   <$> parseBList
           <|> BDict   <$> parseBDict

parseBString :: Parser B.ByteString
parseBString = decimal <* char ':' >>= take

parseBInt :: Parser Integer
parseBInt = char 'i' *> signed decimal <* char 'e'

parseBList :: Parser [BValue B.ByteString]
parseBList = char 'l' *> many' parseBValue <* char 'e'

parseBDict :: Parser (BDict B.ByteString)
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
-- BUILDERS
----------------------------------------

buildBValue :: Stringable a => BValue a -> Builder
buildBValue (BString x) = buildBString x
buildBValue (BInt    x) = buildBInt    x
buildBValue (BList   x) = buildBList   x
buildBValue (BDict   x) = buildBDict   x

buildBString :: Stringable a => a -> Builder
buildBString x = integerDec (lengthify x) <> char8 ':' <> builder x

buildBInt :: Integer -> Builder
buildBInt = surround 'i' . integerDec

buildBList :: Stringable a => [BValue a] -> Builder
buildBList = surround 'l' . mconcat . map buildBValue

buildBDict :: Stringable a => BDict a -> Builder
buildBDict x = surround 'd' $ mconcat [ buildBString k <> buildBValue v
                                      | (k, v) <- M.toAscList x
                                      ]

surround :: Char -> Builder -> Builder
surround = (.) (<> char8 'e') . mappend . char8

----------------------------------------
-- STRINGABLE
----------------------------------------

class Stringable a where
    lengthify :: a -> Integer
    builder :: a -> Builder

instance Stringable B.ByteString where
    lengthify = toInteger . B.length
    builder = byteString

instance Stringable L.ByteString where
    lengthify = toInteger . L.length
    builder = lazyByteString

type IBuilder = (Sum Integer, Builder)

instance Stringable (Sum Integer, Builder) where
    lengthify = getSum . fst
    builder = snd

prefix :: Stringable a => a -> IBuilder
prefix a = (Sum (lengthify a), builder a)

