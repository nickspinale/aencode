{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Aencode
    ( BDict
    , BValue(..)
    , asString
    , asInt
    , asList
    , asDict
  --
    , parseBValue
    , parseBString
    , parseBInt
    , parseBList
    , parseBDict
  --
    , Stringable(..)
    , buildBValue
    , buildBString
    , buildBInt
    , buildBList
    , buildBDict
  --
    , IBuilder
    , prefix
    , prefixed
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

type BDict k v = M.Map k (BValue v)

-- A bencoded value.
data BValue k v = BString v
                | BInt Integer
                | BList [BValue v]
                | BDict (BDict k v)
                deriving Show

instance Functor (BValue k) where
    fmap f (BString k) = f v
    fmap f (BList [b]) = BList $ (fmap.fmap) f b
    fmap f (BDict m) = BDict $ (fmap.fmap) f b
    fmap f int = int

asString :: BValue k v -> Maybe v
asString (BString x) = Just x
asString _ = Nothing

asInt :: BValue k v -> Maybe Integer
asInt (BInt x) = Just x
asInt _ = Nothing

asList :: BValue k v -> Maybe [BValue k v]
asList (BList x) = Just x
asList _ = Nothing

asDict :: BValue k v -> Maybe (BDict k v)
asDict (BDict x) = Just x
asDict _ = Nothing

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

buildBValue :: (Ord k, Stringable k, Stringable v) => BValue k v -> Builder
buildBValue (BString x) = buildBString x
buildBValue (BInt    x) = buildBInt    x
buildBValue (BList   x) = buildBList   x
buildBValue (BDict   x) = buildBDict   x

buildBString :: Stringable v => v -> Builder
buildBString x = integerDec (lengthify x) <> char8 ':' <> builder x

buildBInt :: Integer -> Builder
buildBInt = surround 'i' . integerDec

buildBList :: (Ord k, Stringable k, Stringable v) => [BValue k v] -> Builder
buildBList = surround 'l' . mconcat . map buildBValue

buildBDict :: (Ord k, Stringable k, Stringable v) => BDict k v -> Builder
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

-- This is not a synonym because it SHOULD NOT BE AN INSTANCE OF ORD
data IBuilder = IBuilder (Sum Integer) Builder

instance Monoid IBuilder where
    empty = IBuilder empty empty
    mappend (IBuilder a b) (IBuilder c d) = IBuilder (a <> b) (c <> d)

instance Stringable IBuilder where
    lengthify (IBuilder (Sum n) _) = n
    builder = (IBuilder _ b) = b

----------------------------------------
-- USEFUL FOR IBUILDERS
----------------------------------------

prefix :: FiniteBits a => (a -> Builder) -> a -> IBuilder
prefix f a = (Sum (toInteger $ finiteByteSize a), f a)

prefixed :: Stringable a => a -> IBuilder
prefixed a = (Sum (lengthify a), builder a)

finiteByteSize :: forall a. FiniteBits a => a -> Int
finiteByteSize _ = case r of 0 -> q
                             _ -> q + 1
  where
    (q, r) = finiteBitSize (undefined :: a) `quotRem` 8

