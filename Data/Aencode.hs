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
    , mapStrings
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

type BDict a = M.Map a (BValue a)

-- A bencoded value.
data BValue a = BString a
              | BInt Integer
              | BList [BValue a]
              | BDict (BDict a)
              deriving Show

asString :: BValue a -> Maybe a
asString (BString x) = Just x
asString _ = Nothing

asInt :: BValue a -> Maybe Integer
asInt (BInt x) = Just x
asInt _ = Nothing

asList :: BValue a -> Maybe [BValue a]
asList (BList x) = Just x
asList _ = Nothing

asDict :: BValue a -> Maybe (BDict a)
asDict (BDict x) = Just x
asDict _ = Nothing

----------------------------------------
-- FUNCTOR-LIKE
----------------------------------------

-- Not functor because of Ord restraint
mapStrings :: Ord b => (a -> b) -> BValue a -> BValue b
mapStrings f (BString x) = BString $ f x
mapStrings _ (BInt    x) = BInt x
mapStrings f (BList   x) = BList $ (fmap.mapStrings) f x
mapStrings f (BDict   x) = BDict . (fmap.mapStrings) f $ M.mapKeys f x

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

----------------------------------------
-- USEFUL FOR IBUILDERS
----------------------------------------

prefix :: Stringable a => a -> IBuilder
prefix a = (Sum (lengthify a), builder a)

prefixed :: FiniteBits a => (a -> Builder) -> a -> IBuilder
prefixed f a = (Sum (toInteger $ finiteByteSize a), f a)

finiteByteSize :: forall a. FiniteBits a => a -> Int
finiteByteSize _ = case r of 0 -> q
                             _ -> q + 1
  where
    (q, r) = finiteBitSize (undefined :: a) `quotRem` 8

