{-# LANGUAGE TemplateHaskell #-}

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
    , prefixed

    , _BValue
    , _BValue'
  --
    , ToBencode
    , FromBencode
    , _Translated
  --
    , ToBencode'
    , FromBencode'
    , _Translated'
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (char, decimal, signed)
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Function
import           Data.Monoid
import qualified Data.Map.Strict as M
import           Data.Wordplay
import           Prelude hiding (take)

type BDict a = M.Map a (BValue a)

-- A bencoded value.
data BValue a = BString a
              | BInt Integer
              | BList [BValue a]
              | BDict (BDict a)
              deriving Show

instance Functor BValue where
    fmap f (BString x) = BString $ f x
    fmap f (BList   x) = BList $ (fmap.fmap) f x
    fmap f (BDict   x) = BDict . (fmap.fmap) f $ mapKeys f x
    fmap _ i = i

asString :: BValue a -> Maybe a
asString (BString x) = Just x
asString _ = Nothing

asInt :: BValue a -> Maybe Int
asInt (BInt x) = Just x
asInt _ = Nothing

asList :: BValue a -> Maybe [BValue a]
asList (BList x) = Just x
asList _ = Nothing

asDict :: BValue a -> Maybe (BDict a)
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
-- WRITERS
----------------------------------------

class (Monoid a, Lengthable a) => Stringable a where
    char8' :: Char -> a
    intDec' :: Int -> a
    integerDec' :: Integer -> a

instance Stringable a => Foldable a where
    foldMap f (BString x) = let y = f x in intDec' (lengthify y) <> char8' ':' <> y
    foldMap _ (BInt    x) = surround 'i' $ integerDec' x
    foldMap f (BList   x) = surround 'l' $ mconcat $ map (foldMap f) x
    foldMap f (BDict   x) = surround 'd' $ mconcat [ (foldMap f) k <> (foldMap f) v
                                                   | (k, v) <- M.toAscList x
                                                   ]

surround :: Stringable => Char -> a -> a
surround = (.) (<> char8' 'e') . mappend . char8'

----------------------------------------
-- STRINGABLE
----------------------------------------

instance Stringable B.ByteString where
    char8' = C.singleton
    intDec' = C.pack . show
    integerDec' = C.pack . show

instance Stringable L.ByteString where
    char8' = C.singleton
    intDec' = LC.pack . show
    integerDec' = LC.pack . show

type IBuilder = (Sum Int, Builder)

instance Stringable IBuilder where
    buildBString (Sum i, b) = intDec i <> char8 ':' <> d

----------------------------------------
-- FOR LENGTHABLES
----------------------------------------

prefixed :: Lengthable a => (a -> Builder) -> a -> IBuilder
prefixed f a = (Sum (lengthify a), f a)

instance Lengthabe B.ByteString where
    lengthify = B.length

instance Lengthabe L.ByteString where
    lengthify = L.length

----------------------------------------
-- OTHER PARSER STUFF
----------------------------------------

-- Not very efficient because it toStrict's a bytestring
_BValue :: Prism' B.ByteString BValue
_BValue = prism' (L.toStrict . toLazyByteString . buildBValue) (onlyDo parseBValue)

-- Parse exactly a _ (strict)
onlyDo :: Parser a -> B.ByteString -> Maybe a
onlyDo = ((maybeResult . (`feed` B.empty)) .) . parse . (<* endOfInput)

_BValue' :: Prism' L.ByteString BValue
_BValue' = prism' (toLazyByteString . buildBValue) (onlyDo' parseBValue)

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
