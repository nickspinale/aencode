{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RankNTypes #-}

module Data.Aencode.Lens
    ( _BString
    , _BInt
    , _BList
    , _BDict
  --
    , _BValue
    , _BValue'
    ) where

import           Data.Aencode

import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as M
import           Data.Profunctor

----------------------------------------
-- LENS CONSTRUCTION
----------------------------------------

-- Directly from Control.Lens.Prism

type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
type Prism' s a = Prism s s a a

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))

_BString :: Prism' (BValue a) a
_BString = prism' BString asString

_BInt :: Prism' (BValue a) Integer
_BInt = prism' BInt asInt

_BList :: Prism' (BValue a) [BValue a]
_BList = prism' BList asList

_BDict :: Prism' (BValue a) (BDict a)
_BDict = prism' BDict asDict

----------------------------------------
-- OTHER PARSER STUFF
----------------------------------------

-- Not very efficient because it toStrict's a bytestring
_BValue :: Prism' B.ByteString (BValue B.ByteString)
_BValue = prism' (L.toStrict . toLazyByteString . buildBValue) (onlyDo parseBValue)

-- Parse exactly a _ (strict)
onlyDo :: Parser a -> B.ByteString -> Maybe a
onlyDo = ((maybeResult . (`feed` B.empty)) .) . parse . (<* endOfInput)

_BValue' :: Prism' L.ByteString (BValue B.ByteString)
_BValue' = prism' (toLazyByteString . buildBValue) (onlyDo' parseBValue)

-- Parse exactly a _ (lazy)
onlyDo' :: Parser a -> L.ByteString -> Maybe a
onlyDo' = (A.maybeResult .) . A.parse . (<* endOfInput)


-- =========================================================
-- Tabled for now:
-- =========================================================

-- ----------------------------------------
-- -- CLASSES
-- ----------------------------------------

-- -- Context-free

-- class ToBencode a where
--     encode :: a -> BValue

-- class FromBencode a where
--     decode :: BValue -> Maybe a

-- _Translated :: (FromBencode a, ToBencode a) => Prism' BValue a
-- _Translated = prism' encode decode

-- -- Not context-free

-- class ToBencode' a where
--     encode' :: c -> a -> BValue

-- class FromBencode' a where
--     decode' :: c -> BValue -> Maybe a

-- -- _Translated' :: (FromBencode' a, ToBencode' a) => c -> Prism' BValue a
-- _Translated' c = prism' (encode' c) (decode' c)
