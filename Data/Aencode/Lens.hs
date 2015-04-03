module Data.Aencode.Lens
    ( _BString
    , _BInt
    , _BList
    , _BDict
    ) where

import           Data.Aencode

import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as M

type Prism = forall p f. (Choice p, Applicative f) => p a (f b) -> p a (f t)

prism :: (Choice p, Applicative (b -> s) -> (s -> Either t a) -> Prism s t a p

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
