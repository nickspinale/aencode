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
