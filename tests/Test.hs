module Test () where

import           Data.Aencode
import           Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L

test = do
    x <- B.readFile "examples/slackware64-14.1-install-dvd.torrent"
    case parseOnly parseBDict x of
        Left str -> print str
        Right y -> print $ x == L.toStrict (toLazyByteString (buildBDict y))
