module Test () where

import           Data.Aencode
import           Data.Attoparsec.ByteString
import qualified Data.ByteString as B

test = do
    f <- B.readFile "examples/slackware64-14.1-install-dvd.torrent"
    case parseOnly parseBValue' f of
        Left str -> print str
        Right b -> print b
