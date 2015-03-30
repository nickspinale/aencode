module Test () where

import           Data.Aencode
import qualified Data.ByteString as B

test = do
    f <- B.readFile "slackware64-14.1-install-dvd.torrent"
    case parseOnly parseBValue' f of
        Left str -> print str
        Right b -> print b
