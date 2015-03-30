module Test () where

import           Control.Lens
import           Data.Aencode
import           Data.Attoparsec.ByteString
import qualified Data.ByteString as B

test = (^? _BValue) `fmap` B.readFile "examples/slackware64-14.1-install-dvd.torrent"

