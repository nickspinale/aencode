module Test () where

import           Control.Lens
import           Data.Aencode
import           Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

test1 = (^? _BValue) `fmap` B.readFile "examples/slackware64-14.1-install-dvd.torrent"

test2 = (^? _BValue') `fmap` L.readFile "examples/slackware64-14.1-install-dvd.torrent"

