{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant bracket" #-}

import Bencode
import Tracker
import Crypto.Hash (Digest, SHA1, hash)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import Data.ByteString (ByteString)
import Numeric (showHex)
import Data.Char
import Data.Char (intToDigit, ord, chr)
import Data.Word (Word8)
import Network.URI.Encode
import Network.HTTP.Base
import Network.Wreq
import Control.Lens
import Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.ByteString (ByteString, unpack)
import Data.Word (Word16)
-- import Network.Socket (inet_ntoa, tupleToHostAddress)
import Data.Bits (shiftL, (.|.))
