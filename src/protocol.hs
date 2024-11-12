-- actual implementation (janky) of the protocol
-- we are only considering single file torrents for now, 
-- if time permits we will try multi-file torrents
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant bracket" #-}

import Bencode
import Crypto.Hash (Digest, SHA1, hash)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import Data.ByteString (ByteString)
import Numeric (showHex)
import Data.Char (intToDigit, ord)
import Data.Word (Word8)


-- !Auxiliary Functions
return_keys :: [(String, Bencode)] -> [String]
return_keys = map fst

(!!!) :: [(String, Bencode)]  -> String -> Bencode
(!!!) dict key = case dict of
                        (x:xs) -> if fst x== key then snd x
                                  else (!!!) xs key
                        [] -> BList []

splitToBytes :: ByteString -> [ByteString]
splitToBytes = map BC.singleton . BC.unpack

sha_hash :: ByteString -> Digest SHA1
sha_hash = hash


-- get_urlencode :: Digest SHA1 -> String
-- get_urlencode digest = concatMap (('%':) . flip showHex "") (BC.unpack $ B16.encode $ BC.pack $ show digest)

main :: IO()
main = do
    res <- parse_file "big-buck-bunny.torrent"
    let BDict torrent_dict = head res
    let info_dict = torrent_dict !!! "info"
    let to_hash = BC.pack $ bencode_deparser info_dict
    let digest = sha_hash to_hash
    print (digest)





