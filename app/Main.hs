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
import Data.Char
import Data.Char (intToDigit, ord, chr)
import Data.Word (Word8)
import Network.URI.Encode
import Network.HTTP.Base
import Network.Wreq
import Control.Lens



-- !Auxiliary Functions
return_keys :: [(a, b)] -> [a]
return_keys = map fst

(!!!) :: [(ByteString, a)]  -> String -> [a]
(!!!) dict key = case dict of
                        (x:xs) -> if fst x== (BC.pack key) then [snd x]
                                  else (!!!) xs key
                        [] -> []

(!!!!) :: [(String, a)]  -> String -> [a]
(!!!!) dict key = case dict of
                        (x:xs) -> if fst x== key then [snd x]
                                  else (!!!!) xs key
                        [] -> []

sha_hash :: ByteString -> Digest SHA1
sha_hash = hash

url_encode_sha_hash :: String -> String
url_encode_sha_hash hash = case hash of 
                    (x:y:xs) -> ('%' : (toUpper x) : (toUpper y) : []) ++ (url_encode_sha_hash xs) 
                    [] -> ""-- get_urlencode :: Digest SHA1 -> String


form_announce_url :: String -> IO ([(String, String)])
form_announce_url fname = do
    res <- parse_file fname
    let BDict torrent_dict = head res
    let info_bencode = head $ torrent_dict !!! "info"
    let BDict info_dict = info_bencode
    let info_hash = show $ sha_hash $ bencode_deparser info_bencode
    let url_encoded_hash = url_encode_sha_hash $ info_hash
    let peer_id = url_encode_sha_hash $ show $ sha_hash $ BC.pack $ "juggernautjha"
    let BBString tracker = head $ torrent_dict !!! "announce"
    pure [("url", BC.unpack tracker),("attribs" , (urlEncodeVars $ [("info_hash", url_encoded_hash) , ("peer_id" , peer_id), ("port" , "2000"), ("uploaded", "0"), 
          ("downloaded", "0"), ("event" , "started")]))]
    
ping_from_dict :: [(String, String)] -> String
ping_from_dict dict = let (url, attribs) = (head $ dict !!!! "url" ,head $ dict !!!! "attribs")
                      in url ++ "?" ++ attribs
--3f9aac158c7de8dfcab171ea58a17aabdf7fbc93
run :: String -> IO([(ByteString, Bencode)])
run fname = do
    res <- parse_file fname
    let BDict torrent_dict = head res
    pure torrent_dict
    -- let info_dict = torrent_dict !!! "info"
    -- let to_hash = BC.pack $ bencode_deparser info_dict
    -- let digest = sha_hash to_hash
    -- print (BC.pack $ show digest)
    -- -- print (splitToBytes $ BC.pack $ show digest)

    -- pure (show $ digest)






-- Convert a String to ByteString
stringToByteString :: String -> BC.ByteString
stringToByteString = BC.pack

main :: IO ()
main = do
    let str = "Hello, World!"
    let byteString = stringToByteString str
    print byteString