-- actual implementation (janky) of the protocol
-- we are only considering single file torrents for now, 
-- if time permits we will try multi-file torrents
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant bracket" #-}


module Tracker where 
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
import Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.ByteString (ByteString, unpack)
import Data.Word (Word16)
-- import Network.Socket (inet_ntoa, tupleToHostAddress)
import Data.Bits (shiftL, (.|.))




-- Function to extract the peers from the binary data
extract_peers :: ByteString -> [([Word8], Word16)]
extract_peers bs = go bs []
  where
    -- Process the ByteString in chunks of 6 bytes (4 for IP, 2 for Port)
    go :: ByteString -> [([Word8], Word16)] -> [([Word8], Word16)]
    go bs peers
        | BS.length bs < 6 = reverse peers  -- End of recursion when no more peers are available
        | otherwise =
            let (ipBytes, rest) = BS.splitAt 4 bs
                (portBytes, rest') = BS.splitAt 2 rest
                ip =  (BS.unpack ipBytes)
                port = (fromIntegral (BS.index portBytes 0) `shiftL` 8) .|. fromIntegral (BS.index portBytes 1)  -- combine 2 bytes to form a port
            in go rest' ((ip, port) : peers)

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
    pure [("url", BC.unpack tracker), ("info_hash", url_encoded_hash) , ("peer_id" , peer_id), ("port" , "2000"), ("uploaded", "0"), 
          ("downloaded", "0"), ("event" , "started")]
    

replace :: Eq a => [(a, b)] -> a -> b -> [(a,b)]
replace dct key newval = case dct of 
                         (x:xs) -> if fst x == key then (key, newval) : xs
                                   else x : (replace xs key newval)
                         _ -> []


default_announce_uri :: String
default_announce_uri = "http://tracker.renfei.net:8080/announce" -- ! some urls have problems with TLS versions, this doesn't. 


-- ! pattern matching is so fun I keep making redundant functions. 
encode_uri :: [(String, String)] -> String
encode_uri dict = case dict of 
                  (x: xs) -> case xs of 
                        [] -> fst x ++ "=" ++ snd x
                        _ -> fst x ++ "=" ++ snd x ++ "&" ++ (encode_uri xs)
                  [] -> ""
                  


get_ping_uri :: [(String, String)] -> Bool -> String
get_ping_uri dict use_def= if use_def == True then (fst $ head $ dict) ++ "?" ++ final
                             else default_announce_uri ++ "?" ++ final
                             where final = encode_uri (tail dict)


ping :: String -> IO ByteString
ping uri = do
    r <- get uri
    pure (BL.toStrict (r ^. responseBody))


getpeerstring :: String -> IO ByteString
-- the idea is to pipe the ping_url through ping, and then parse the resulting bytestring
getpeerstring uri = do
    response_bs <- ping uri
    let BDict r = head $ parse_string_naive $ response_bs
    let peer_list = head $ r !!! "peers"
    let BBString peer_bs = peer_list
    pure(peer_bs)
