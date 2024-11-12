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
import Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

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