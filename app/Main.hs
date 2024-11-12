--trying to implement the peer wire protocol. I will be jubilant if i can download any piece

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant bracket" #-}


import Bencode
import Tracker
import Network.Socket
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Network.Socket.ByteString (sendAll)

-- Create a socket and connect to a peer
connectToPeer :: String -> Int -> IO Socket
connectToPeer host port = do
    addr <- resolve host port
    sock <- openSocket addr
    return sock
  where
    resolve host port = do
        let hints = defaultHints { addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just (show port))
        return addr
    openSocket addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock (addrAddress addr)
        return sock

-- Send a handshake message to a peer
sendHandshake :: Socket -> IO ()
sendHandshake sock = do
    let protocol = "BitTorrent protocol"
        infoHash = BS.replicate 20 0x00  -- Example infoHash
        peerId = BS.replicate 20 0x01  -- Example peerId
        reserved = BS.replicate 8 0x00
        handshake = BS.concat [BS.singleton (fromIntegral (length protocol)), BS.pack (map fromIntegral (map (fromEnum) protocol)), reserved, infoHash, peerId]
    sendAll sock handshake

main :: IO ()
main = do
    sock <- connectToPeer "95.173.217.26" 62993
    sendHandshake sock
    putStrLn "Handshake sent"
    -- Further implementation would be needed to handle peer-to-peer communication
