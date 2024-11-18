--trying to implement the peer wire protocol. I will be jubilant if i can download any piece

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant bracket" #-}

module Main where
import Bencode
import Tracker
import Peers
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL  -- Lazy ByteString
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)
import Data.Word (Word8, Word16)

download_torrent :: String -> Int -> String -> String -> IO()
download_torrent ip port fname outfile = do
    sock <- connect_to_peer ip port
    run_event_loop fname outfile sock


main :: IO()
main = do
    putStrLn "Enter the IP address of the peer:"
    ip <- getLine
    putStrLn "Enter the port number:"
    port <- readLn :: IO Int
    putStrLn "Enter the torrent file name:"
    fname <- getLine
    putStrLn "Enter the output file name:"
    outfile <- getLine
    download_torrent ip port fname outfile