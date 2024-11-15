--trying to implement the peer wire protocol. I will be jubilant if i can download any piece

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant bracket" #-}


import Bencode
import Tracker
import Network.Socket
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL  -- Lazy ByteString
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)
import Data.Word (Word8, Word16)
-- import Data.HashMap.Strict
import Network.Socket.ByteString (sendAll)
import Network.Socket.ByteString (recv)
import qualified Data.HashMap.Strict as HM
import Crypto.Hash.SHA1 (hash)
import Data.ByteString.Char8 (pack, unpack)
import Numeric (showHex)


-- Contains Peer Information
type State = HM.HashMap ByteString [Int] -- !am_choking am_interested peer_choking peer_interested
type PieceTable = HM.HashMap Int (Bool, ByteString, Int) -- !Hashmap of Index -> (downloaded, length, hash)

data Position = Fresh | Connected | Request | Downloading | Finished deriving (Show, Eq)
data Message = Choke | Unchoke | Interested | NotInterested | Have | Req | Bitfield | Piece | Cancel | Port deriving (Show, Eq, Enum, Bounded)

message_to_byte :: Message -> Word8
message_to_byte msgType = 1 + (fromIntegral $ fromEnum msgType)

byte_to_message :: Word8 -> Maybe Message
byte_to_message byte
    | byte > 0 && byte <= fromIntegral (fromEnum (maxBound :: Message)) = Just (toEnum (fromIntegral byte - 1))
    | otherwise = Nothing
my_id :: ByteString
my_id =  hash $ BC.pack $ "juggernautjha"
there_id = hash $ BC.pack $ "cmprssnenthsst"


shaa :: ByteString -> String
shaa = concatMap (`showHex` "") . B.unpack . hash

protocol :: String
protocol = "BitTorrent protocol"

get_bstring :: String -> ByteString
get_bstring str = B.pack (map fromIntegral (map (fromEnum) str))

max_req_size :: ByteString
max_req_size = B.pack [0x00, 0x00, 0x40, 0x00]

max_req_bytes :: Int
max_req_bytes = 16384


-- !helper functions for working with HMs
init_state = HM.empty

insert_state key value state = HM.insert key value state

update_state key newValue state = HM.adjust (const newValue) key state

lookup_state key state = HM.lookup key state



parse_response :: ByteString -> Int -> Int -> ByteString
parse_response bstr start stop = B.take (stop - start) . B.drop start $ bstr

-- !Create a socket and connect to a peer
connect_to_peer :: String -> Int -> IO Socket
connect_to_peer host port = do
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

-- !Send a handshake message to a peer
send_handshake :: ByteString -> Socket  -> IO ()
send_handshake info_hash sock= do
    let reserved = B.replicate 8 0x00
        handshake = B.concat [B.singleton (fromIntegral (length protocol)), (get_bstring protocol), reserved, info_hash, my_id]
    sendAll sock handshake


-- !send keep_alive
send_keepalive :: Socket -> IO()
send_keepalive sock = do
    let msg = B.replicate 4 0x00
    sendAll sock msg

-- !non standard, i send my roll number to get the other peer to disconnect
send_kill :: Socket -> IO()
send_kill sock = do
    let msg = B.pack [0x32, 0x31, 0x30, 0x38, 0x30, 0x32]
    sendAll sock msg



-- !send status
send_status_singleton :: Socket -> Message -> IO ()
send_status_singleton sock msg = do
    let byte = message_to_byte msg
    if byte < 5 then
        let message = B.concat [B.pack [0x00, 0x00, 0x00, 0x01], B.singleton byte]
        in sendAll sock message
    else
        putStrLn ("maybe use other message codes? x. ")




-- ! for debugging only
funzies idx = B.pack [ fromIntegral (idx `div` 256^3) 
                            , fromIntegral (idx `div` 256^2 `mod` 256)
                            , fromIntegral (idx `div` 256 `mod` 256)
                            , fromIntegral (idx `mod` 256) ]
-- request: <len=0013><id=6><index><begin><length>
-- !this sends the request for a particular piece
send_request :: Socket -> Int -> Int -> IO ()
send_request sock idx begin = do
    let byte = B.singleton $ message_to_byte Req 
    let index_bytes = B.pack [ fromIntegral (idx `div` 256^3) 
                            , fromIntegral (idx `div` 256^2 `mod` 256)
                            , fromIntegral (idx `div` 256 `mod` 256)
                            , fromIntegral (idx `mod` 256) ]
    let begin_bytes = B.pack [ fromIntegral (begin `div` 256^3) 
                            , fromIntegral (begin `div` 256^2 `mod` 256)
                            , fromIntegral (begin `div` 256 `mod` 256)
                            , fromIntegral (begin `mod` 256) ]
    let length_bytes = max_req_size
    let message = B.concat [B.pack [0x00, 0x00, 0x00, 0x0D], byte, index_bytes, begin_bytes, length_bytes]
    sendAll sock message

-- !naively downloads a block    
download_naive :: Socket -> Int -> Int -> IO (ByteString, Int)
download_naive sock idx begin = do
    send_request sock idx begin
    res <- listen_on_socket sock
    pure (res, begin + max_req_bytes)

-- !Download a full piece by repeatedly downloading blocks
download_full_piece :: Socket -> Int -> PieceTable -> IO PieceTable 
download_full_piece sock idx pieces_table = do
    let download_blocks begin acc = do
            (block, next_begin) <- download_naive sock idx begin
            let new_acc = B.append acc block
            if B.length new_acc >= max_req_bytes
                then return new_acc
                else download_blocks next_begin new_acc
    piece_data <- download_blocks 0 B.empty
    let filename = "piece_" ++ show idx ++ ".dat"
    B.writeFile filename piece_data
    case lookup_state idx pieces_table of
        Just (st, hsh, ln) -> do
            let new_pieces_table = update_state idx (True, hsh, ln) pieces_table
            return new_pieces_table
        Nothing -> return pieces_table

-- !Listen for incoming messages from a peer
listen_on_socket :: Socket -> IO ByteString
listen_on_socket sock = do
    msg <- recv sock 1024  -- Adjust the buffer size as needed
    return msg

split bstring sz = if (B.length bstring == 0) then [] 
                   else [B.take sz bstring] : split (B.drop sz bstring) sz

generate_table splitstrings piece_length residual = case splitstrings of 
    (x:xs) -> if null xs then [(x, residual)] else (x, piece_length) : generate_table xs piece_length residual
    [] -> []
    

-- !Initialize PieceTable from Info
init_piece_table :: Bencode -> PieceTable
-- ! !!! returns a list 
init_piece_table info_b = 
    let BDict pieces_dict = info_b
        BInt length = head $ pieces_dict !!! "length"
        BInt piece_length = head $ pieces_dict !!! "piece length"
        residual = length `mod` piece_length
        num_pieces = 1 + (length `div` piece_length)
        BBString hash_bstring = head $ (pieces_dict !!! "pieces")
        
        splitstrings = [x | xs <- (split hash_bstring 20), x <- xs]
        naive_table  = generate_table splitstrings piece_length residual
        better_naive_table = [(False, fst $ x, snd $ x) | x <- naive_table]
        actual_table = [(i, better_naive_table !! i) | i <- [0 .. num_pieces-1]]

    in HM.fromList actual_table

-- ! Meat MMM
event_loop :: Socket -> (Position, State, ByteString) -> ByteString -> Int -> PieceTable -> IO()
event_loop sock (status, state, expected) info_hash current_idx pieces_table = do
    case status of
        Fresh -> do
            send_handshake info_hash sock
            res <- listen_on_socket sock
            if (parse_response res 48 68) == expected then 
                let newstatus = Connected
                    newstate = insert_state expected [1,0,1,0] state
                in event_loop sock (newstatus,newstate,expected) info_hash current_idx pieces_table 
            else 
                event_loop sock (status,state,expected) info_hash current_idx pieces_table -- need to change this in multiclients 
        
        Connected -> do
            -- send a keep alive message
            send_status_singleton sock Interested
            res <- listen_on_socket sock
            if (res == (B.concat [B.pack [0x00, 0x00, 0x00, 0x01], B.singleton (message_to_byte Interested)])) then
                let newstatus = Request
                    newstate = update_state expected [0,1,0,1] state
                in event_loop sock (newstatus,newstate,expected) info_hash current_idx pieces_table
            else 
                event_loop sock (status,state,expected) info_hash current_idx pieces_table

        Request -> do
            if current_idx ==  length pieces_table then event_loop sock (Finished, state, expected) info_hash (current_idx+1) pieces_table
            else case lookup_state current_idx pieces_table of
                Just (st, hsh, ln) -> case st of 
                    True -> if (current_idx + 1) == (length pieces_table) 
                        then event_loop sock (Finished, state, expected) info_hash (current_idx+1) pieces_table
                        else event_loop sock (status, state, expected) info_hash (current_idx+1) pieces_table
                    False -> do
                        pt <- download_full_piece sock current_idx pieces_table
                        let new_pieces_table = pt
                        event_loop sock (Connected, state, expected) info_hash (current_idx+1) new_pieces_table
                Nothing -> putStrLn "piece not found in the table"     -- !ideally the flow should never reach here.

        Finished -> do
            putStrLn ( "download Finished, closing the connection now.")
            send_kill sock
            close sock
        _ -> putStrLn ("bruh what even oWo.")



run_event_loop :: String -> Socket -> IO ()
run_event_loop fname sock = do
    (a,b,c) <- get_announce_result fname
    let pieces_table = init_piece_table b
    event_loop sock (Fresh, HM.empty, there_id) a 0 pieces_table


-- mymain :: (a,b) -> IO ByteString
-- mymain pair = do
--     sock <- connect_to_peer "127.0.0.1" 5000
--     (a, b, c) <- get_announce_result "examples/ubuntu.torrent"
--     send_handshake a sock 
    
    -- Further implementation would be needed to handle peer-to-peer communication
