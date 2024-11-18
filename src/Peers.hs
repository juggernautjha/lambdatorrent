--trying to implement the peer wire protocol. I will be jubilant if i can download any piece

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant bracket" #-}

module Peers where 
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
import Data.Bits (shiftL, (.|.))


-- !Contains Peer Information
type State = HM.HashMap ByteString [Int] -- !am_choking am_interested peer_choking peer_interested

-- !Contains downloading information.
type PieceTable = HM.HashMap Int (Bool, ByteString, Int) -- !Hashmap of Index -> (downloaded, length, hash)

data Position = Fresh | Connected | Request | Downloading | Finished deriving (Show, Eq)
data Message = Choke | Unchoke | Interested | NotInterested | Have | Bitfield | Req | Piece | Cancel | Port deriving (Show, Eq, Enum, Bounded)

message_to_byte :: Message -> Word8
message_to_byte msgType = fromIntegral $ fromEnum msgType

byte_to_message :: Word8 -> Maybe Message
byte_to_message byte
    | byte >= fromIntegral (fromEnum (minBound :: Message)) && byte <= fromIntegral (fromEnum (maxBound :: Message)) = Just (toEnum (fromIntegral byte))
    | otherwise = Nothing

my_id :: ByteString
my_id =  hash $ BC.pack $ "juggernautjha"

there_id :: ByteString
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

bs_to_bin = B.foldl' (\acc byte -> (acc `shiftL` 8) .|. fromIntegral byte) 0


-- ! Gets a chunk of the response 
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


-- !verify the piece message is valid
verify_piece_fr :: ByteString -> Int -> Int -> Int -> Bool
verify_piece_fr response idx begin bytes_requested = let len = (bs_to_bin $ parse_response response 0 4) == (9 + bytes_requested)
                                                         id = (parse_response response 4 5) == B.singleton (message_to_byte Piece)
                                                         resp_idx = (parse_response response 5 9) == encode_to_bytes idx
                                                         resp_begin = (parse_response response 9 13) == encode_to_bytes begin
                                                     in and [len, id, resp_idx, resp_begin]



-- ! for debugging only
encode_to_bytes :: Int -> ByteString
encode_to_bytes idx = B.pack [ fromIntegral (idx `div` 256^3) 
                            , fromIntegral (idx `div` 256^2 `mod` 256)
                            , fromIntegral (idx `div` 256 `mod` 256)
                            , fromIntegral (idx `mod` 256) ]


piece_len :: PieceTable -> Int -> IO (Maybe Int)
piece_len pieces_table idx = do
    case lookup_state idx pieces_table of
        Just (a, b, c) -> return (Just c)
        Nothing -> return Nothing

-- !this sends the request for a particular piece
send_request :: Socket -> Int -> Int -> Int -> IO ()
send_request sock idx begin bytes_requested = do
    let byte = B.singleton $ message_to_byte Req 
    let index_bytes = encode_to_bytes idx
    let begin_bytes = encode_to_bytes begin
    let length_bytes = encode_to_bytes bytes_requested
    let message = B.concat [B.pack [0x00, 0x00, 0x00, 0x0D], byte, index_bytes, begin_bytes, length_bytes]
    sendAll sock message

-- !naively downloads a block    
download_naive :: Socket -> Int -> Int -> Int -> IO (ByteString, Int)
download_naive sock idx begin bytes_requested = do
    send_request sock idx begin bytes_requested
    res <- listen_on_socket sock
    case verify_piece_fr res idx begin bytes_requested of
        True ->  pure (parse_response res 13 (B.length res), begin + max_req_bytes)
        False -> pure (BC.pack "", begin)

-- !Download a full piece by repeatedly downloading blocks
download_full_piece :: Socket -> Int -> PieceTable -> String -> IO PieceTable 
download_full_piece sock idx pieces_table fname = do
    maybe_piece_length <- piece_len pieces_table idx
    case maybe_piece_length of
        Just piece_length -> do
            let download_blocks begin acc = do
                    (block, next_begin) <- download_naive sock idx begin max_req_bytes
                    let new_acc = B.append acc block
                    if next_begin > piece_length
                        then return new_acc
                    else download_blocks next_begin new_acc
            piece_data <- download_blocks 0 B.empty
            let filename = fname ++ show idx ++ ".dat"

            B.writeFile filename piece_data
            case lookup_state idx pieces_table of
                Just (st, hsh, ln) -> do
                    let new_pieces_table = update_state idx (True, hsh, ln) pieces_table
                    return new_pieces_table
                Nothing -> return pieces_table

        Nothing -> return pieces_table

-- !Listen for incoming messages from a peer
listen_on_socket :: Socket -> IO ByteString
listen_on_socket sock = do
    msg <- recv sock 1024  -- Adjust the buffer size as needed
    return msg


-- split:: ByteString -> Int -> [ByteString]
split bstring sz = if (B.length bstring == 0) then [] 
                   else [B.take sz bstring] : split (B.drop sz bstring) sz

generate_table splitstrings bytes_requestedgth residual = case splitstrings of 
    (x:xs) -> if null xs then [(x, residual)] else (x, bytes_requestedgth) : generate_table xs bytes_requestedgth residual
    [] -> []
    

-- !Initialize PieceTable from Info
init_piece_table :: Bencode -> PieceTable
init_piece_table info_b = 
    let BDict pieces_dict = info_b
        BInt length = head $ pieces_dict !!! "length"
        BInt bytes_requestedgth = head $ pieces_dict !!! "piece length"
        residual = length `mod` bytes_requestedgth
        num_pieces = 1 + (length `div` bytes_requestedgth)
        BBString hash_bstring = head $ (pieces_dict !!! "pieces")
        
        splitstrings = [x | xs <- (split hash_bstring 20), x <- xs]
        naive_table  = generate_table splitstrings bytes_requestedgth residual
        better_naive_table = [(False, fst $ x, snd $ x) | x <- naive_table]
        actual_table = [(i, better_naive_table !! i) | i <- [0 .. num_pieces-1]]

    in HM.fromList actual_table

-- ! Meat MMM
event_loop :: Socket -> (Position, State, ByteString) -> ByteString -> String -> Int -> PieceTable -> IO()
event_loop sock (status, state, expected) info_hash fname current_idx pieces_table = do
    case status of
        Fresh -> do
            send_handshake info_hash sock
            res <- listen_on_socket sock
            if (parse_response res 48 68) == expected then 
                let newstatus = Connected
                    newstate = insert_state expected [1,0,1,0] state
                in event_loop sock (newstatus,newstate,expected) info_hash fname current_idx pieces_table 
            else 
                event_loop sock (status,state,expected) info_hash fname current_idx pieces_table -- need to change this in multiclients 
        
        Connected -> do
            -- send a keep alive message
            send_status_singleton sock Interested
            res <- listen_on_socket sock
            if (res == (B.concat [B.pack [0x00, 0x00, 0x00, 0x01], B.singleton (message_to_byte Interested)])) then
                let newstatus = Request
                    newstate = update_state expected [0,1,0,1] state
                in event_loop sock (newstatus,newstate,expected) info_hash fname current_idx pieces_table
            else 
                event_loop sock (status,state,expected) info_hash fname current_idx pieces_table

        Request -> do
            if current_idx ==  length pieces_table then event_loop sock (Finished, state, expected) info_hash fname (current_idx+1) pieces_table
            else case lookup_state current_idx pieces_table of
                Just (st, hsh, ln) -> case st of 
                    True -> if (current_idx + 1) == (length pieces_table) 
                        then event_loop sock (Finished, state, expected) info_hash fname (current_idx+1) pieces_table
                        else event_loop sock (status, state, expected) info_hash fname (current_idx+1) pieces_table
                    False -> do
                        pt <- download_full_piece sock current_idx pieces_table fname
                        let new_pieces_table = pt
                        event_loop sock (Connected, state, expected) info_hash fname (current_idx+1) new_pieces_table
                Nothing -> putStrLn "piece not found in the table"     -- !ideally the flow should never reach here.

        Finished -> do
            putStrLn ( "download finished, closing the connection now.")
            send_kill sock
            close sock
        _ -> putStrLn ("bruh what even oWo.")





run_event_loop :: String -> String -> Socket -> IO ()
run_event_loop fname output sock = do
    (a,b,c) <- get_announce_result fname

    let pieces_table = init_piece_table b
    event_loop sock (Fresh, HM.empty, there_id) a output 0 pieces_table
