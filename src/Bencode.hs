-- help me god, modiji
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use lambda-case" #-}


module Bencode where
import Control.Applicative (Alternative (empty, (<|>)), some)
import Distribution.Compat.CharParsing (CharParsing(char))
import Data.Char (isDigit)
import qualified Data.ByteString as B   -- Strict ByteString
import qualified Data.ByteString.Lazy as BL  -- Lazy ByteString
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Char8 (ByteString)
import Distribution.PackageDescription (PackageFlag(flagName))
import Data.Ord (comparing)
import Data.List (sortBy)



data Bencode = BInt Int
            | BBString ByteString
            | BList [Bencode]
            | BDict [(ByteString, Bencode)] -- ! Keys should be byte strings/ 
            deriving (Show, Eq)

data Info = BInfo String 

newtype Parser a = Parser {
    run_parser :: ByteString -> Maybe (ByteString, a)
}


instance Functor Parser where
    
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $ \input -> do
                        (input', x) <- p input
                        Just (input', f x)

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = Parser $ \input -> Just (input, x)
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser p1) <*> (Parser p2) =
        Parser $ \input -> do
            (input', f) <- p1 input
            (input'', a) <- p2 input'
            Just (input'', f a)

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \input -> do
        (input', x) <- p input
        run_parser (f x) input'

instance Alternative Parser where
    -- empty :: Parser a
    empty = Parser $ const Nothing
    -- (<|>) :: Parser a -> Parser a -> Parser a
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input




-- ! Parsing
char_parser :: Char -> Parser Char
char_parser x = Parser $ \i -> 
    if BC.null i 
    then Nothing 
    else 
        let (y, ys) = (BC.head i, BC.tail i)
        in if y == x then Just (ys, x) else Nothing

string_parser :: String -> Parser [Char]
string_parser = traverse char_parser

int_parser :: Parser Int
int_parser = fmap read (some (satisfy isDigit))

-- Parser that matches a single character satisfying a given predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \input -> case BC.uncons input of
    Just (x, xs) | predicate x -> Just (xs, x)
    _ -> Nothing

-- ! parsing numbers
bint_parser :: Parser Bencode
bint_parser = bint_parser_pos <|> bint_parser_neg

bint_parser_pos :: Parser Bencode
bint_parser_pos = do
    _ <- char_parser 'i'
    num <- int_parser
    _ <- char_parser 'e'
    pure (BInt num)


bint_parser_neg :: Parser Bencode
bint_parser_neg = do
    _ <- char_parser 'i'
    _ <- char_parser '-'
    num <- int_parser
    _ <- char_parser 'e'
    pure (BInt (negate num))

-- ! parsing bytestrings
bytestring_parser :: Parser Bencode
bytestring_parser = do
    num <- int_parser
    _ <- char_parser ':'
    str <- Parser $ \input ->
        let (str, rest) = BC.splitAt num input
        in if BC.length str == num then Just (rest, str) else Nothing
    pure (BBString str)

-- ! parsing lists
consume_string_lst :: Parser [Bencode]
consume_string_lst = (do
    parsed <- bencode_parser
    rest <- consume_string_lst
    pure (parsed : rest)) <|> pure []

list_parser :: Parser Bencode
list_parser = do
    _ <- char_parser 'l'
    parsed_list <- consume_string_lst
    _ <- char_parser 'e'
    pure (BList parsed_list)

dict_flatten :: [(ByteString, Bencode)] -> [Bencode]
dict_flatten [] = []
dict_flatten (x:xs) = BBString (fst x) : snd x : dict_flatten xs


sort_by_first :: [(ByteString, b)] -> [(ByteString, b)]
sort_by_first = sortBy (comparing fst)

dict_convert :: [Bencode] -> [(ByteString, Bencode)]
dict_convert [] = []
dict_convert (BBString x: y:xs) = (x, y) : dict_convert xs
dict_convert _ = []

dict_parser :: Parser Bencode
dict_parser = do
    _ <- char_parser 'd'
    parsed_list <- consume_string_lst
    _ <- char_parser 'e'
    pure (BDict (sort_by_first (dict_convert parsed_list)))



bencode_parser :: Parser Bencode
bencode_parser = bint_parser <|> bytestring_parser <|> list_parser <|> dict_parser

-- ! de-parsing
-- ! stupid name, I am sorry
bencode_deparser :: Bencode -> ByteString
bencode_deparser (BInt x) = BC.pack $ "i" ++ show x ++ "e"
bencode_deparser (BBString str) = BC.pack (show (BC.length str) ++ ":") `BC.append` str
bencode_deparser (BList lst) = BC.pack "l" `BC.append` BC.concat (map bencode_deparser lst) `BC.append` BC.pack "e"
bencode_deparser (BDict dct) = BC.pack "d" `BC.append` BC.concat (map bencode_deparser (dict_flatten dct)) `BC.append` BC.pack "e"


-- ! helper functions. 

parse_string_naive :: ByteString -> [Bencode]
parse_string_naive str = case run_parser bencode_parser str of
    Just (x,y) -> y : parse_string_naive x
    Nothing -> []

get_fstring :: FilePath -> IO ByteString
get_fstring fname = do
    filecontent <- B.readFile fname

    -- let fstring = BC.unpack filecontent
    return filecontent

parse_file :: FilePath -> IO [Bencode]
parse_file fname = do
    fstring <- get_fstring fname
    return (parse_string_naive fstring)
    


--"https://torrent.ubuntu.com/announce?info_hash=%253F%259A%25AC%2515%258C%257D%25E8%25DF%25CA%25B1%2571%25EA%2558%25A1%257A%25AB%25DF%257F%25BC%2593&peer_id=%251C%2595%2576%259B%25AA%2535%2560%253B%2587%25AD%2544%25CD%2591%2504%250D%2507%250D%259D%2500%259F&port=2000&uploaded=0&downloaded=0&event=started"
-- info_hash=%253F%259A%25AC%2515%258C%257D%25E8%25DF%25CA%25B1%2571%25EA%2558%25A1%257A%25AB%25DF%257F%25BC%2593&peer_id=%251C%2595%2576%259B%25AA%2535%2560%253B%2587%25AD%2544%25CD%2591%2504%250D%2507%250D%259D%2500%259F&port=2000&uploaded=0&downloaded=0&event=started