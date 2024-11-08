-- help me god, modiji
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use lambda-case" #-}
import Control.Applicative (Alternative (empty, (<|>)), some)
import Distribution.Compat.CharParsing (CharParsing(char))
import Data.Char (isDigit)
import qualified Data.ByteString as B   -- Strict ByteString
import qualified Data.ByteString.Lazy as BL  -- Lazy ByteString
import qualified Data.ByteString.Char8 as BC
import Distribution.PackageDescription (PackageFlag(flagName))



data Bencode = BInt Int
            | BBString String
            | BList [Bencode]
            | BDict [(String, Bencode)] -- ! Keys should be byte strings/ 
            deriving (Show, Eq)

newtype Parser a = Parser {
    run_parser :: String -> Maybe (String, a)
}


instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $ \input -> do
                        (input', x) <- p input
                        Just (input', f x)

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \input -> Just (input, x)
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
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
    empty :: Parser a
    empty = Parser $ const Nothing
    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input





char_parser :: Char -> Parser Char
char_parser x = Parser $ \i -> case i of
                    y:ys
                        | y == x -> Just (ys, x)
                    [] -> Nothing
                    _ -> Nothing

string_parser :: String -> Parser [Char]
string_parser = traverse char_parser

int_parser :: Parser Int
int_parser = fmap read (some (satisfy isDigit))

-- Parser that matches a single character satisfying a given predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \input -> case input of
    (x:xs) | predicate x -> Just (xs, x)
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
        let (str, rest) = splitAt num input
        in if length str == num then Just (rest, str) else Nothing
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


-- ! parsing dictionaries
dict_convert [] = []
dict_convert (BBString x: y:xs) = (x, y) : dict_convert xs
dict_convert _ = []

dict_parser :: Parser Bencode
dict_parser = do
      _ <- char_parser 'd'
      parsed_list <- consume_string_lst
      _ <- char_parser 'e'
      pure (BDict (dict_convert parsed_list))



bencode_parser :: Parser Bencode
bencode_parser = bint_parser <|> bytestring_parser <|> list_parser <|> dict_parser


parse_string_naive :: String -> [Bencode]
parse_string_naive str = case run_parser bencode_parser str of
    Just (x,y) -> y : parse_string_naive x
    Nothing -> []

get_fstring :: FilePath -> IO [Char]
get_fstring fname = do
    filecontent <- B.readFile fname

    let fstring = BC.unpack filecontent
    return fstring

parse_file :: FilePath -> IO [Bencode]
parse_file fname = do
    fstring <- get_fstring fname
    return (parse_string_naive fstring)
    



