module Quenta.Token2 where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Quenta.PrettyPrint

data BracketStackType
    = Indent Int
    | Curly
    | Square
    | Round
    deriving (Eq, Show)

data TokenClass
    = OpenBracket BracketStackType
    | CloseBracket BracketStackType
    | Newline
    | Whitespace
    | Ident
    | Comment
    | String
    | Number
    | Symbol
    | Error String
    deriving (Show, Eq)

isBracket :: TokenClass -> Bool
isBracket (OpenBracket _) = True
isBracket (CloseBracket _) = True
isBracket _ = False

data Token = Token
    { tokenClass :: TokenClass
    , tokenValue :: String
    }
    deriving (Show)

instance PrettyPrint Token where
    pp t =
        let xs = show (tokenClass t)
         in xs ++ replicate (25 - length xs) ' ' ++ show (tokenValue t)

tokenize :: [BracketStackType] -> String -> [Token]
tokenize bts@(bt : bts0) (x : xs) =
    case x of
        '\n'
            | ('\n' : xs') <- xs ->
                let (newlines, xs0) = span (== '\n') xs'
                 in Token Whitespace ('\n' : newlines) : tokenize bts ('\n' : xs0)
        '\n' -> tokenizeNewline bts xs
        '(' -> Token (OpenBracket Round) "(" : tokenize (Round : bts) xs
        ')' | Round <- bt -> Token (CloseBracket Round) ")" : tokenize bts0 xs
        '[' -> Token (OpenBracket Square) "[" : tokenize (Square : bts) xs
        ']' | Square <- bt -> Token (CloseBracket Square) "]" : tokenize bts0 xs
        '{' -> Token (OpenBracket Curly) "{" : tokenize (Curly : bts) xs
        '}' | Curly <- bt -> Token (CloseBracket Curly) "}" : tokenize bts0 xs
        ' ' ->
            let (spaces, xs0) = span (== ' ') xs
             in Token Whitespace (' ' : spaces) : tokenize bts xs0
        '/'
            | ('/' : xs') <- xs ->
                let (comment, xs0) = spanComment xs'
                 in Token Comment ('/' : '/' : comment) : tokenize bts xs0
        '"' ->
            let (string, xs0) = spanString xs
             in Token String ('"' : string) : tokenize bts xs0
        '\'' ->
            let (char, xs0) = spanChar xs
             in Token String ('\'' : char) : tokenize bts xs0
        _
            | isIdentFirstChar x ->
                let (ident, xs0) = span isIdentConsecutiveChar xs
                 in Token Ident (x : ident) : tokenize bts xs0
            | isDigit x ->
                let (number, xs0) = span isDigit xs
                 in Token Number (x : number) : tokenize bts xs0
            | isSymbolChar x ->
                let (symbol, xs0) = spanSymbol xs
                 in Token Symbol (x : symbol) : tokenize bts xs0
            | otherwise -> Token (Error (show x)) [x] : tokenize bts xs
tokenize [_] [] = []
tokenize bts [] = dedentMany bts 0 []

isIdentFirstChar :: Char -> Bool
isIdentFirstChar c = isAlpha c || c == '_'

isIdentConsecutiveChar :: Char -> Bool
isIdentConsecutiveChar c = isAlphaNum c || c == '_'

isSymbolChar :: Char -> Bool
isSymbolChar c = not (isAlphaNum c) && not (isSpace c) && (c `notElem` "(){}[]\"")

spanSymbol :: String -> (String, String)
spanSymbol xs =
    case xs of
        '/' : '/' : _ -> ([], xs)
        x : xs0
            | isSymbolChar x ->
                let (xs1, xs2) = spanSymbol xs0
                 in (x : xs1, xs2)
        _ -> ([], xs)

spanComment :: String -> (String, String)
spanComment xs =
    case xs of
        '\n' : xs0 -> ("\n", xs0)
        x : xs0 ->
            let (xs1, xs2) = spanComment xs0
             in (x : xs1, xs2)
        [] -> ([], [])


spanChar :: String -> (String, String)
spanChar xs =
    case xs of
        '\'' : xs0 -> ("\'", xs0)
        '\\' : '\'' : xs0 ->
            let (xs1, xs2) = spanChar xs0
             in ('\\' : '\'' : xs1, xs2)
        x : xs0 ->
            let (xs1, xs2) = spanChar xs0
             in (x : xs1, xs2)
        [] -> ([], [])

tokenizeNewline :: [BracketStackType] -> String -> [Token]
tokenizeNewline bts@(bt : bts0) xs =
    let (spaces, xs0) = span (== ' ') xs
        n = length spaces
     in case bt of
            Indent x0 -> case compare x0 n of
                EQ -> Token Newline ('\n' : spaces) : tokenize bts xs0
                LT -> Token (OpenBracket (Indent n)) ('\n' : spaces) : tokenize (Indent n : bts) xs0
                GT -> Token (CloseBracket (Indent x0)) ('\n' : spaces) : dedentMany bts0 n xs0
            _ -> Token Whitespace ('\n' : spaces) : tokenize bts xs0

dedentMany :: [BracketStackType] -> Int -> String -> [Token]
dedentMany bts@(bt : bts0) n xs0 =
    case bt of
        Indent x0 -> case compare x0 n of
            EQ -> tokenize bts xs0
            LT -> tokenize (Indent n : bts) xs0
            GT -> Token (CloseBracket (Indent x0)) "" : dedentMany bts0 n xs0
        _ -> Token (Error $ "Unmatched bracket: " ++ show bt) "" : tokenize bts0 xs0
