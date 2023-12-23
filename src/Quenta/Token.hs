module Quenta.Token (
    Token,
    tokenSrc,
    tokenConstr,
    tokenPos,
    tokenValue,
    tokenLength,
    tokenLine,
    tokenLinesBefore,
    tokenLinesAfter,
    tokenLinePos,
    tokenErrorReport,
    tokenize,
    symbols,
    braces,
) where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List (intercalate)

data Token = Token
    { tokenSrc :: String
    , tokenConstr :: String
    , tokenPos :: Int
    , tokenLength :: Int
    }

instance Show Token where
    show t = tokenConstr t ++ "(" ++ tokenValue t ++ ")"

tokenValue :: Token -> String
tokenValue t = take (tokenLength t) $ drop (tokenPos t) (tokenSrc t)

tokenLine :: Token -> String
tokenLine t =
    let (ln, _) = tokenLinePos t
     in lines (tokenSrc t) !! ln

tokenLinesBefore :: Int -> Token -> [String]
tokenLinesBefore k t =
    let (ln, _) = tokenLinePos t
        underflow = max (k - ln) 0
     in take (k - underflow) $ drop (ln - k) $ lines (tokenSrc t)

tokenLinesAfter :: Int -> Token -> [String]
tokenLinesAfter k t =
    let (ln, _) = tokenLinePos t
     in take k $ drop (ln + 1) $ lines (tokenSrc t)

tokenLinePos :: Token -> (Int, Int)
tokenLinePos t =
    let (ln, col) = go (tokenPos t) (tokenSrc t)
     in ( ln
        , case col of
            Left col0 -> col0
            Right col0 -> col0
        )
  where
    go 0 _ = (0, Left 0)
    go i xs =
        case xs of
            '\n' : xs0 ->
                let (ln, col) = go (i -1) xs0
                 in ( ln + 1
                    , case col of
                        Left col0 -> Right col0
                        Right col0 -> Right col0
                    )
            _ : xs0 ->
                let (ln, col) = go (i -1) xs0
                 in ( ln
                    , case col of
                        Left col0 -> Left (col0 + 1)
                        Right col0 -> Right col0
                    )
            [] -> (0, Right (- i))

tokenErrorReport :: Token -> String
tokenErrorReport t =
    let (_, col) = tokenLinePos t
     in intercalate "\n" (tokenLinesBefore 10 t)
            ++ "\n"
            ++ tokenLine t
            ++ "\n"
            ++ replicate col ' '
            ++ "^\n"
            ++ intercalate "\n" (tokenLinesAfter 5 t)
            ++ "\n"

symbols1 :: [Char]
symbols1 = ".=;<>,+-*/:%"

symbols2 :: [[Char]]
symbols2 = ["->", "=>", ">=", "<=", "==", "!=", "&&", "||"]

symbols :: [[Char]]
symbols = symbols2 ++ map return symbols1

braces :: [Char]
braces = "({[]})"

tokenize :: String -> [Token]
tokenize src = f 0 src
  where
    tok = Token src

    f i xs = case xs of
        ' ' : xs0 -> f (i + 1) xs0
        '\n' : xs0 -> f (i + 1) xs0
        '\t' : xs0 -> f (i + 1) xs0
        '/' : '/' : xs0 -> comment (i + 2) xs0
        x : y : xs0 | [x, y] `elem` symbols2 -> tok "SYM" i 2 : f (i + 2) xs0
        x : xs0
            | x `elem` symbols1 -> tok "SYM" i 1 : f (i + 1) xs0
        x : xs0
            | x `elem` braces -> tok "BRA" i 1 : f (i + 1) xs0
        '"' : xs0 -> string i 2 xs0
        x : xs0
            | isAlpha x -> ident i 1 xs0
        x : xs0
            | isDigit x -> int i 1 xs0
        [] -> []
        a -> error ("FATAL ERROR: UNRECOGNIZED TOKEN: " ++ show a)

    comment i xs = case xs of
        '\n' : xs0 -> f (i + 1) xs0
        _ : xs0 -> comment (i + 1) xs0
        [] -> f i []

    string i j xs = case xs of
        '"' : xs0 -> tok "STR" i j : f (i + j) xs0
        _ : xs0 -> string i (j + 1) xs0
        [] -> tok "BAD_STR" i j : f (i + j) []

    ident i j xs = case xs of
        x : xs0
            | isAlphaNum x -> ident i (j + 1) xs0
        _ -> tok "ID" i j : f (i + j) xs

    int i j xs = case xs of
        x : xs0
            | isDigit x -> int i (j + 1) xs0
        '.' : xs0 -> num i (j + 1) xs0
        x : xs0
            | isAlpha x -> badNum i (j + 1) xs0
        _ -> tok "INT" i j : f (i + j) xs

    num i j xs = case xs of
        x : xs0
            | isDigit x -> num i (j + 1) xs0
        x : xs0
            | isAlpha x || x == '.' -> badNum i (j + 1) xs0
        _ -> tok "NUM" i j : f (i + j) xs

    badNum i j xs = case xs of
        x : xs0
            | isAlphaNum x || x == '.' -> badNum i (j + 1) xs0
        _ -> tok "BAD_NUM" i j : f (i + j) xs
