module Quenta.PrettyPrint where

import Data.List

class PrettyPrint a where
    pp :: a -> String

instance PrettyPrint a => PrettyPrint (Maybe a) where
    pp x =
        case x of
            Just x0 -> pp x0
            _ -> "Nothing"

instance PrettyPrint a => PrettyPrint [a] where
    pp x = intercalate "\n" (map pp x)

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (a, b) where
    pp (x, y) = pp x ++ "\n" ++ pp y

show' :: Show a => a -> String
show' x =
    let xs = show x
    in go 0 xs
    where
        go i xs = case xs of
            x : y : xs0 | [x, y] `elem` ["()", "[]", "{}"] -> x : y : go i xs0
            x : xs0 | x `elem` "([{" -> x : '\n' : replicate (i + 2) ' ' ++ go (i+2) xs0
            x : xs0 | x `elem` ")]}" -> '\n' : replicate (i - 2) ' ' ++ x : go (i-2) xs0
            ',' : ' ' : xs0 -> ',' : '\n' : replicate i ' ' ++ go i xs0
            ',' : xs0 -> ',' : '\n' : replicate i ' ' ++ go i xs0
            '"' : xs0 ->
                let (string, xs1) = spanString xs0
                in '"' : string ++ go i xs1
            x : xs0 -> x : go i xs0
            [] -> ""

spanString :: String -> (String, String)
spanString xs =
    case xs of
        '"' : xs0 -> ("\"", xs0)
        '\\' : '"' : xs0 ->
            let (xs1, xs2) = spanString xs0
             in ('\\' : '"' : xs1, xs2)
        x : xs0 ->
            let (xs1, xs2) = spanString xs0
             in (x : xs1, xs2)
        [] -> ([], [])
