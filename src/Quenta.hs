module Quenta (
    module Quenta.Ast,
    example,
) where

import Quenta.Ast
import Quenta.BinopProps
import Quenta.Cst
import Quenta.SyntaxTree

binops :: [BinopProps]
binops =
    [ binop "TUPLE" [","] ","
    , binop "ASSIGN" ["="] "nb"
    , binop "LAM" ["=>"] "rb"
    , binop "TYPE" [":"] "nb"
    , binop "ARROW" ["->"] "rb"
    , binop "OR" ["||"] "lt"
    , binop "AND" ["&&"] "lt"
    , binop "COMPARISON" ["==", "<", ">", "<=", "!="] "nt"
    , binop "SUM" ["+", "-"] "lt"
    , binop "PRODUCT" ["*", "/", "%"] "lt"
    , binop "INDEX" ["."] "lb"
    ]

example :: IO ()
example = do
    xs <- readFile "src/Quenta/ex.que"
    let cst = parseCst binops xs
        ast = convert binops cst
    putStrLn $ stPrettyPrint cst
    putStrLn $ stPrettyPrint ast
    return ()
