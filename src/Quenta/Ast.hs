module Quenta.Ast where

import Data.Char
import Quenta.Cst
import Quenta.SyntaxTree
import Quenta.Token
import Quenta.BinopProps

data AstLit
    = AstLitString String
    | AstLitInt Int
    | AstLitNumber Float
    | AstLitVar String

data Pattern
    = PatternVar String
    | PatternTup [Pattern]
data Exp
    = App Exp Exp
    | ExpLit AstLit
    | Lam Pattern Exp

instance Show AstLit where
    show l =
        case l of
            AstLitString x -> x
            AstLitInt n -> show n
            AstLitNumber n -> show n
            AstLitVar x -> x

type Ast = ST CstError AstLit

keywords :: [String]
keywords = ["if", "class", "while", "else", "import", "this"]

convert :: [BinopProps] -> Cst -> Ast
convert binops = stFold (simplifyAst (map (\bp -> (binopConstr bp, bp)) binops)) convertLiteral STErr

convertLiteral :: Token -> Ast
convertLiteral t =
    case tokenConstr t of
        "STR" -> STLeaf $ AstLitString (tokenValue t)
        "INT" -> STLeaf $ AstLitInt (read $ tokenValue t)
        "NUM" -> STLeaf $ AstLitNumber (read $ tokenValue t)
        "ID"
            | tokenValue t `elem` keywords -> STConstr (map toUpper $ tokenValue t) []
            | otherwise -> STLeaf $ AstLitVar (tokenValue t)
        _ -> STConstr (tokenValue t) []

simplifyAst :: [(String, BinopProps)] -> String -> [Ast] -> Ast
simplifyAst binops f as = go f as
  where
    go "APP" as | (STConstr g [] : as0) <- as, g `elem` (map (map toUpper) $ keywords) = STConstr (map toUpper g) as0
    go "BLOCK" [_, a0, _] = a0
    go "STMT" [a0, a1]
        | STConstr ";" [] <- a1 = a0
        | STConstr ":" [STConstr "CLASS" [a2], a3] <- a0 = STConstr "CLASS" [STConstr ":" [a2, a3], a1]
        | STConstr g as0 <- a0, g `elem` (map (map toUpper) $ keywords) = STConstr g (as0 ++ [a1])
    go "EMPTY_BRACES" [STConstr ob [], STConstr cb []] = STConstr (ob ++ cb) []
    go "BRACES" [a0, a1, a2]
        | STConstr "(" [] <- a0
          , STConstr ")" [] <- a2 =
            a1
    go "STMT_LIST" as
        | STConstr "CLASS" [a0, STConstr "STMT_LIST" as1] <- last as = STConstr "CLASS" [a0, STConstr "IMPORTS" (init as), STConstr "MEMBERS" as1]
    go _ [STConstr g [], a0]
        | Just _ <- lookup f binops = STConstr g [a0]
    go _ (a0 : as)
        | Just bp <- lookup f (map (\(k, v) -> (k ++ "_EXPR", v)) binops) =
            case binopAssoc bp of
                BinopAssocLeft a -> leftOp a a0 as
                BinopAssocRight a -> rightOp a a0 as
                BinopAssocList -> listOp a0 as
                BinopAssocNone a -> noneOp a a0 as
    go _ _ = STConstr f as

    leftOp _ a0 [] = a0
    leftOp bp a0 (STConstr g [a1] : as) = leftOp bp (app bp g a0 a1) as

    rightOp _ a0 [] = a0
    rightOp bp a0 (STConstr g [a1] : as) = app bp g a0 (rightOp bp a1 as)

    noneOp bp a0 [STConstr g [a1]] = app bp g a0 a1
    noneOp bp a0 xs = error $ show (bp, a0, xs)

    listOp a0 (STConstr g [a1] : as) = STConstr g ([a0, a1] ++ map q as)
      where
        q (STConstr _ [a1]) = a1

    app bp g a0 a1 =
        case bp of
            BinopAppBuiltin -> STConstr g [a0, a1]
            BinopAppTuple -> STConstr "APP" [STLeaf $ AstLitString $ g, STConstr "TUPLE" [a0, a1]]
            BinopAppCurried -> STConstr "APP" [STConstr "APP" [STLeaf $ AstLitString $ g, a0], a1]
