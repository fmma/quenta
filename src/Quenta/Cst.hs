module Quenta.Cst where

import Quenta.Parser (
    Parser,
    choice,
    getRemainingTokens,
    many,
    runParser,
    sat,
 )
import Quenta.SyntaxTree
import Quenta.BinopProps
import Quenta.Token

data CstError = CstError [Token] String

instance Show CstError where
    show (CstError (t : _) err) =
        let (ln, col) = tokenLinePos t
         in err
                ++ "\nActual: "
                ++ show (tokenValue t)
                ++ ".\nAt: "
                ++ show (ln, col)
                ++ ".\n\n"
                ++ tokenErrorReport t
    show (CstError [] err) = err

type Cst = ST CstError Token

parseCst :: [BinopProps] -> String -> Cst
parseCst binops xs =
    let ts = tokenize xs
        (cst, i) = runParser (parseStmts binops) ts
     in case drop i ts of
            [] -> cst
            _ -> STConstr "PREFIX" [cst, STErr $ CstError ts "Unable to parse remaining tokens."]

ok :: Cst -> Maybe Cst
ok c =
    case c of
        STErr _ -> Nothing
        _ -> Just c

failEarly :: Parser Cst -> (Cst -> Parser Cst) -> Parser Cst
failEarly p f = do
    c <- p
    case ok c of
        Nothing -> return c
        Just _ -> f c

orElse :: Parser Cst -> Parser Cst -> Parser Cst
orElse p1 = choice (ok <$> p1)

keyword :: String -> Parser Cst
keyword kw = do
    mt <- sat ((== kw) . tokenValue)
    case mt of
        Nothing -> do
            ts <- getRemainingTokens
            return $ STErr $ CstError (take 1 ts) $ "Expected: " ++ show kw ++ "."
        Just t -> return $ STLeaf t

ident :: Parser Cst
ident = do
    mt <- sat ((== "ID") . tokenConstr)
    case mt of
        Nothing -> do
            ts <- getRemainingTokens
            return $ STErr $ CstError (take 1 ts) $ "Expected IDENT."
        Just t -> return $ STLeaf t

literal :: Parser Cst
literal = do
    mt <- sat ((`elem` ["INT", "NUM", "STR"]) . tokenConstr)
    case mt of
        Nothing -> do
            ts <- getRemainingTokens
            return $ STErr $ CstError (take 1 ts) $ "Expected INT, NUM or STR."
        Just t -> return $ STLeaf t

parseBraces :: String -> String -> Parser Cst -> Parser Cst
parseBraces ob cb p = do
    keyword ob `failEarly` \c0 -> emptyBraces c0 `orElse` nonEmptyBraces c0
    where
        nonEmptyBraces c0 = do
            c1 <- p
            c2 <- keyword cb
            return $ STConstr "BRACES" [c0, c1, c2]

        emptyBraces c0 = do
            keyword cb `failEarly` \c1 -> return $ STConstr "EMPTY_BRACES" [c0, c1]

parens :: Parser Cst -> Parser Cst
parens = parseBraces "(" ")"

squareBrackets :: Parser Cst -> Parser Cst
squareBrackets = parseBraces "[" "]"

{-
 e ::= x | lit | e e | x => e | (e) | e (+) e

 precedence
 .
 * / %
 + -
 == < > >= <=
 &&
 ||
 =>
 :
 =
 ,
-}

parseStmts :: [BinopProps] -> Parser Cst
parseStmts binops = STConstr "STMT_LIST" <$> many (ok <$> statement)
  where
    expr = parseExpr binops

    block = do
        keyword "{" `failEarly` \c0 -> do
            c1 <- STConstr "STMT_LIST" <$> many (ok <$> statement)
            c2 <- keyword "}"
            return $ STConstr "BLOCK" [c0, c1, c2]

    statement = do
        expr `failEarly` \c0 -> do
            c1 <- keyword ";" `orElse` block
            return $ STConstr "STMT" [c0, c1]

parseExpr :: [BinopProps] -> Parser Cst
parseExpr binops = foldr ($) app (map prepareBinop binops)
  where
    prepareBinop p = binopParser (binopConstr p) (foldl1 orElse (map keyword $ binopOps p))

    expr = parseExpr binops

    atom =
        squareBrackets expr
            `orElse` parens expr
            `orElse` ident
            `orElse` literal

    binopParser constr pOp pSub = do
        pSub `failEarly` \c0 -> do
            cs <- many (ok <$> with)
            return $ case cs of
                [] -> c0
                _ -> STConstr (constr ++ "_EXPR") (c0 : cs)
      where
        with = do
            pOp `failEarly` \c0 -> do
                c1 <- pSub
                return $ STConstr constr [c0, c1]

    app = do
        atom `failEarly` \c0 -> do
            cs <- many (ok <$> atom)
            return $ case cs of
                [] -> c0
                _ -> STConstr "APP" (c0 : cs)
