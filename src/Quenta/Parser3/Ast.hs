module Quenta.Parser3.Ast where

import Quenta.PrettyPrint
import Quenta.Parser3
import Quenta.Parser3.TokenTree

import Quenta.Token2
import Control.Monad.Writer
import Control.Monad.Trans.Maybe

data Literal
    = Lstring String
    | Lnumber Int
    deriving (Show)

data Expr
    = Evar String
    | Elam (Args String ()) Expr
    | Eapp Expr Expr
    | Elit Literal
    deriving (Show)

data Type
    = Tvar String
    | Tfun Type Type
    | Tapp Type Type
    deriving (Show)

data Args a b
    = ArgsCons (Args a b) a
    | ArgsBase b
    deriving (Show)

data Class = Class
    { cname :: Args String String
    , ctype :: Maybe Type
    , cmembers :: [Member]
    }
    deriving (Show)

data Member
    = TypeDef String Type
    | ExprDef (Args String String) Expr
    deriving (Show)

type Parser' m a = Parser m TokenTree a

parseBrackets :: (MonadPlus m) => Parser' m a -> Parser' m a
parseBrackets p = sat $ \case
    Brackets _ tt _ ::: t0 -> do
        (x, tt0) <- runParser p tt
        case tt0 of
            End -> return (x, t0)
            _ -> empty
    Brackets _ tt _ -> do
        (x, tt0) <- runParser p tt
        case tt0 of
            End -> return (x, End)
            _ -> empty
    _ -> empty

parseId :: MonadPlus m => Parser' m String
parseId = sat $ \case
    Leaf (Token Ident id) ::: t0 -> return (id, t0)
    _ -> empty

parseKeyword :: MonadPlus m => String -> Parser' m ()
parseKeyword kw = sat $ \case Leaf (Token _ id) ::: t0 | id == kw -> return ((), t0); _ -> empty

parseArgs :: MonadPlus m => Parser' m a -> Parser' m b -> Parser' m (Args a b)
parseArgs p1 p2 = do
    b <- p2
    as <- many p1
    return $ foldl ArgsCons (ArgsBase b) as

parseType :: MonadPlus m => Parser' m Type
parseType =
    asum
        [ parseBrackets parseType
        , do
            t <- parseTypeAtom
            ts <- many (parseKeyword "->" >> parseTypeAtom)
            return $ foldr1 Tfun (t : ts)
        ]
  where
    parseTypeAtom = do
        t <- Tvar <$> parseId
        ts <- many (Tvar <$> parseId)
        return $ foldl1 Tapp (t : ts)

parseExpr :: MonadPlus m => Parser' m Expr
parseExpr =
    asum
        [ Evar <$> parseId
        ]

parseMember :: (MonadWriter String m, MonadPlus m) => Parser' m Member
parseMember =
    asum
        [ do
            name <- parseId
            parseKeyword ":"
            TypeDef name <$> parseType
        , do
            name <- parseArgs parseId parseId
            ExprDef name <$> parseBrackets parseExpr
        , do
            Quenta.Parser3.lift $ do
                tell "hej"
                empty
        ]

parseClass :: MonadPlus m => Parser' m Class
parseClass =
    asum
        [ do
            parseKeyword "interface"
            name <- parseArgs parseId parseId
            members <- asum [parseBrackets (many parseMember), return []]
            return $
                Class
                    { cname = name
                    , ctype = Nothing
                    , cmembers = members
                    }
        , do
            parseKeyword "class"
            name <- parseArgs parseId parseId
            parseKeyword ":"
            t <- parseType
            members <- many parseMember
            return $
                Class
                    { cname = name
                    , ctype = Just t
                    , cmembers = members
                    }
        ]

type M = MaybeT (Writer [String])

example :: IO ()
example = do
    tt <- Quenta.Parser3.TokenTree.examplett
    case runWriter (runMaybeT ((runParser @M) parseType tt)) of
        (Nothing, log) ->
            mapM_ putStrLn log
        (Just x, log) -> do
            mapM_ putStrLn log
            putStrLn $ show' x
