module Quenta.Parser where

import Quenta.Token

newtype Parser a = Parser { runParser :: [Token] -> (a, Int) }

instance Functor Parser where
    fmap f p = Parser $ \ ts -> let (x, i) = runParser p ts in (f x, i)

instance Applicative Parser where
    pure x = Parser $ \ _ -> (x, 0)
    p1 <*> p2 = Parser $ \ ts ->
        let (f, i0) = runParser p1 ts
            (x, i1) = runParser p2 (drop i0 ts)
        in (f x, i0 + i1)

instance Monad Parser where
    return = pure
    p >>= f = Parser $ \ ts ->
        let (x, i0) = runParser p ts
            (y, i1) = runParser (f x) (drop i0 ts)
        in (y, i0 + i1)

getRemainingTokens :: Parser [Token]
getRemainingTokens = Parser $ \ ts -> (ts, 0)

dropTokens :: Int -> Parser ()
dropTokens k = Parser $ \ _ -> ((), k)

many :: Parser (Maybe a) -> Parser [a]
many p = do
    x <- p
    case x of
        Nothing -> return []
        Just x' -> do
            xs <- many p
            return $ x':xs

many1 :: Parser (Maybe a) -> Parser (Maybe [a])
many1 p = do
    xs <- many p
    case xs of
        [] -> return Nothing
        _ -> return $ Just xs

choice :: Parser (Maybe a) -> Parser a -> Parser a
choice p1 p2 = do
    x <- p1
    case x of
        Nothing -> p2
        Just x -> return x

sat :: (Token -> Bool) -> Parser (Maybe Token)
sat p = do
    ts <- getRemainingTokens
    case ts of
        t:_ | p t -> do
            dropTokens 1
            return $ Just t
        _ -> return Nothing
