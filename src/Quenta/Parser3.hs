module Quenta.Parser3 (
    asum,
    Parser (..),
    sat,
    getRemainingTokens,
    setRemainingTokens,
    MonadPlus,
    empty,
    (<|>),
    many,
    lift,
) where

import Control.Applicative (Alternative (empty, many, (<|>)))
import Control.Monad.Writer (MonadPlus (..))
import Data.Foldable (asum)

newtype Parser m t a = Parser {runParser :: t -> m (a, t)}

getRemainingTokens :: Monad m => Parser m t t
getRemainingTokens = Parser $ \ts -> return (ts, ts)

setRemainingTokens :: Monad m => t -> Parser m t ()
setRemainingTokens ts = Parser $ \_ -> return ((), ts)

sat :: (t -> m (a, t)) -> Parser m t a
sat = Parser

lift :: Monad m => m a -> Parser m t a
lift ma = Parser $
    \ts -> do
        a <- ma
        return (a, ts)

instance Monad m => Functor (Parser m t) where
    fmap f p = Parser $
        \ts -> do
            (x, ts0) <- runParser p ts
            return (f x, ts0)

instance Monad m => Applicative (Parser m t) where
    pure x = Parser $ \ts -> return (x, ts)

    p1 <*> p2 = Parser $
        \ts -> do
            (f, ts0) <- runParser p1 ts
            (x, ts1) <- runParser p2 ts0
            return (f x, ts1)

instance Monad m => Monad (Parser m t) where
    return = pure

    p >>= f = Parser $
        \ts -> do
            (x, ts0) <- runParser p ts
            (y, ts1) <- runParser (f x) ts0
            return (y, ts1)

instance (MonadPlus m) => Alternative (Parser m t) where
    empty = Parser $ const empty

    p1 <|> p2 = Parser $ \ts -> runParser p1 ts <|> runParser p2 ts

instance (MonadPlus m) => MonadPlus (Parser m t) where
    mzero = empty

    mplus p1 p2 = p1 <|> p2

instance MonadFail m => MonadFail (Parser m t) where
    fail x = Parser $ \_ -> fail x
