module Quenta.Parser2 where

import Control.Applicative ( Alternative((<|>), empty) )
import Control.Monad ( MonadPlus(..) )

newtype Parser m t a = Parser { runParser :: [t] -> m (a, Int) }

getRemainingTokens :: Monad m => Parser m t [t]
getRemainingTokens = Parser $ \ ts -> return (ts, 0)

dropTokens :: Monad m => Int -> Parser m t ()
dropTokens k = Parser $ \ _ -> return ((), k)

sat :: MonadPlus m => (t -> Bool) -> Parser m t t
sat p = do
    ts <- getRemainingTokens
    case ts of
        t:_ | p t -> do
            dropTokens 1
            return t
        _ -> mzero

instance Monad m => Functor (Parser m t) where
    fmap f p = Parser $ \ ts -> do
        (x, i) <- runParser p ts
        return (f x, i)

instance Monad m => Applicative (Parser m t) where
    pure x = Parser $ \ _ -> return (x, 0)
    p1 <*> p2 = Parser $ \ ts -> do
        (f, i0) <- runParser p1 ts
        (x, i1) <- runParser p2 (drop i0 ts)
        return (f x, i0 + i1)

instance Monad m => Monad (Parser m t) where
    return = pure
    p >>= f = Parser $ \ ts -> do
        (x, i0) <- runParser p ts
        (y, i1) <- runParser (f x) (drop i0 ts)
        return (y, i0 + i1)

instance (MonadPlus m) => Alternative (Parser m t) where
    empty = Parser $ const empty
    p1 <|> p2 = Parser $ \ ts -> runParser p1 ts <|> runParser p2 ts

instance (MonadPlus m) => MonadPlus (Parser m t) where
    mzero = empty
    mplus p1 p2 = p1 <|> p2

instance MonadFail m => MonadFail (Parser m t) where
    fail x = Parser $ \ _ -> fail x
