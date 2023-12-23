module Quenta.Parser3.TokenTree where

import Quenta.PrettyPrint
import Quenta.Token2
import Quenta.Parser3

data TokenTree
    = TokenTree ::: TokenTree
    | Brackets Token TokenTree Token
    | Leaf Token
    | End
    deriving (Show)

instance PrettyPrint TokenTree where
    pp t = "\n" ++ go 0 t
      where
        go i t =
            case t of
                tt1 ::: tt2 -> go i tt1 ++ go i tt2
                Leaf t -> replicate i ' ' ++ pp t ++ "\n"
                Brackets t1 tt t2 ->
                    replicate i ' ' ++ pp t1 ++ "\n"
                        ++ go (i + 2) tt
                        ++ replicate i ' ' ++ pp t2 ++ "\n"
                End -> ""

parseTokenTree :: MonadPlus m => Parser m [Token] TokenTree
parseTokenTree =
    asum
        [ do
            (t1, ob) <- sat $ \case t@(Token (OpenBracket ob) _) : xs -> return ((t, ob), xs); _ -> empty
            tt1 <- parseTokenTree
            t2 <- sat $ \case t@(Token (CloseBracket cb) _) : xs | ob == cb -> return (t, xs); _ -> empty
            (:::) (Brackets t1 tt1 t2) <$> parseTokenTree
        , do
            t <- sat $ \case t@(Token tc _) : xs | not (isBracket tc) -> return (t, xs); _ -> empty
            (:::) (Leaf t) <$> parseTokenTree
        , sat $ \ts -> return (End, ts)
        ]

toString :: [Token] -> String
toString = concatMap tokenValue

toStringTT :: TokenTree -> String
toStringTT tt =
    case tt of
        tt1 ::: tt2 -> toStringTT tt1 ++ toStringTT tt2
        Leaf t -> tokenValue t
        Brackets t1 tt t2 -> tokenValue t1 ++ toStringTT tt ++ tokenValue t2
        End -> ""

filterWhitespace :: TokenTree -> TokenTree
filterWhitespace tt =
    case tt of
        tt1 ::: tt2 -> cons (filterWhitespace tt1) (filterWhitespace tt2)
        Leaf t -> if tokenClass t == Whitespace then End else Leaf t
        Brackets t1 tt t2 -> Brackets t1 (filterWhitespace tt) t2
        End -> End
  where
    cons End tt2 = tt2
    cons tt1 tt2 = tt1 ::: tt2

examplett :: IO TokenTree
examplett = do
    xs <- readFile "src/Quenta/ex2.que"
    let ts = tokenize [Indent 0] xs
    -- mapM_ print ts
    putStrLn ""
    case runParser (parseTokenTree @Maybe) ts of
        Just (tt, []) -> do
            putStrLn . pp $ filterWhitespace tt
            writeFile "src/Quenta/ex2.que.tokens" (pp tt)
            print [xs == toString ts, xs == toStringTT tt]
            return $ filterWhitespace tt
        x -> putStrLn (pp x) >> error ""
