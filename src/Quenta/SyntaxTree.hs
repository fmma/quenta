module Quenta.SyntaxTree where

import Data.List

data ST err a
    = STConstr String [ST err a]
    | STLeaf a
    | STErr err

stConstr :: ST err a -> String
stConstr c =
    case c of
        STConstr f _ -> f
        _ -> ""

stSubtrees :: ST err a -> [ST err a]
stSubtrees c =
    case c of
        STConstr _ cs -> cs
        _ -> []

stLeaf :: ST err a -> Maybe a
stLeaf c =
    case c of
        STLeaf t -> Just t
        _ -> Nothing

stErrors :: ST err a -> [err]
stErrors c =
    case c of
        STConstr _ cs -> concatMap stErrors cs
        STErr err -> [err]
        _ -> []

instance (Show err, Show a) => Show (ST err a) where
    show c =
        case c of
            STConstr f cs -> f ++ "(" ++ intercalate ", " (map show cs) ++ ")"
            STLeaf t -> show t
            STErr err -> "<ERROR> " ++ show err ++ "</ERROR>"

stPrettyPrint :: (Show err, Show a) => ST err a -> String
stPrettyPrint st = go 0 st
  where
    go i c =
        case c of
            STLeaf t -> show t
            STConstr f [] -> f
            STConstr f cs ->
                f
                    ++ "\n"
                    ++ ind (i + 1)
                    ++ intercalate ("\n" ++ ind (i + 1)) (map (go (i + 1)) cs)
            STErr err ->
                "<ERROR>\n"
                    ++ show err
                    ++ ind i
                    ++ "</ERROR>"
    ind i = replicate (4 * i) ' '

instance Functor (ST err) where
    fmap f st =
        case st of
            STConstr x sts -> STConstr x (map (fmap f) sts)
            STLeaf x -> STLeaf (f x)
            STErr err -> STErr err

stFold ::
    (String -> [b] -> b) ->
    (a -> b) ->
    (err -> b) ->
    ST err a ->
    b
stFold f g h st = case st of
    STConstr x sts -> f x (map (stFold f g h) sts)
    STLeaf x -> g x
    STErr err -> h err
