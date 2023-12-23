module Quenta.BinopProps where

data BinopPropsAssoc
    = BinopAssocLeft BinopPropsAppKind
    | BinopAssocRight BinopPropsAppKind
    | BinopAssocList
    | BinopAssocNone BinopPropsAppKind
    deriving (Show, Eq)

data BinopPropsAppKind
    = BinopAppCurried
    | BinopAppBuiltin
    | BinopAppTuple
    deriving (Show, Eq)

instance Read BinopPropsAppKind where
    readsPrec _ ('c':xs) = [(BinopAppCurried, xs)]
    readsPrec _ ('b':xs) = [(BinopAppBuiltin, xs)]
    readsPrec _ ('t':xs) = [(BinopAppTuple, xs)]
    readsPrec _ _ = []

instance Read BinopPropsAssoc where
    readsPrec i ('l':xs) = do
        (a, xs0) <- readsPrec i xs
        return (BinopAssocLeft a, xs0)
    readsPrec i ('r':xs) = do
        (a, xs0) <- readsPrec i xs
        return (BinopAssocRight a, xs0)
    readsPrec i ('n':xs) = do
        (a, xs0) <- readsPrec i xs
        return (BinopAssocNone a, xs0)
    readsPrec i (',':xs) = [(BinopAssocList, xs)]

data BinopProps = BP
    { binopConstr :: String
    , binopOps :: [String]
    , binopAssoc :: BinopPropsAssoc
    }
    deriving (Show)

binop :: String -> [String] -> String -> BinopProps
binop constr [] _ = error "Cannot declare BinopProps without any ops"
binop constr ops assoc = BP constr ops (read assoc)
