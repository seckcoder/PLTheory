module Var (
    Var(..)
    ) where

data Var = Var { _varname :: String
               , _varindex :: Int }

instance Eq Var where
    (==) (Var _ n1) (Var _ n2) = n1 == n2

instance Ord Var where
    compare (Var _ n1) (Var _ n2) = compare n1 n2