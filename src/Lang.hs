module Lang (
    TermOps(..)
) where

    
import Operator

data TermOps = Num Int
             | Plus
             | Let deriving (Show, Eq)

instance Operator TermOps where
    arity o = case o of
            Num _ -> []
            Plus -> [0,0]
            Let -> [0,1]