module Operator (
    Operator(..)
    ) where

class (Show o, Eq o) => Operator o where
  arity :: o -> [Int]