module AST where

type Variable = String

data Term o = Var Variable
            | Op o [Term o]

-- for ast, just find the variable with the same name and replace it
subst :: Term o -> Variable -> Term o
subst = undefined