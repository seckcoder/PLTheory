module ABTUtil (
    subst,
    freevars,
    var,
    lam,
    app
) where


import Var
import ABT
import Operator
import qualified Data.List as L
import Control.Applicative
import Control.Monad.State
import Data.List.Util

var :: (Operator o) => Var -> Term o
var v = into (V v)
lam :: (Operator o) => Var -> Term o -> Term o
lam v t = into (L v t)
app :: (Operator o) => o -> [Term o] -> Term o
app o ts = into (A o ts)

freevars :: (Operator o) => Term o -> State Int [Var]
freevars t = do
    view <- out t
    let go view = case view of
            V v -> return $ [v]
            L v t -> L.delete v <$> freevars t
            A f ts -> concatM ts -- TODO: how to fix this?
        concatM [] = return []
        concatM (t:ts) = do
            vs1 <- freevars t
            vs2 <- concatM ts
            return $ vs1 ++ vs2
    unique <$> go view

subst :: (Operator o) => Term o -> Var -> Term o -> State Int (Term o)
subst t v e = do
    view <- out e
    case view of
        V v' | v' == v -> return t
        V v' -> return $ var v'
        -- Impossible to get such error since each time the bound variables are chosen fresh
        -- So v' can't equal to v
        L v' t' | v' == v -> error "Impossible Error"
        L v' t' -> (lam v') <$> subst t v t'
        A o ts -> (app o) <$> mapM (subst t v) ts