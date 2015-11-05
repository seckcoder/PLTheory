{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module ABT (
    View(..),
    into,
    out,
    aequiv,
    newvar,
    bind,
    unbind,
    Term
) where

import Control.Applicative
import Control.Monad.State
import qualified Data.HashSet as S
import qualified Data.List as L
import Var
import Operator
import Data.List.Util

data Term o = Free Var
            | Bound Int
            | Abs (Term o)
            | Op o [Term o]

data View o = V Var
            | L Var (Term o)
            | A o [Term o]


newvar :: String -> State Int Var
newvar s = do
    n <- get
    put (n+1)
    return $ Var s (n+1)

bind :: (Operator o) => Var -> Term o -> Term o
bind v t = go 0 t
    where go d t = case t of
            Free v' | v == v' -> Bound d -- this is a bound variable
            Free _ -> t
            Bound _ -> t
            Abs t' -> Abs (go (d+1) t')
            Op o ts -> Op o (map (go d) ts)
            
unbind :: (Operator o) => Term o -> State Int (Var, Term o)
unbind t = do
    v <- newvar "y"
    let go d t = case t of
            Free v' | v' == v -> error "Malformed"
            Free _ -> t
            Bound n | n == 0 -> Free v
            Bound _ -> t
            Abs t' -> Abs (go (d+1) t')
            Op o ts -> Op o (map (go d) ts)
    return (v, go 0 t)

into :: (Operator o) => View o -> Term o
into view = case view of
        V v -> Free v
        L v t -> Abs (bind v t)
        A f args ->
            if (length $ arity f) == length args
            then Op f args
            else error "Malformed"

out :: (Operator o) => Term o -> State Int (View o)
out t = case t of
        Free v -> return $ V v
        Bound n -> error "Malformed Bound Variable"
        Abs t' -> do
            (v, t') <- unbind t'
            return $ L v t'
        Op o ts -> return $ A o ts

aequiv :: (Eq o, Operator o) => Term o -> Term o -> Bool
aequiv t1 t2 = case (t1,t2) of
        (Free v1, Free v2) | v1 == v2 -> True
        (Bound n1, Bound n2) | n1 == n2 -> True
        (Abs t1, Abs t2) | aequiv t1 t2 -> True
        (Op o1 ts1, Op o2 ts2) | o1 == o2 && all (uncurry aequiv) (zip ts1 ts2) -> True
        (_,_) -> False