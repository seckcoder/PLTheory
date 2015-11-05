{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}


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

import           Control.Applicative
import           Control.Monad.State
import qualified Data.HashSet        as S
import qualified Data.List           as L
import           Data.List.Util
import           Operator
import           Var

-- | The inner representation of the Abstract Binding Tree
-- o is type of operator
data Term o = Free Var        -- free variable
            | Bound Int       -- bound variable. Int represents number of hops between the binder and the occurrence of the variable
            | Abs (Term o)    -- Abstraction. Each abstraction introduce one bound variable.
            | Op o [Term o]   -- Operator. 

-- | The outer representation of teh ABT
data View o = V Var             -- Free Variable
            | L Var (Term o)
            | A o [Term o]


newvar :: String -> State Int Var
newvar s = do
    n <- get
    put (n+1)
    return $ Var s (n+1)

-- | Convert \ x -> t to Abs t
-- For variable equal to v, we change it to bound variable
bind :: (Operator o) => Var -> Term o -> Term o
bind v t = go 0 t
    where go d t = case t of
            Free v' | v == v' -> Bound d -- variable bound to v
            Free _ -> t  -- free variable
            Bound _ -> t -- variable not bound to v
            Abs t' -> Abs (go (d+1) t')  -- for each abstraction, we increase one hop
            Op o ts -> Op o (map (go d) ts)  -- operator

-- | Convert Abs t to \ x -> t'
-- If some variable in t is bound to x, then it becomes free variable in t'
-- Each time, we generate freshly named variable when unbind an abstraction. This makes
-- substituion much easier to implement.
unbind :: (Operator o) => Term o -> State Int (Var, Term o)
unbind t = do
    v <- newvar "y"  -- a freshly generated variable
    let go d t = case t of
            Free v' | v' == v -> error "Malformed"
            Free _ -> t
            Bound n | n == d -> Free v  -- bound to v
            Bound _ -> t  -- not bound to v
            Abs t' -> Abs (go (d+1) t')
            Op o ts -> Op o (map (go d) ts)
    return (v, go 0 t)

-- | converts view to term
into :: (Operator o) => View o -> Term o
into view = case view of
        V v -> Free v
        L v t -> Abs (bind v t)
        A f args ->
            if (length $ arity f) == length args
            then Op f args
            else error "Malformed"

-- | converts term to view
out :: (Operator o) => Term o -> State Int (View o)
out t = case t of
        Free v -> return $ V v
        -- Bound variable should only exist in `Abs`. But when we encounter
        -- `Abs`, we already change the bound variable(bound to current `Abs`) to free variable
        -- The if we find a bound variable, it's malformed.
        Bound n -> error "Malformed Bound Variable"
        Abs t' -> do
            (v, t') <- unbind t'
            return $ L v t'
        Op o ts -> return $ A o ts

-- | alpha equivalence
aequiv :: (Eq o, Operator o) => Term o -> Term o -> Bool
aequiv t1 t2 = case (t1,t2) of
        (Free v1, Free v2) | v1 == v2 -> True
        (Bound n1, Bound n2) | n1 == n2 -> True
        (Abs t1, Abs t2) | aequiv t1 t2 -> True
        (Op o1 ts1, Op o2 ts2) | o1 == o2 && all (uncurry aequiv) (zip ts1 ts2) -> True
        (_,_) -> False