{-# LANGUAGE BangPatterns #-}

-- |
-- Module           : AI.Surely
-- Copyright        : 2012 Gatlin Johnson
-- License          : LGPL 3.0
-- Maintainer       : rokenrol@gmail.com
-- Stability        : experimental
-- Portability      : GHC
--
-- Simple SAT and (soon!) SMT algorithms exploiting the properties of Maybe.
--
-- Based on the work at:
-- https://cs.uwaterloo.ca/~david/cl/dpll-abstract.pdf.

module AI.Surely (solve) where
import Data.Maybe

type Literal = Integer
type Clause  = [Literal]
type Formula = [Clause]
type Record  = [Literal]

-- | The state of a solver at any given time is a subset of the original
--   formula and a record of assignments; that is, a list of the literals
--   considered to be true.
data SolverState = SolverState { formula :: !Formula
                               , record  :: !Record
                               } deriving (Show)

-- | The core algorithm, a simple back-tracking search with unitpropagation.
dpll :: SolverState -> Maybe Record
dpll s
    | null f    = Just r
    | contra f  = Nothing
    | otherwise = oneOf l (-l)
    where
        l  = chooseLiteral f
        {-# INLINE l #-}
        s' = unitpropagate s
        {-# INLINE s' #-}
        f = formula s'
        {-# INLINE f  #-}
        r = record s'
        {-# INLINE r  #-}
        oneOf !a !b = listToMaybe . catMaybes . map try $ [a,b]
        {-# INLINE oneOf #-}
        try !lit =
            dpll $! SolverState (simplify f lit) (lit:r)
        {-# INLINE try #-}

-- | unitpropagate simplifies the formula for every variable in a unit clause
--   (that is, a clause with only one unit).
unitpropagate :: SolverState -> SolverState
unitpropagate (SolverState f r) =
    case getUnit f of
        Nothing -> SolverState f r
        Just u -> unitpropagate $ SolverState (simplify f u) (u:r)

-- | Returns a `Just Literal` or Nothing if the formula has a contradiction.
--   If this yields `Nothing` then the algorithm will backtrack.
chooseLiteral :: Formula -> Literal
chooseLiteral = head . head

-- | Returns `True` if the formula is a contradiction, i.e. if there is an
-- empty clause (that cannot be satisfied).
contra :: Formula -> Bool
contra = any null

-- | If a unit clause (singleton list) exists in the formula, return the
--   literal inside it, or Nothing.
getUnit :: Formula -> Maybe Literal
getUnit !xs = listToMaybe [ x | [x] <- xs ]

-- | Simplifying a formula `f` wrt a literal `l` means, for every clause in
--   which `-l` is a member remove `-l`, and remove every clause from f which
--   contains `l`.
--
--   Reasoning: a disjunction with a false value does not need to
--   consider that value, and a disjunction with a true value is trivially
--   satisfied.
simplify :: Formula -> Literal -> Formula
simplify !f !l = [ simpClause x l | x <- f, not (elem l x) ]
    where
        simpClause c' l' = filter (/= -l') c'
        {-# INLINE simpClause #-}

-- | The top-level function wrapping `dpll` and hiding the library internals.
--   Accepts a list of lists of Integers, treating the outer list as a
--   conjunction and the inner lists as disjunctions.
solve :: [[Integer]]     -- ^ CNF formula, represented as [[Integer]]
      -> Maybe [Integer] -- ^ Just a list of literals considered true, or Nothing
solve = dpll . flip SolverState []
{-# INLINE solve #-}
