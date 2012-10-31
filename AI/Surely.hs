{-# LANGUAGE BangPatterns #-}

module AI.Surely (solve) where
import Data.Maybe
import Control.Monad

type Literal = Integer
type Clause = [Literal]
type Formula = [Clause]
type Record = [Literal]

data SolverState = SolverState { formula :: !Formula
                               , record  :: !Record
                               } deriving (Show)

dpll :: SolverState -> Maybe Record
dpll (SolverState [] r) = return r
dpll s
    | null f = return r
    | otherwise = do
        l  <- chooseLiteral f
        case dpll (SolverState (simplify f l) (l:r)) of
            Just record -> return record
            Nothing -> dpll $! SolverState (simplify f (-l)) ((-l):r)
    where
        s' = unitpropagate s
        {-# INLINE s' #-}
        f = formula s'
        {-# INLINE f  #-}
        r = record s'
        {-# INLINE r  #-}

unitpropagate :: SolverState -> SolverState
unitpropagate (SolverState f r) =
    case getUnit f of
        Nothing -> SolverState f r
        Just u -> unitpropagate $ SolverState (simplify f u) (u:r)

chooseLiteral :: Formula -> Maybe Literal
chooseLiteral !f = listToMaybe . concat $! f

getUnit :: Formula -> Maybe Literal
getUnit !xs = listToMaybe [ x | [x] <- xs ]

simplify :: Formula -> Literal -> Formula
simplify !f !l = [ simpClause x l | x <- f, not (elem l x) ]
    where
        simpClause c l = filter (/= -l) c
        {-# INLINE simpClause #-}

solve :: [[Integer]] -> Maybe [Integer]
solve = dpll . flip SolverState []
