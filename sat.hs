import Data.Maybe
import Control.Monad

type Literal = Integer
type Clause = [Literal]
type Formula = [Clause]
type Record = [Literal]

data SolverState = SolverState { formula :: Formula
                               , record :: Record
                               } deriving (Show)

dpll :: SolverState -> Maybe Record
dpll (SolverState [] r) = return r
dpll s =
    if null f then return r
    else do
        l  <- chooseLiteral f
        case dpll (SolverState (simplify f l) (l:r)) of
            Just record -> return record
            Nothing -> dpll $ SolverState (simplify f n) (n:r)
                where n = -l
    where
        s' = unitpropagate s
        f = formula s'
        r = record s'

unitpropagate :: SolverState -> SolverState
unitpropagate (SolverState f r) =
    case getUnit f of
        Nothing -> SolverState f r
        Just u -> unitpropagate $ SolverState (simplify f u) (u:r)

chooseLiteral :: Formula -> Maybe Literal
chooseLiteral = listToMaybe . concat

getUnit :: Formula -> Maybe Literal
getUnit xs = listToMaybe [ x | [x] <- xs ]

simplify :: Formula -> Literal -> Formula
simplify f l = [ simpClause x l | x <- f, not (clauseSat x l) ]

simpClause :: Clause -> Literal -> Clause
simpClause = flip (.) ((/=) . negate) . flip filter

clauseSat :: Clause -> Literal -> Bool
clauseSat = flip elem

solve :: [[Integer]] -> Maybe [Integer]
solve = dpll . flip SolverState []
