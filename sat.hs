import Data.Maybe
import Control.Applicative
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
dpll (SolverState f r) = do
    case chooseLiteral f of
        Just l -> do
            case dpll (SolverState (simplify f l) (l:r)) of
                Just record -> return record
                Nothing -> do
                    n <- return $ negate l
                    dpll (SolverState (simplify f n) (n:r))
        Nothing -> Nothing

unitpropagate :: SolverState -> Maybe SolverState
unitpropagate (SolverState f r) = do
    case getUnit f of
        Nothing -> return $ SolverState f r
        Just u -> unitpropagate $ SolverState (simplify f u) (u:r)

chooseLiteral :: Formula -> Maybe Literal
chooseLiteral xs = listToMaybe [ x | x:_ <- xs ]

simplify :: Formula -> Literal -> Formula
simplify [] l = []
simplify f l = [ simpClause x l | x <- f, not (clauseSat x l) ]

simpClause :: Clause -> Literal -> Clause
simpClause [] l = []
simpClause c l = [ x | x <- c, x /= -l ]

clauseSat :: Clause -> Literal -> Bool
clauseSat [] l = False
clauseSat c l = or [ x == l | x <- c ]

getUnit :: Formula -> Maybe Literal
getUnit xs = listToMaybe [ x | [x] <- xs ]

solve :: [[Integer]] -> Maybe [Integer]
solve f = dpll $ SolverState f []
