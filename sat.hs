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
dpll (SolverState [[l]] r) = return (l:r)
dpll s = do
    s' <- unitpropagate s
    f  <- return $ formula s'
    r  <- return $ record s'
    l  <- chooseLiteral f
    n  <- return $ negate l
    let resl = dpll $ SolverState (simplify f l) (l:r)
    let resn = dpll $ SolverState (simplify f n) (n:r)
    case resl of
        Just record -> return record
        Nothing -> resn

unitpropagate :: SolverState -> Maybe SolverState
unitpropagate s = do
    f <- return $ formula s
    r <- return $ record s
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

