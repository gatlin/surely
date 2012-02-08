import Data.Maybe

type Literal = Integer
type Clause = [Literal]
type Formula = [Clause]
type Record = [Literal]

data SolverState = SolverState { formula :: Formula
                               , record :: Record
                               } deriving (Show)

data Result = Result { sat :: Bool
                     , answer :: Record
                     } deriving (Show)

dpll :: SolverState -> Result
dpll (SolverState [] r) = Result True r
dpll s
    | containsEmpty f = Result False []
    | sat resl = resl
    | otherwise = resn
    where
        s1 = unitpropagate s
        f = formula s1
        r = record s1
        l = (fromJust . chooseLiteral) f
        runDpll a = dpll $ SolverState (simplify f a) (a:r)
        resl = runDpll l
        resn = runDpll (-l)

unitpropagate :: SolverState -> SolverState
unitpropagate (SolverState [] r) = SolverState [] r
unitpropagate s
    | containsEmpty f || isNothing u = SolverState f r
    | otherwise = unitpropagate $ SolverState (simplify f (fromJust u)) ((fromJust u):r)
    where
        f = formula s
        r = record s
        u = getUnit f

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

containsEmpty :: Formula -> Bool
containsEmpty [] = False
containsEmpty f = or [ x == [] | x <- f ]

getUnit :: Formula -> Maybe Literal
getUnit xs = listToMaybe [ x | [x] <- xs ]

solve :: [[Integer]] -> (Bool,[Integer])
solve f = let result = dpll (SolverState f [])
          in (sat result, answer result)