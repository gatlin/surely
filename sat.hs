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
    | sat res = res
    | otherwise = dpll $ SolverState (simplify f n) (n:r)
    where
        s1 = unitpropagate s
        f = formula s1
        r = record s1
        l = chooseLiteral f
        rl = l:r
        res = dpll $ SolverState (simplify f l) rl
        n = l * (-1)

unitpropagate :: SolverState -> SolverState
unitpropagate (SolverState [] r) = SolverState [] r
unitpropagate s
    | containsEmpty f || not (unit f) = SolverState f r
    | otherwise = unitpropagate $ SolverState (sfl) (lr)
    where
        f = formula s
        r = record s
        l = chooseUnit f
        sfl = simplify f l
        lr = l:r

chooseLiteral :: Formula -> Literal
chooseLiteral ([]:xs) = chooseLiteral xs
chooseLiteral (x:xs) = (\(y:ys) -> y) x

simplify :: Formula -> Literal -> Formula
simplify [] l = []
simplify f l = [ simpClause x l | x <- f, not (clauseSat x l) ]

simpClause :: Clause -> Literal -> Clause
simpClause [] l = []
simpClause c l = [ x | x <- c, x /= l * (-1) ]

clauseSat :: Clause -> Literal -> Bool
clauseSat [] l = False
clauseSat c l = or $ [ x == l | x <- c ]

containsEmpty :: Formula -> Bool
containsEmpty [] = False
containsEmpty f = or $ [ x == [] | x <- f ]

chooseUnit :: Formula -> Literal
chooseUnit f = let c = head f
               in if 1 == (length c) then head c else chooseUnit $ tail f

unit :: Formula -> Bool
unit [] = False
unit f = and $ [ (\(y:ys) -> ys == []) x | x <- f ]

solve :: [[Integer]] -> (Bool,[Integer])
solve f = let result = dpll (SolverState f [])
          in (sat result, answer result)