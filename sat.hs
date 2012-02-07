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
dpll s = let s1 = unitpropagate s
             f  = formula s1
             r  = record s1
         in if empty f then Result True r
            else if containsEmpty f then Result False []
                 else let l  = chooseLiteral f
                          rl = l:r
                          res= dpll (SolverState (simplify f l) rl)
                      in if sat res then res
                         else let n = l * (-1)
                              in dpll $ SolverState (simplify f n) (n:r)

empty :: [a] -> Bool
empty l
    | length l == 0 = True
    | otherwise     = False

unitpropagate :: SolverState -> SolverState
unitpropagate s = let prop f r = if (empty f) || (containsEmpty f) || not (unit f)
                                 then SolverState f r
                                 else let l = chooseUnit f
                                      in prop (simplify f l) (l:r)
                      f = formula s
                      r = record s
                  in prop f r

chooseLiteral :: Formula -> Literal
chooseLiteral f = let c = head f
                  in if not (empty c) then head c else chooseLiteral (tail f)

simplify :: Formula -> Literal -> Formula
simplify f l = let simp f l g = if empty f then g
                                else let c = head f
                                         r = tail f
                                     in if clauseSat c l then simp r l g
                                        else simp r l ((simpClause c l):g)
               in simp f l []

clauseSat :: Clause -> Literal -> Bool
clauseSat c l = if empty c then False
                else let m = head c
                     in if l == m then True
                        else clauseSat (tail c) l

simpClause :: Clause -> Literal -> Clause
simpClause c l = let sc c l d = if empty c then d
                                else let m = head c
                                         r = tail c
                                     in if l == (m * (-1)) then sc r l d
                                        else sc r l (m:d)
                 in sc c l []

containsEmpty :: Formula -> Bool
containsEmpty [] = False
containsEmpty f = let c = head f
                  in if empty c then True else containsEmpty (tail f)

chooseUnit :: Formula -> Literal
chooseUnit f = let c = head f
               in if 1 == (length c) then head c else chooseUnit $ tail f

unit :: Formula -> Bool
unit [] = False
unit f = let c = head f
             r = tail f
         in if (length c) == 1 then True else unit r
