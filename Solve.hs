{-# Language OverloadedStrings, TypeSynonymInstances, FlexibleInstances#-}

module Solve
    (solve)
where

import Expression
import Simplify
import Data.List (partition)
import Debug.Trace (traceShow)

p x = traceShow x x

solve :: Name -> Expression -> [Expression]
solve name = solve2 name . canonicalize

solve2 :: Name -> Expression -> [Expression]
solve2 name exp = case expand exp of
        Var x -> [Num 0]
        Num y -> [Num y]
        Power bottom top
            | containsVariable name bottom -> do
                            solution <- (solve2 name bottom)
                            return $ Power solution (1/top)
            | otherwise -> error "Solver not implemented"
        Prod stuff -> solveProd name (partition (containsVariable name) stuff)
        _ -> error "Solver not implemented"

solveProd :: Name -> ([Expression], [Expression]) -> [Expression]
solveProd name (terms_with_name, others) = case (terms_with_name) of
                    [] -> error "Solver has an error in it"
                    [x] -> do
                            solution <- solve2 name x
                            return $ Prod (solution:(map (\x -> 1/x) others))
                    _ -> error "Probably can't solve"

containsVariable :: Name -> Expression -> Bool
containsVariable name exp = case exp of
        Var x -> x == name
        Num _ -> False
        Power bottom top -> containsVariable name bottom ||
                                 containsVariable name top
        Sum stuff -> any (containsVariable name) stuff
        Prod stuff -> any (containsVariable name) stuff

--collect sums:
-- [x, x**2, 2**x, y*x, 2, 4*x**2]
--becomes

-- [x : 1+y,
-- x**2 : 1+4,
-- 2**x : 1,
-- 1 : 2]

--now.

extractVariableFromExpressions :: Name -> Expression -> (Expression,Expression)
extractVariableFromExpressions = undefined

collectSums :: Name -> [Expression] -> ([(Expression, Expression)], Expression)
collectSums name stuff = undefined

-- ([("m", "g"*"h"*"EK"**-1)], -1)

--solveSum :: Name -> [Expression] -> [Expression]
--solveSum name stuff = case (collectSums stuff) of
--            ([()]) -> undefined

main = print (solve2 ("x" :: Name) ("x" + "x") ("y"))