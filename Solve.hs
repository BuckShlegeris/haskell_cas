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
        Var x -> [Num 1]
        Num y -> [Num y]
        Power bottom top
            | contains_variable name bottom -> do
                            solution <- (solve2 name bottom)
                            return $ Power solution (1/top)
            | otherwise -> error "Solver not implemented"
        Prod stuff -> solveProd name (partition (contains_variable name) stuff)
        _ -> error "Solver not implemented"

solveProd :: Name -> ([Expression], [Expression]) -> [Expression]
solveProd name (terms_with_name, others) = case (terms_with_name) of
                    [] -> error "Solver has an error in it"
                    [x] -> do
                            solution <- solve2 name x
                            return $ Prod (solution:(map (\x -> 1/x) others))
                    _ -> error "Probably can't solve"


contains_variable :: Name -> Expression -> Bool
contains_variable name exp = case exp of
        Var x -> x == name
        Num _ -> False
        Power bottom top -> contains_variable name bottom ||
                                 contains_variable name top
        Sum stuff -> any (contains_variable name) stuff
        Prod stuff -> any (contains_variable name) stuff

main = print (solve ("x" :: Name) $ "x" * 2)