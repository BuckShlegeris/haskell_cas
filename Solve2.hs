{-# Language OverloadedStrings, TypeSynonymInstances, FlexibleInstances#-}

module Solve2
    (solve)
where

import Expression
import Simplify
import Data.List (partition)
import Debug.Trace (traceShow)

p x = traceShow x x

-- Usage: solve "v" ("EK" - "m"*"v"**2)
-- solve2 "v"  [0] ("EK" - "m"*"v"**2)
-- solve2 "v"  ["EK"] "m"*"v"**2
-- solve2 "v"  ["EK"/"m"] "v"**2
-- solve2 "v"  [("EK"/"m")**0.5, -("EK"/"m")**0.5]  "v"
-- Done!

--- The name variable
solve :: Name -> Expression -> Expression
solve name equation = solve2 name 0 (canonicalize equation)

solve2 :: Name -> Expression -> Expression -> Expression
solve2 name lhs rhs = case p rhs of
        Var x
              | name == x -> lhs
              | otherwise -> error "your solver is fucked"
        Num _ -> error "your solver is fucked"
        Power bottom top
            | containsVariable name bottom && containsVariable name top
                                           -> error "you're fucked"
            | containsVariable name bottom ->
                  traceShow (lhs,rhs) $ solve2 name
                                  (Power rhs (Power top (-1))) bottom
            | otherwise -> error "Solver not implemented"
        Prod stuff -> undefined
        _ -> undefined

containsVariable :: Name -> Expression -> Bool
containsVariable name exp = case exp of
        Var x -> x == name
        Num _ -> False
        Power bottom top -> containsVariable name bottom ||
                                 containsVariable name top
        Sum stuff -> any (containsVariable name) stuff
        Prod stuff -> any (containsVariable name) stuff

main = print $ solve2 "x" "y" "mother"*3