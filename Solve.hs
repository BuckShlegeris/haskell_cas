{-# Language OverloadedStrings, TypeSynonymInstances, FlexibleInstances#-}

module Solve
    (solve)
where

import Expression
import Simplify

solve :: Name -> Expression -> [Expression]
solve name exp = case exp of
        Var x -> []
        Num y -> [Num y]
        Power bottom top -> error "Solver not implemented"
        _ -> error "Solver not implemented"

contains_variable :: Name -> Expression -> Bool
contains_variable name exp = case exp of
        Var x -> x == name
        Num _ -> False
        Power bottom top -> contains_variable name bottom ||
                                 contains_variable name top
        Sum stuff -> any (contains_variable name) stuff
        Prod stuff -> any (contains_variable name) stuff

