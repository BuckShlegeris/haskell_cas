{-# Language OverloadedStrings, TypeSynonymInstances, FlexibleInstances#-}

module Calculus
    (diff)

where

import Expression
import Simplify

diff :: Name -> Expression -> Expression
diff var exp = expand $ canonicalize (diff' var exp)
    where
    diff' :: Name -> Expression -> Expression
    diff' var exp = case exp of
        Num _ -> 0
        Var name
            | var == name -> 1
            | otherwise -> 0
        Sum stuff -> Sum (map (diff' var) stuff)
        Prod [] -> 0
        Prod [x] -> diff' var x
        Prod (x:xs) -> (diff' var x * Prod xs) + (x * diff' var (Prod xs))
        Power bottom top
            | isConstant top -> top * Power bottom (top - 1)