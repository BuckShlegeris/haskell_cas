{-# Language OverloadedStrings, TypeSynonymInstances, FlexibleInstances#-}

module Expression
    (Expression (..),
    isConstant,
    Name)

where

import Data.String (IsString, fromString)
import Data.List (intersperse)

type Name = String

data Expression = Num Double
                | Var Name
                | Sum [Expression]
                | Prod [Expression]
                | Power Expression Expression
        deriving (Eq, Ord)

instance Show Expression where
    show exp = case exp of
        Num x -> show x
        Var name -> name
        Sum stuff -> "(" ++ concat (intersperse "+" (map show stuff)) ++ ")"
        Prod stuff -> "(" ++ concat (intersperse "*" (map show stuff)) ++ ")"
        Power bottom top -> "(" ++ show bottom ++ "**" ++ show top ++ ")"

instance Num Expression where
    a + b = Sum [a, b]
    a - b = Sum [a, Prod[-1,b]]
    a * b = Prod [a,b]
    fromInteger x = Num (fromIntegral x)
    abs = id
    signum x = 1

instance Fractional Expression where
    a / b = a * (Power b (-1))
    fromRational x = undefined

instance Floating Expression where
    a ** b = Power a b
    pi = undefined
    exp = undefined
    log = undefined
    sin = undefined
    cos = undefined
    asin = undefined
    acos = undefined
    atan = undefined
    sinh = undefined
    cosh = undefined
    atanh = undefined
    asinh = undefined
    acosh = undefined

instance IsString Expression
    where fromString x = Var x

isConstant (Num x) = True
isConstant _ = False

fromConstant (Num x) = x

