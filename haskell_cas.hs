{-# Language OverloadedStrings, TypeSynonymInstances, FlexibleInstances#-}

import Data.List
import Data.String
import Control.Monad
import Debug.Trace (traceShow)
import qualified Data.Map as M

p x = traceShow x x

type Name = String

data Expression = Num Integer
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
    fromInteger x = Num x
    abs = id
    signum x = 1

instance IsString Expression
    where fromString x = Var x

isConstant (Num x) = True
isConstant _ = False

diff :: Name -> Expression -> Expression
diff var exp = case exp of
    Num _ -> 0
    Var name
        | var == name -> 1
        | otherwise -> 0
    Sum stuff -> Sum (map (diff var) stuff)
    Prod [] -> 0
    Prod [x] -> diff var x
    Prod (x:xs) -> (diff var x * Prod xs) + (x * diff var (Prod xs))

canonicalize :: Expression -> Expression
canonicalize exp = case exp of
    Num x -> Num x
    Var x -> Var x
    Sum stuff -> canonicalizeSum stuff
    Prod stuff -> canonicalizeProduct stuff
    Power bottom top -> Power bottom top

canonicalizeProduct :: [Expression] -> Expression
canonicalizeProduct stuff = packageExpsIntoProduct $ collectIntoPowers
    $ simplifyTerms $ sort $ flattenProduct $ map canonicalize $ stuff
    where
        flattenProduct :: [Expression] -> [Expression]
        flattenProduct [] = []
        flattenProduct (Prod stuff:xs) = stuff ++ flattenProduct xs
        flattenProduct (x:xs) = x : flattenProduct xs

        simplifyTerms :: [Expression] -> [Expression]
        simplifyTerms terms = if coefficient==1
                              then otherTerms
                              else (Num coefficient : otherTerms)
            where (coefficient, otherTerms) = takeOutConstantFactor terms

        collectIntoPowers :: [Expression] -> [Expression]
        collectIntoPowers terms = map (\(n,x) ->
                if n==1 then x else Power x (Num n)) $
                            runLengthEncode terms

packageExpsIntoProduct :: [Expression] -> Expression
packageExpsIntoProduct exps = case exps of
    [] -> 1
    [x] -> x
    _ -> Prod exps

canonicalizeSum :: [Expression] -> Expression
canonicalizeSum = packageTerms . packageMap . collectTerms . extractConstants
           . flattenSum . sort . map canonicalize
    where
        flattenSum :: [Expression] -> [Expression]
        flattenSum [] = []
        flattenSum (Sum stuff:xs) = stuff ++ flattenSum xs
        flattenSum (x:xs) = x : flattenSum xs

        packageTerms :: [Expression] -> Expression
        packageTerms terms = case terms of
            [] -> 0
            [x] -> x
            _ -> Sum terms

        packageMap :: M.Map Expression Integer -> [Expression]
        packageMap mapping = do
            (k, v) <- M.toList mapping
            guard $ v /= 0
            guard $ k /= Prod []
            return $ if v == 1 then k else Prod [Num v,k]


        collectTerms :: [(Integer, Expression)] -> M.Map Expression Integer
        collectTerms exps = foldl (<<) M.empty exps

extractConstants :: [Expression] -> [(Integer, Expression)]
extractConstants x = map extractConstant x
    where
        extractConstant :: Expression -> (Integer, Expression)
        extractConstant (Prod stuff) = (x,packageExpsIntoProduct y)
                where (x,y) = takeOutConstantFactor stuff
        extractConstant (Num x) = (x, 1)
        extractConstant exp = (1,exp)


runLengthEncode :: Eq x => [x] -> [(Integer, x)]
runLengthEncode [] = []
runLengthEncode (x:xs) = runLengthEncode' x 1 xs
    where
        runLengthEncode' :: Eq x => x -> Integer -> [x] -> [(Integer, x)]
        runLengthEncode' x n [] = [(n,x)]
        runLengthEncode' x n (y:ys)
            | x == y    = runLengthEncode' x (n+1) ys
            | otherwise = (n,x):runLengthEncode' y 1 ys

takeOutConstantFactor :: [Expression] -> (Integer, [Expression])
takeOutConstantFactor terms = (coefficient, otherTerms)
    where
        (nums, otherTerms) = partition isConstant terms
        coefficient = product $ map (\(Num x) -> x) nums

distribute :: [Expression] -> [Expression] -> [Expression]
distribute xs ys = do
        x <- xs
        y <- ys
        return $ x * y

(<<) :: M.Map Expression Integer -> (Integer, Expression) -> M.Map Expression Integer
mapping << (n,item) = M.insert item (n + M.findWithDefault 0 item mapping) mapping


main = return $ canonicalize $ ("x" + "y") * ("x" + "y")