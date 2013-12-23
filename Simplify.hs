{-# Language OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
-- {-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Simplify
    (canonicalize,
    expand)
where

import Data.List (sort, partition)
import Debug.Trace (traceShow)
import qualified Data.Map as M
import Expression

p x = traceShow x x

canonicalize :: Expression -> Expression
canonicalize exp = case exp of
    Num x -> Num x
    Var x -> Var x
    Sum stuff -> canonicalizeSum (map canonicalize stuff)
    Prod stuff -> canonicalizeProduct (map canonicalize stuff)
    Power bottom top -> canonicalizePower (canonicalize bottom) (canonicalize top)

canonicalizePower :: Expression -> Expression -> Expression
canonicalizePower bottom top = case (bottom, top) of
                (0, 0)                  -> 1 -- THIS IS REALLY SKETCHY
                (0, _)                  -> 0
                (1, _)                  -> 1
                (_, 0)                  -> 1
                (_, 1)                  -> bottom
                (Power bottom2 top2, _) -> (Power bottom2 (canonicalize $ top * top2))
                (_, _)                  -> Power bottom top



canonicalizeProduct :: [Expression] -> Expression
canonicalizeProduct stuff = packageExpsIntoProduct $ collectIntoPowers
    $ simplifyTerms $ sort $ flattenProduct $ stuff
    where
        flattenProduct :: [Expression] -> [Expression]
        flattenProduct [] = []
        flattenProduct (Prod stuff:xs) = stuff ++ flattenProduct xs
        flattenProduct (x:xs) = x : flattenProduct xs

        simplifyTerms :: [Expression] -> [Expression]
        simplifyTerms terms
                | coefficient == 1 = otherTerms
                | coefficient == 0 = []
                | otherwise = (Num coefficient : otherTerms)
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


packageTermsInSum :: [Expression] -> Expression
packageTermsInSum terms = case terms of
    [] -> 0
    [x] -> x
    _ -> Sum terms

canonicalizeSum :: [Expression] -> Expression
canonicalizeSum = packageTermsInSum . packageMap . collectTerms . extractConstants
           . flattenSum . sort
    where
        flattenSum :: [Expression] -> [Expression]
        flattenSum [] = []
        flattenSum (Sum stuff:xs) = stuff ++ flattenSum xs
        flattenSum (x:xs) = x : flattenSum xs

        packageMap :: M.Map Expression Double -> [Expression]
        packageMap mapping = concatMap packageItem (M.toList mapping)
            where
                packageItem (k,v) = case (k,v) of
                    (_,0) -> []
                    (Prod [], v) -> [Num v]
                    (k, 1) -> [k]
                    (Num x, y) -> [Num (x*y)]
                    (Prod x,y) -> [Prod (Num y:x)]
                    (x,y) -> [Prod [x,Num y]]

        collectTerms :: [(Double, Expression)] -> M.Map Expression Double
        collectTerms exps = foldl (<<) M.empty exps
            where
                (<<) :: M.Map Expression Double -> (Double, Expression) ->
                                    M.Map Expression Double
                mapping << (n,item) = M.insert item
                                (n + M.findWithDefault 0 item mapping) mapping

extractConstants :: [Expression] -> [(Double, Expression)]
extractConstants x = map extractConstant x
    where
        extractConstant :: Expression -> (Double, Expression)
        extractConstant (Prod stuff) = (x,packageExpsIntoProduct y)
                where (x,y) = takeOutConstantFactor stuff
        extractConstant (Num x) = (x, 1)
        extractConstant exp = (1,exp)


runLengthEncode :: Eq x => [x] -> [(Double, x)]
runLengthEncode [] = []
runLengthEncode (x:xs) = runLengthEncode' x 1 xs
    where
        runLengthEncode' :: Eq x => x -> Double -> [x] -> [(Double, x)]
        runLengthEncode' x n [] = [(n,x)]
        runLengthEncode' x n (y:ys)
            | x == y    = runLengthEncode' x (n+1) ys
            | otherwise = (n,x):runLengthEncode' y 1 ys

takeOutConstantFactor :: [Expression] -> (Double, [Expression])
takeOutConstantFactor terms = (coefficient, otherTerms)
    where
        (nums, otherTerms) = partition isConstant terms
        coefficient = product $ map (\(Num x) -> x) nums

distribute :: [Expression] -> [Expression] -> [Expression]
distribute xs ys = do
        x <- xs
        y <- ys
        return $ x * y

-- We assume this has already been canonicalized
expand :: Expression -> Expression
expand (Prod stuff) = (canonicalizeSum . map canonicalize . map packageExpsIntoProduct .
            distributeEverything . makeProductOfSums . map (canonicalize . expand))
                    stuff
expand (Sum stuff) = canonicalize $ Sum $ map expand stuff
expand x = x

makeProductOfSums :: [Expression] -> [[Expression]]
makeProductOfSums stuff = map granularize stuff
    where
        granularize :: Expression -> [Expression]
        granularize (Prod x) = error "oh god, oh god"
        granularize (Sum x) = x
        granularize x = [x]

-- distributeEverything takes a product of sums and turns it into a sum of products
distributeEverything :: [[Expression]] -> [[Expression]]
distributeEverything [] = []
distributeEverything [sum] = map return sum
distributeEverything (sum:xs) = do
                    x <- sum
                    y <- distributeEverything xs
                    return (x:y)

main = print $ expand $ canonicalize $  "x" + "x" + "x"