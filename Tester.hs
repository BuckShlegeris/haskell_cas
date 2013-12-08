module Tester

where

import Solve
import Simplify
import Expression
import Weaver
import Data.List (findIndex)
import Debug.Trace (traceShow)

allExpressions :: [Expression]
allExpressions = weave ([map Num [0,1,2,4]] ++
                        [map Var ["x","y","z"]] ++
                        [map Sum (nonEmptyListsOf allExpressions)] ++
                        [map Prod (nonEmptyListsOf allExpressions)] ++
                        [map (uncurry Power)
                                (pairs allExpressions allExpressions)])

allCanonicalizedExpressions :: [Expression]
allCanonicalizedExpressions = filter (\x-> x == canonicalize x) allExpressions

main = do
    print $ map (\x-> traceShow x 1) $ take 100 allCanonicalizedExpressions