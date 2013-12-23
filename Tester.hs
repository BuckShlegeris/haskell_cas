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
                        [map Prod (nonEmptyListsOf allExpressions)] ) -- ++
                        --[map (uncurry Power)
                        --          (pairs allExpressions allExpressions)])

allCanonicalizedExpressions :: [Expression]
allCanonicalizedExpressions = filter (\x-> x == canonicalize x) allExpressions

lilExps :: [Expression]
lilExps = filter (\x-> x == canonicalize x)
                  $ weightedWeave [
                        (x, map Num [0,1,2,4]),
                        (x, map Var ["x","y","z"]),
                        (x, map Sum (nonEmptyListsOf lilExps)),
                        (x, map Prod (nonEmptyListsOf lilExps))]
                       -- (1, map (uncurry Power) (pairs lilExps lilExps))]

x = 2

main = do
    print $ take 100 allCanonicalizedExpressions
