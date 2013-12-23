module Weaver
where

import Data.List (findIndex)
import Language.Haskell.Exts.Parser (parseDecl)


-- If you've got an infinite list of finite lists, use concat

-- weave takes a finite list of either infinite or finite lists
weave :: [[a]] -> [a]
weave [] = []
weave [x] = x
weave ([]:xs) = weave xs
weave ((x:xs):ys) = x:(weave (ys++[xs]))

weightedWeave :: [(Int,[a])] -> [a]
weightedWeave [] = []
weightedWeave [(n,x)] = x
weightedWeave ((_,[]):xs) = weightedWeave xs
weightedWeave ((n,x):xs) = (take n x) ++ (weightedWeave (xs ++ [(n,drop n x)]))

-- weaveInfinite takes an infinite list of infinite lists
weaveInfinite :: [[a]] -> [a]
weaveInfinite list = diagonalize (\(x,y) -> list!!x !!y) [0..] [0..]


-- diagonalize takes two infinite lists and a function to join them.
diagonalize :: ((a, b) -> c) -> [a] -> [b] -> [c]
diagonalize f list1 list2 = map f $ pairs list1 list2

pairs :: [a] -> [b] -> [(a,b)]
pairs x y = pairs' x y 0
    where
        pairs' :: [a] -> [b] -> Int -> [(a,b)]
        pairs' list1 list2 n = zip (take n list1) (reverse $ take n list2)
                                    ++ pairs' list1 list2 (n+1)


lists :: [[Int]]
lists = [[]] ++ diagonalize (uncurry (:)) integers lists

listsOf :: [a] -> [[a]]
listsOf list = [[]] ++ diagonalize (uncurry (:)) list (listsOf list)

nonEmptyListsOf :: [a] -> [[a]]
nonEmptyListsOf list = diagonalize (uncurry (:)) list (listsOf list)

integers :: [Int]
integers = weave [[0..],[-1,-2..]]

listsOfListsOfInts :: [[[Int]]]
listsOfListsOfInts = listsOf (listsOf integers)

--main = print $ findIndex ((==) [1,2,4,0]) lists
--main = print $ findIndex ((==) $ [[4],[3]]) (listsOfListsOfInts)


--data Btree = Null | Node Int Btree Btree
--    deriving (Show, Eq)


--trees :: [Btree]
--trees = weave [[Null], (map (\(x,(y,z)) -> Node x y z)
--                                    $ pairs integers (pairs trees trees))]
