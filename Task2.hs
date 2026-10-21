import Data.Char
main ::IO()
main=do
	print (listUnion [1,2,34,4,9] [8,6,0,2,8])
	print (listUnion [1,2,3,4] [1,4,2,3])
	print (listIntersection [1,2,34,4,9] [8,6,0,2,8])
	print (listIntersection [ ] [7])
	print (listIntersection [1,2,3] [1,2,3])
	print (listDifference [1,2,3] [1,2,5])
	print (listDifference [1,2,3] [1])
	
	--MATHEMATICAL EXPLANATION--
--listUnion [1,2,3,4] [1,4,2,3]
--here the answer will be the element from both set elimination the repeated number
--Answer: [1,2,3,4]

--listIntersection [1,2,34,4,9] [8,6,0,2,8]
--here the answer will be the one which is present on both of the set 
--2 is the common 
--Answer: [2]

--listDifference [1,2,3] [1,2,5]
--here the difference will be the one element which is present on the first list but not in the second one.
--1,2 is present on both but 3 is not in the second list so answer will be 3 
--Answer: [3] 
--listDifference [1,2,3] [1]
--1 is present on the second list but 2,3 is not 
--Answer: [2,3]


--listIntersection [ ] [7] 
--here one is empty set and the other one has 7 that’s why it returns an empty set 
--Answer: []
--listIntersection [1,2,3]  [1,2,3]
--Here both set are same so the answer will be one set element only 
--Answer: [1,2,3]



-- | listDifference xs ys returns elements that are in xs but NOT in ys.
--   Ordering follows xs.
listDifference :: (Eq a) => [a] -> [a] -> [a]
listDifference xs ys = [x | x <- xs, not (x `elem` ys)]

	
--listUnion first initialisation as a variable 
listUnion :: (Eq a) => [a] -> [a] -> [a]
listUnion xs ys = nubPreserve (xs ++ ys)	
--Remove duplicates while preserving first occurrences.
nubPreserve :: (Eq a) => [a] -> [a]
nubPreserve [] = []
nubPreserve (x:xs)| x `elem` xs = x : nubPreserve (filter (/= x) xs)| otherwise   = x : nubPreserve xs
-- | listUnion xs ys produces the union of two sets represented as lists.
--   Order is preserved from xs first, then any new elements from ys.


-- | listIntersection xs ys returns the set of all elements appearing in both lists.
--   Ordering follows the order of xs.
listIntersection :: (Eq a) => [a] -> [a] -> [a]
listIntersection xs ys = nubPreserve [x | x <- xs, x `elem` ys]

