import Data.Char
main ::IO()
main=do
	print (sortList [15,9,6,4,7,30])
	print (sortList [3,4,2,1,0,4])
	
	
	--MATHEMATICAL EXPLANATION--
--sortList [15,9,6,4,7,30] 
--the answer will give a list of these element in ascending order 
--Answer: [4,6,7,9,15,30]

--sortList [3,4,2,1,0,4] 
--the answer will give a list of these element in ascending order 
--Answer: [0,1,2,3,4,4]




--insert each element into a sorted list one at a time 
insertSorted :: Int -> [Int] -> [Int]
insertSorted x [] = [x]
insertSorted x (y:ys)
    | x <= y    = x : y : ys      -- x goes before y
    | otherwise = y : insertSorted x ys

-- Main sorting function 
sortList :: [Int] -> [Int]
sortList = foldr insertSorted []
-- foldr inserts elements one by one into the sorted result

