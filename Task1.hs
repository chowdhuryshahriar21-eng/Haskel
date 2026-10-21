import Data.Char
main ::IO()
main=do
	print (factorial 6)	--return the answer for factorial of 6
	print (factorial (-8))-- return the answer for factorial of -8
--MATHEMATICAL EXPLANATION--
--factorial 6
-- 6 * factorial (6-1)
-- 6*5 factorial (5-1)
--30*4 factorial (4-1) 
--120* 3 factorial (3-1) 
--360*2 factorial (2-1) 
--720*1 factorial (1-1) 
--720 factorial 0 
--720 *1
--720

	
factorial :: Integer -> Integer
factorial n|n < 0= error "Error: factorial does not work for negative value" |n==0=1|otherwise = n * factorial (n-1)
--error message for the negative value 