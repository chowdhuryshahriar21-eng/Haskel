import Data.Char
-- CHARACTER MAPPING (A=1..26, space=0)

charToInt :: Char -> Integer
charToInt ' ' = 0 -- for space
charToInt c   = toInteger (ord (toUpper c) - ord 'A' + 1)

intToChar :: Integer -> Char
intToChar 0 = ' '
intToChar n = chr (fromInteger n + ord 'A' - 1)
-- RSA core


powMod :: Integer -> Integer -> Integer -> Integer
powMod b 0 m = 1
powMod b e m
    | even e    = let h = powMod b (e `div` 2) m in (h * h) `mod` m
    | otherwise = (b * powMod b (e - 1) m) `mod` m

-- RSA ENCRYPTION AND DECRYPTION
encryptNum :: Integer -> Integer -> Integer -> Integer
encryptNum m e n = powMod m e n

decryptNum :: Integer -> Integer -> Integer -> Integer
decryptNum c d n = powMod c d n

encrypt :: String -> Integer -> Integer -> [Integer]
encrypt msg e n = [ encryptNum (charToInt ch) e n | ch <- msg ]

decrypt :: [Integer] -> Integer -> Integer -> String
decrypt nums d n = [ intToChar (decryptNum x d n) | x <- nums ]



nVal :: Integer
nVal = 187      -- 11 * 17(n value pq)

eVal :: Integer
eVal = 7        -- public key exponent 

dVal :: Integer
dVal = 23       -- private key exponent

-------------------------------------------------------
-- MAIN PROGRAM WITH ENCRYPT + DECRYPT
-------------------------------------------------------

main :: IO ()
main = do
    putStrLn "=RSA Messaging System ="
    putStrLn "1. Encrypt a message"
    putStrLn "2. Decrypt numbers"
    putStrLn "Choose (1 or 2):"
    choice <- getLine

    if choice == "1" then do
        putStrLn "Enter a message to encrypt: "
        msg <- getLine
        let cleaned = map (\c -> if c == ' ' then ' ' else toUpper c) msg
        let enc = encrypt cleaned eVal nVal
        putStrLn ("Encrypted numbers: " ++ show enc)--encrypted message in number 
        putStrLn ("Decrypted again:   " ++ decrypt enc dVal nVal)

    else if choice == "2" then do--condition for decryption 
        putStrLn "Enter numbers separated by spaces:"
        numsLine <- getLine
        let nums = map read (words numsLine) :: [Integer]
        putStrLn ("Decrypted message: " ++ decrypt nums dVal nVal)

    else
        putStrLn "Invalid choice." --if user select wrong option 

--Mathematical Explanation--
--Encrypting message for “HI”
--charToInt c = toInteger (ord (toUpper c) - ord 'A' + 1)
--H-8th position
--I-9th position
--m=[8,9]
--encryptNum m e n = powMod m e n
--c=m7 mod187
--c= 87 mod187
--c= 2097152 mod 187 
--c=134
--For H -encrypted code is 134
--       For I
--c= 97 mod 187
--c=70
--So, Encrypted message for HI is: [134,70]
--To decrypt a code [134,70] 
--decryptNum c d n = powMod c d n
--m=c23 mod 187
--m=13423 mod 187
--m= 8
--8th position is H 
--m= 7023 mod 187 
--m=9
--9th position is I 
--So, decrypted message is “HI”
