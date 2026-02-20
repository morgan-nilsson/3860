{-
  CS3860
  Lab Assignment #3

  exports in ()
  1 - matrix (transp)
  2 - (matmult)
  3 - Pascal's triangle (pascal, triangleR, pascalTriangle)
  4 - Caesar cipher and decipher (cipher, decipher)
-}

-- Task 1
-- transpose a 2D matrix by recursively extracting column heads
transp :: [[Int]] -> [[Int]]
transp xs
  | null xs        = []
  | any null xs    = []
  | otherwise      = map head xs : transp (map tail xs)

-- Task 2
-- dot product of two vectors
dotProduct :: [Int] -> [Int] -> Int
dotProduct xs ys = sum (zipWith (*) xs ys)

-- multiply two matrices using transp and dotProduct
matmult :: [[Int]] -> [[Int]] -> [[Int]]
matmult a b =
  let bt = transp b
  in map (\rowA -> map (dotProduct rowA) bt) a

-- Task 3
-- given a row return the row immediately below it
pascal :: [Int] -> [Int]
pascal xs = zipWith (+) (0:xs) (xs ++ [0])

-- return the n-th row of Pascal's triangle directly
triangleR :: Int -> [Int]
triangleR 0 = [1]
triangleR n = pascal (triangleR (n-1))

-- generate all rows of Pascal's triangle from 0 to n
pascalTriangle :: Int -> [[Int]]
pascalTriangle 0 = [[1]]
pascalTriangle n = pascalTriangle (n-1) ++ [triangleR n]

-- Task 4
-- 53-char alphabet: A-Z, a-z, space
alphabet :: String
alphabet = ['A'..'Z'] ++ ['a'..'z'] ++ [' ']

-- map each character to its index
letterNums :: [(Char, Int)]
letterNums = zip alphabet [0..]

-- map each index back to its character
numLetters :: [(Int, Char)]
numLetters = map (\(c, n) -> (n, c)) letterNums

-- shift each index by n, wrapping around the alphabet
shifter :: [Int] -> Int -> [Int]
shifter xs shift = map (\x -> (x + shift) `mod` length alphabet) xs

-- convert string to list of indices
stringToNum :: String -> [Int]
stringToNum str = [n | c <- str, Just n <- [lookup c letterNums]]

-- convert list of indices back to string
numToString :: [Int] -> String
numToString ns = [c | n <- ns, Just c <- [lookup n numLetters]]

-- stringToNum -> shift -> numToString; cipher uses +n decipher uses -n
applyShift :: Int -> String -> String
applyShift shift = numToString . shifter' . stringToNum
  where shifter' xs = shifter xs shift

-- encrypt message by shifting letters forward n positions
cipher :: String -> Int -> String
cipher message shift = applyShift shift message

-- decrypt message by shifting letters back n positions
decipher :: String -> Int -> String
decipher message shift = applyShift (-shift) message
