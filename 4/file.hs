-- split list into chunks of size n
chunkLt :: Int -> [a] -> [[a]]
chunkLt _ [] = []
chunkLt n xs = take n xs : chunkLt n (drop n xs)

-- find first index of element in list
firstLoc :: Eq a => a -> [a] -> Maybe Int
firstLoc x xs = foldr (\(i, y) acc -> if y == x then Just i else acc) Nothing (zip [0..] xs)

-- transpose 2D matrix
transp :: [[Int]] -> [[Int]]
transp xs
  | null xs        = []
  | any null xs    = []
  | otherwise      = [h | (h:_) <- xs] : transp [t | (_:t) <- xs]

-- dot product of two vectors
dotProduct :: [Int] -> [Int] -> Int
dotProduct xs ys = sum (zipWith (*) xs ys)

-- multiply two matrices
matmult :: [[Int]] -> [[Int]] -> [[Int]]
matmult a b =
  let bt = transp b
  in map (\rowA -> map (dotProduct rowA) bt) a

alphabet :: String
alphabet = ['a'..'z']

-- char to index lookup
letterNums :: [(Char, Int)]
letterNums = zip alphabet [0..]

-- index to char lookup
numLetters :: [(Int, Char)]
numLetters = map (\(c, n) -> (n, c)) letterNums

-- convert string -> 3xN matrix
translateStringToMatrix :: String -> [[Int]]
translateStringToMatrix str =
  let indices = map (\c -> case lookup c letterNums of
        Just n -> n
        Nothing -> error "Character not in alphabet") str
  in transp (chunkLt 3 indices)

-- convert 3xN matrix -> string
translateMatrixToString :: [[Int]] -> String
translateMatrixToString mat = concatMap (map (\n -> case lookup n numLetters of
        Just c -> c
        Nothing -> error "Index out of bounds")) (transp mat)

-- encrypt plaintext
cipher :: [[Int]] -> String -> String
cipher cipherMatrix plaintext =
  let ptMatrix = translateStringToMatrix plaintext
      product = matmult cipherMatrix ptMatrix
      ctMatrix = map (map (`mod` 26)) product
  in translateMatrixToString ctMatrix

-- decrypt ciphertext
decipher :: [[Int]] -> String -> String
decipher decipherMatrix ciphertext =
  let ctMatrix = translateStringToMatrix ciphertext
      product = matmult decipherMatrix ctMatrix
      ptMatrix = map (map (`mod` 26)) product
  in translateMatrixToString ptMatrix