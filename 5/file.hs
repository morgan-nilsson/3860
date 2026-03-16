data TTTree a = Empty
    | TwoC a (TTTree a, TTTree a)
    | ThreeC (a, a) (TTTree a, TTTree a, TTTree a)
    deriving(Show, Eq, Ord)

-- Inorder traversal to list
inorder :: TTTree a -> [a]
inorder tree = case tree of
    Empty -> []
    TwoC x (left, right) -> inorder left ++ [x] ++ inorder right
    ThreeC (x, y) (left, middle, right) -> inorder left ++ [x] ++ inorder middle ++ [y] ++ inorder right

-- Height; empty = -1
height :: TTTree a -> Int
height tree = case tree of
    Empty -> -1
    TwoC _ (left, right) -> 1 + max (height left) (height right)
    ThreeC _ (left, middle, right) -> 1 + maximum [height left, height middle, height right]

-- True if height-balanced
balance :: TTTree a -> Bool
balance tree = case tree of
    Empty -> True
    TwoC _ (left, right) -> balance left && balance right && height left == height right
    ThreeC _ (left, middle, right) -> balance left && balance middle && balance right
        && height left == height middle && height middle == height right

-- helper func
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- True if properly ordered
ordered :: Ord a => TTTree a -> Bool
ordered tree = isSorted (inorder tree)

instance Functor TTTree where
    fmap _ Empty = Empty
    fmap f (TwoC x (left, right)) = TwoC (f x) (fmap f left, fmap f right)
    fmap f (ThreeC (x, y) (left, middle, right)) = ThreeC (f x, f y) (fmap f left, fmap f middle, fmap f right)

main :: IO ()
main = do
    let t = TwoC 5 (ThreeC (2, 4) (Empty, Empty, Empty), ThreeC (7, 9) (Empty, Empty, Empty))
    putStrLn $ show t
    putStrLn $ show (inorder t)
    putStrLn $ show (height t)
    putStrLn $ show (balance t)
    putStrLn $ show (ordered t)
    -- if the tree el contains 2, 5, or 9 = True, else False
    putStrLn $ show $ fmap (\x -> x `elem` [2, 5, 9]) t
