myLast :: [a] -> a
myLast [] = error "fuck"
myLast [x] = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast (x:[a]) = x
myButLast (x:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt (x:_) 1  = x
elementAt (x:xs) n = elementAt xs $ pred n

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = succ $ myLength xs

myLength' :: [a] -> Int
myLength' x = foldl (\ a _ -> succ a) 0 x

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

data NestedList a = Elem a | List [NestedList a] 

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List x) = concatMap flatten' x

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress xs = foldl (\a1 a2 -> if last a1 == a2 then a1 else a1 ++ [a2] ) [(head xs)] (tail xs)

compress' :: (Eq a) => [a] -> [a]
compress' [] = []
compress' (x:xs) = x : dropWhile (== x) (compress' xs)

compress'' (x:ys@(y:_))
    | x==y = compress'' ys
    | otherwise = x : compress'' ys
compress'' xs = xs

-- problem 9
pack :: (Eq a) => [a] -> [[a]]
pack = foldr func []
    where 
        func x [] = [[x]]
        func y (stack@(x:xs)) = if (head x) == y then (y:x):xs else [y]:stack

pack' :: (Eq a) => [a] -> [[a]]
pack' [] = []
pack' (all@(x:xs)) = matches:pack' last
    where (matches,last) = span ( == x) all

-- problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (func) . pack
    where func xs = (length xs, head xs)

-- problem 11
data Prob11Data a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [Prob11Data a]
encodeModified = map make_date . encode
    where
        make_date (1,x) = Single x
        make_date (n,x) = Multiple n x

--  problem 12
decodeModified :: (Eq a) => [Prob11Data a] -> [a]
decodeModified [] = []
decodeModified ((Single val):xs) = val:decodeModified xs
decodeModified ((Multiple count val):xs) = (take count $ repeat val) ++ decodeModified xs

-- problme 13
encodeDirect :: (Eq a) => [a] -> [Prob11Data a]
encodeDirect [] = []
encodeDirect (x:xs) = do_chose x $ encodeDirect xs
    where 
        do_chose x [] = [Single x]
        do_chose x (all@(y:ys))
            | x == (get_value y) = (inc y):ys
            | otherwise = (Single x):all
        get_value (Single x) = x
        get_value (Multiple _ x) = x
        --inc :: Prob11Data a -> a
        inc (Single x) = Multiple 2 x
        inc (Multiple count value) = Multiple (count + 1) value 

-- problem 14
dupli :: [a] -> [a]
dupli = concatMap (\ x -> x:[x])

dupli' :: [a] -> [a]
dupli' [] = []
dupli' (x:xs) = x:x:dupli' xs

-- problem 15
repli :: [a] -> Int -> [a]
repli list count = concatMap (replicate count) list

-- problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery _ 0 = []
dropEvery list num = (take (num - 1) list) ++ dropEvery (drop 3 list) num 

-- problem 17
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split all@(x:xs) count
    | count > 0 = (x:n1, all)
    | otherwise = ([], all)
    where (n1, n2) = split xs (count - 1)
--split list n1 n2 = take (n2 - n1) (drop (n1 - 1) list)
