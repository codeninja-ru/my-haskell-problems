import System.Random
import Data.List

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
split' :: [a] -> Int -> ([a], [a])
split' [] _ = ([], [])
split' all@(x:xs) count
    | count > 0 = (x:n1, all)
    | otherwise = ([], all)
    where (n1, n2) = split' xs (count - 1)
--split list n1 n2 = take (n2 - n1) (drop (n1 - 1) list)

-- problem 18
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice list 1 count = take' count list
	where 
		take' _ [] = []
		take' 1 (x:_) = [x]
		take' count (x:xs) = x:take (count - 1) xs
slice (x:xs) p1 p2 = slice xs (p1 - 1) (p2 - 1)

-- and with hight order functions
slice' :: [a] -> Int -> Int -> [a]
slice' list p1 p2 = drop (p1 - 1) (take p2 list)

-- problem 19
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate x 0 = x
rotate list n 
	| n > len = rotate list (n - len)
	| n < 0 = rotate list (n + len)
	| otherwise = f2 ++ f1 
	where 	len = length list
		(f1, f2) = splitAt n list

-- Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n list = (last f1, (init f1) ++ f2)
	where (f1, f2) = splitAt (n + 1) list


-- problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt s xs 1 = s:xs
insertAt s (x:xs) num = x:insertAt s xs (num - 1)

insertAt' s list num = f1 ++ s:f2 where (f1, f2) = splitAt (num - 1) list

-- problem 22
range :: Int -> Int -> [Int]
range first last = [first..last]

--problem 23
rnd_select :: [a] -> Int -> [a]
rnd_select xs n = rnd xs n $ mkStdGen 10
	where 	rnd [] _ _ = []
		rnd _ 0 _ = []
		rnd xs n gen = r:rnd xss (n - 1) new_gen
			where
				(num, new_gen) = randomR (0, (length xs) - 1) gen
				r = xs !! num
				xss = map snd (filter (\(x,_) -> x /= num) (zip [0..] xs))

rnd_select' :: [a] -> Int -> [a]
rnd_select' xs n = map (xs!!) ns
	where ns = take n . nub $ randomRs (0, length xs - 1) (mkStdGen 10)

-- problem 24
diff_select count m = take count $ randomRs (1, m) (mkStdGen 10)

diff_selectIO :: Int -> Int -> IO [Int]
diff_selectIO 0 _ = return []
diff_selectIO c m = do
			r <- randomRIO (1, m) :: IO Int
			next <- diff_selectIO (c - 1) m
			return (r:next)

-- problem 25
-- Generate a random permutation of the elements of a list. 
rnd_permu :: [a] -> IO [a]
rnd_permu x = do
	gen <- newStdGen
	return (rnd_permu' x gen)
	where 	rnd_permu' x gen = map (x!!) (take (length x) (nub (randomRs (0, length x - 1) gen :: [Int])))

rnd_permu'' :: (RandomGen g) => [a] -> g -> [a]
rnd_permu'' [] _ = []
rnd_permu'' xs gen = elem:rnd_permu'' nxs new_gen
	where 	(elem, nxs) = removeAt idx xs
		(idx, new_gen) = randomR (0, length xs - 1) gen

-- problem 26
combinations :: (Eq a) => Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations c xs =
		 let	
			do_comb 1 xs ys = ys
			do_comb n xs ys = do_comb (n - 1) xs (comb n xs ys)
			comb n xs ys = [x:y | (x, y) <- zip (rep (length ys) xs) (cycle ys), not (elem x y)]
			-- повтоярет массив n раз 
			-- rep 2 abc = aabbcc
			rep _  [] = []
			rep n (x:xs) = (replicate n x) ++ rep n xs
		in do_comb c xs (map (:[]) xs)
