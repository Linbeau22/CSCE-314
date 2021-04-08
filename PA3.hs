--Beau Lin
--228006534

--On my honor as an aggie, i have followed the honor code

{-
Links:

-}

type Set a = [a]

func :: Set a -> Set a
func ls = ls

--Set constructor
-- isDuplicate :: Set a -> 

--if l is equal to next l, delete l. else continue doing recursion
mkSet :: Eq a => [a] -> Set a --constructing a set from a list: removing duplicates
mkSet [] = []
mkSet (l:ls) = if l `elem` ls then mkSet ls else l:mkSet ls --if head is in the list then run mkSet on tail. If it is not in list, prepend it to ls

--[1,2,3], [1,2,3,4]
subset :: Eq a => Set a -> Set a -> Bool
subset [] _ = True
subset (l1:ls1) ls2 = if l1 `elem` ls2 then subset ls1 ls2 else False --if head of ls1 is  in ls2, then call function on tail of ls1, else false


setEqual :: Eq a => Set a -> Set a -> Bool
setEqual ls1 ls2 = if (subset ls1 ls2) && (subset ls2 ls1) then True else False --if ls1 is a subset of ls2 AND ls2 is a subset of ls1, return True, else False


-- setEqual' :: Eq a => Set(Set a) -> Set(Set a) -> Bool
-- setEqual _ [[]] = True
-- setEqual' (l1:ls1) ls2 = if (l1 `setEqual` head(ls2)) then setEqual 

--[[1,2,3]] [[3,1,2]]

-- setEqualRecursion :: Eq a => Set a -> Set(Set a) -> Bool





--zip' [1,2] [3,4] => [(1,3), (1,4), (2,3), (2,4)]
-- zip' :: Eq b => [a] -> [b] -> [(a, b)]
-- zip' [] [] = []
-- zip' (x:xs) (y:ys) = if null ys then (x, y) : zip' xs ys else (x, y) : zip' [x] ys

setProd :: (Eq ls1, Eq ls2) => Set ls1 -> Set ls2 -> Set (ls1, ls2)
setProd [] _ = []
setProd _ [] = []
setProd (x:xs) ys = map (\y -> (x,y)) ys ++ setProd xs ys

partition :: Int -> Set a -> Set(Set a)
partition _ [] = []
partition n xs = (take n xs) : (partition n (drop n xs))

unionSetPartition :: Set a -> Set(Set a) --takes a set, the subset it gives is the set containing all elements ex: [1,2,3] -> [[1,2,3]]
unionSetPartition xs = partition (length xs) xs

separateSetPartition :: Set a -> Set(Set a) --takes a set, the subsets it gives are the subsets containing the individual elements ex: [1,2,3] -> [[1],[2],[3]]
separateSetPartition xs = partition 1 xs

--[[red],[green,blue]]  [[green],[red,blue]]  [[blue],[red,green]]

xTonPartition :: Int -> Set a -> Set(Set a)
xTonPartition n xs = if (n < length xs) then partition (length xs - n) xs else []

fullPartition :: Int -> Set a -> Set(Set(Set a))
fullPartition n xs = if (n >= 0) then (xTonPartition n xs):(fullPartition (n-1) xs) else []

partitionSet :: Eq t => Set t -> Set(Set(Set t))
partitionSet xs = fullPartition (length (xs) - 1) xs
-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 
--                                                                              !!!!!     FOR THE GRADER    !!!!! 
--         - partitionSet gives one output, and partitionSet' gives another output. partitionSet gives the output of the partitions, but only one combination of each type of partition.
--         - partitionSet' gives the output of all the partitions, but does not take care of duplicates.
-- ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
permutate :: (Eq a) => Set a -> Set(Set a)
permutate [] = [[]]
permutate l = [a:x | a <- l, x <- (permutate $ filter (\x -> x /= a) l)]


-- -- ! How to find the finalPartition of the permutation of each list in a list
-- -- ! then 

--If the head of the list gives partitions that are equal to the partitions already in the list, add it. else don't 
finalPartition' :: Eq a => Set(Set a) -> Set(Set(Set a))
finalPartition' [] = [[]]
finalPartition' permList = (fullPartition (length (head permList)) (head permList)) ++ finalPartition' (tail permList)

partitionSet' :: Eq a => Set a -> Set(Set(Set a))
partitionSet' xs = finalPartition' (permutate xs)

-- cleanList :: Eq a => Set a -> Set(Set(Set a))
-- cleanList xs = if head(xs) `setEqual`  

-- cleanPartitionSet :: Set(Set(Set a)) -> Set(Set(Set a))
-- cleanPartitionSet (x:xs) = 


listOfn n = take n (repeat 1) --gets a list of input length of 1's

bellNum :: Int -> Int
bellNum n = length(partitionSet (listOfn n))


-- -- yeti :: Integer -> Integer
-- yeti x = y
--     where {y = permutate x}