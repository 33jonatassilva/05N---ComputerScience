


-- tot3

tot3 :: [Int] -> [Int]

tot3 [] = []
tot3 [x] = [x]
tot3 [x,y] = [x+y]
tot3 (x:y:z:xs) = (x + y + z) : tot3 xs 


-- rev

rev :: [Int] -> [Int]

rev [] = []
rev [x] = [x]
rev (x:xs) = rev xs ++ [x]


-- seg

seg :: [Char] -> Char

seg [] = '\0'
seg (x:y:xs) = y

-- del_rep

del_rep :: [Int] -> [Int]

del_rep [] = []
del_rep [x] = [x]
del_rep (x:xs) = x : del_rep (filter (/= x) xs)


-- totk

totk :: Int -> [Int] -> [Int]

totk _ [] = []
totk k lst = sum (take k lst) : totk k (drop k lst)  


-- trok2

trok2 :: [Int] -> [Int]

trok2 [] = []
trok2 [x] = [x]
trok2 (x:y:xs) = y:x: trok2 xs


-- delk

delk :: Int -> [Char] -> [Char]

delk _ [] = []
delk k lst = take (k-1) lst ++ delk k (drop k lst)



