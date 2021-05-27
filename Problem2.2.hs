multiply :: Num a => [a] -> a
multiply [] = 0
multiply [a] = a
multiply (x:xs) = x * (multiply xs)

substitute :: Eq a => a -> a -> [a] -> [a]
substitute a b [] = []
substitute a b (x:xs) = if x==a 
                           then b: (substitute a b xs) 
                           else x: (substitute a b xs)

duplicates :: Eq a => [a] -> Bool     
duplicates [a] = False
duplicates (x:xs) = elem x xs || duplicates xs

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = [] 
removeDuplicates (x:xs) = if elem x xs
                             then (removeDuplicates xs)
                             else x: (removeDuplicates xs)

prop_duplicatesRemoved :: [Integer] -> Bool
prop_duplicatesRemoved xs = not (duplicates (removeDuplicates xs))

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = [(x,y) | x<-xs, y<-ys]

pythagoras :: Int -> [(Int,Int,Int)]
pythagoras n = [(a,b,c) | a<-[1..n], b<-[1..n], c<-[1..n]  , a^2+b^2 == c^2, a<=b, b<= c]

countInList :: Eq a => a -> [a] -> Int
countInList a [] = 0
countInList a (x:xs) = if a==x 
                          then 1 + countInList a xs
                          else countInList a xs
isPermut :: Eq a => [a] -> [a] -> Bool
isPermut xs ys = helpPermut xs xs ys && helpPermut ys ys xs
helpPermut :: Eq a => [a] -> [a] -> [a] -> Bool
helpPermut [] xl yl = True
helpPermut (x:xs) xl yl = if elem x xl && (countInList x xl == countInList x yl)
                             then helpPermut xs xl yl
                             else False

shortestAndLongest :: [[a]] -> ([a],[a])
shortestAndLongest xs = (shortest xs , longest xs)
shortest :: [[a]] -> [a]
shortest [x] = x
shortest (x:xs) = if length x <= length (shortest xs)
                     then x
                     else shortest xs
longest :: [[a]] -> [a]
longest [x] = x
longest (x:xs) = if length x >= length (longest xs)
                     then x
                     else longest xs

mystery :: [Int] -> [Int]
mystery xs = foldr (++) [] (map (\y -> [y]) xs)                    

main = do 
    let test2 = ["ge"]
    let test1 =  ["This", "sentence", "is","ridiculous"]
    let test3 = [1,2,3,4,5]
    print $ mystery test3

    
    
