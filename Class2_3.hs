data Set a = Set [a] deriving(Show,Eq)

empty :: Set a
empty = Set []

add :: Eq a => a -> Set a -> Set a
add el (Set as) 
    | inSet el (Set as) = (Set as)
    | otherwise = Set (el:as) 

add2 :: Ord a => a -> Set a -> Set a
add2 el (Set (a:as))
    | el < a = Set (el:a:as)
    | el > a = add2 a 

inSet :: Eq a => a-> Set a -> Bool
inSet el (Set []) = False
inSet el (Set (a:as)) = (el==a) || inSet el (Set as)

create :: Eq a => [a] -> Set a
create [] = empty
create (a:as) = add a (create as) 

union :: Eq a => Set a -> Set a -> Set a
union (Set []) as = as 
union as (Set []) = as
union (Set (a:as)) bs = add a (union (Set as) bs)

intersect :: Eq a => Set a -> Set a -> Set a
intersect (Set []) _ = Set [] 
intersect _ (Set []) = Set []
intersect (Set (a:as)) bs 
    |inSet a bs = add a (intersect (Set as) bs)
    | otherwise = intersect (Set as) bs

remove :: Eq a => a -> Set a -> Set a
remove x (Set []) = Set []
remove x (Set (a:as)) = if (x==a) 
                           then Set as
                           else add a (remove x (Set as))



