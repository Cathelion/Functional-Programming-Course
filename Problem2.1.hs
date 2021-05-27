module Main where

f :: Int -> Int
f n = sum $ map (\x->x^2) [1..n] 

h :: Int -> Int
h 1 = 1
h n = 1 + 2 * h (n-1)

nextFactor :: Int -> Int -> Int
nextFactor k n = if n `mod` k == 0 then k else nextFactor (k+1) n  

smallestFactor :: Int -> Int
smallestFactor n = nextFactor 2 n

numFactors :: Int -> Int
numFactors 1 = 0
numFactors n = sum $ map (\k-> if k == nextFactor k n then 1 else 0) [1..n] 

data Month = Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec deriving (Show)
daysInMonth :: Month -> Int -> Int
daysInMonth m y = case m of {
    Jan -> 31;
    Feb -> if y `mod` 4 ==0 then 29 else 28;
    Mar -> 31;
    Apr -> 30;
    May -> 31;
    Jun -> 30;
    Jul -> 31;
    Aug -> 31;
    Sep -> 30;
    Oct -> 31;
    Nov -> 30;
    Dec -> 31;
}

data Datum = Datum Int Month Int deriving (Show)
datumDay :: Datum -> Int
datumMonth :: Datum -> Month
datumYear :: Datum -> Int
datumValid :: Datum -> Bool
datumYear (Datum y _ _) = y
datumMonth (Datum _ m _) = m
datumDay (Datum _ _ d) = d
datumValid (Datum y m d) = if d <= daysInMonth m y then True else False


main = do 
    let test3= Datum 2003 Feb 29
    print $ datumValid (test3)  
    
