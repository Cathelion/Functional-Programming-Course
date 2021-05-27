-- Florian Gruen 10.05.2021
-- Lund University/LTH EDAN40 Assignment 2 String Alignment
-- This is a string alignment solver using partially dynamical programming

{-EXPLANATION:
I personally struggled the most with the implementations of the alignment functions. The similarityScore functions were easier to write, because it is an easy triple recursion in the primitive case and only tracks one value in the dynamical programming case. In the beginning I even had trouble with the speed for the primitive alignment function for small input strings, the reason was that a used a too complicated concatenation with filter and map for the maximaBy function. For the dynamical alignment it was difficult to keep track of the indicies and the reversing when appending the tails. Firstly I had results, which were glibberish, then result that were half right, half glibberish, then reversed results. For all those maximization functions, there should be some easier way to implement it, maybe using some library functions.
I think it is nicely readable, especially in the dynamical alignment that the code is split up and structured in several help functions.
-}
import Data.List
type AlignmentType = (String, String)

-- constant values for scoring
scoreSpace = -1
scoreMis = -1
scoreMatch = 0

-- extension of max to three values
max3 :: Int -> Int -> Int -> Int
max3 a b c = max (max a b) c

-- help function to build things like "---"
nString :: Int -> Char -> String
nString 0 _ = ""
nString n c = c : nString (n-1) c

-- help function, could be encorporated in sim1/sim2 but kept for better readability
score2 :: Char -> Char -> Int
score2 '-' _ = scoreSpace
score2 _ '-' = scoreSpace
score2 x y = if (x==y) then scoreMatch else scoreMis

-- computes the score of an alignment     
score :: (String , String) -> Int
score ([],[]) = 0
score ((x:xs), (y:ys)) 
    | (x==y) = scoreMatch + score (xs, ys)
    | (x=='-' || y=='-') = scoreSpace + score (xs, ys)
    | otherwise = scoreMis + score (xs, ys)

-- appends h1 to the beginning of the first list of each tuple and h2 to the beginning of the snd list
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList] 

-- appends h1 to the end of the first list of each tuple and h2 to the end of the snd list
attachTails :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachTails t1 t2 aList = [(append t1 xs,append t2 ys) | (xs,ys) <- aList] 
append :: a -> [a] -> [a]
append a [] = [a]
append a (x:xs) = x : append a xs



--SIMILARITY 1, SLOW
-- primitive similarityScore function
sim1 :: String -> String -> Int
sim1 [] ys = scoreSpace * length ys
sim1 xs [] = scoreSpace * length xs
sim1 (x:xs) (y:ys) = max3 (sim1 xs ys + score2 x y) 
                         (sim1 xs (y:ys) + scoreSpace)
                         (sim1 (x:xs) ys + scoreSpace)

                         
                         
-- SIMILARITY 2, FAST
-- improved similarity score, using dynamical programming
-- the boundary case where a string gets aligned with the empty string is logically the alignment (s,"--...-")
-- the recursion starts from the right bottom corner of the table, but the table itself is filled from top left on
sim2 :: String -> String -> Int
sim2 xs ys = finalEntry (length xs) (length ys)
  where
    finalEntry i j = table !!i!!j
    table = [[ entry i j | j<-[0..]] | i<-[0..] ]
       
    entry :: Int -> Int -> Int
    entry i 0 = i*scoreSpace
    entry 0 j = j*scoreSpace
    entry i j = max3 (finalEntry (i-1) (j-1) + score2 (xs!!(i-1)) (ys!!(j-1)))
                     (finalEntry (i-1) j + scoreSpace) 
                     (finalEntry i (j-1) + scoreSpace)



-- ALIGNMENTS 1, SLOW
-- main alignment function
-- computes in each comparison the score on each element
-- in the improved version the score is carried.
optAlignments :: String -> String -> [AlignmentType]
optAlignments [] ys = [(nString (length ys) '-', ys)]
optAlignments xs [] = [(xs, nString (length xs) '-')]
optAlignments (x:xs) (y:ys) = maximaBy score
    ( attachHeads x y (optAlignments xs ys)
    ++ attachHeads x '-' (optAlignments xs (y:ys)) 
    ++ attachHeads '-' y (optAlignments (x:xs) ys) )  

-- generalization of max function; takes all values froma list that evaluate to the max value
maximaBy :: Ord b => (a->b) -> [a] -> [a]
maximaBy valueFcn [] = []
maximaBy valueFcn xs = filter (\x -> valueFcn x ==maxim) xs
    where maxim = maximum $ map valueFcn xs 
    

    
-- ALIGNMENTS 2, FAST
-- improved alignment function using dynamical programming 
-- s and empty string get aligned as (s,--...-)
-- uses attachTails as we basically look at (xs:x) (ys:y) when forward propagating through table
optAlignments2 :: String -> String -> (Int,[AlignmentType])
optAlignments2 xs ys = finalEntry (length xs) (length ys)
  where
    finalEntry i j = table !!i!!j
    table = [[ entry i j | j<-[0..]] | i<-[0..] ]
       
    entry :: Int -> Int -> (Int,[AlignmentType])
    entry i 0 = (i*scoreSpace, [(take i xs, nString i '-')])
    entry 0 j = (j*scoreSpace, [(nString j '-', take j ys)])
    entry i j = process [prep (xs!!(i-1)) (ys!!(j-1))   (finalEntry (i-1) (j-1)) 
                         ,prep '-' (ys!!(j-1)) (finalEntry (i) (j-1))
                         ,prep (xs!!(i-1)) '-' (finalEntry (i-1) (j))]  
                             
-- help function to compute new score and modify alignments                          
prep :: Char -> Char -> (Int,[AlignmentType]) ->  (Int,[AlignmentType])
prep c1 c2 (n,aligns) = (n + score2 c1 c2 , attachTails c1 c2 aligns)

-- help function to take all alignments that score the highest
process :: [(Int,[AlignmentType])] -> (Int,[AlignmentType])
process l =(m,foldr (++) [] [b | (a,b)<-l, a==m]) 
    where m = max3 (fst (l!!0)) (fst (l!!1)) (fst (l!!2))
    
    
    
-- OUTPUT     
-- some nice output, works only with optAlignments1, but could be easily modified for optAlignments2
outputOptAlignment :: String -> String -> IO ()
outputOptAlignment s1 s2 = putStrLn("There are " ++ show(length res) ++ " optimal alignments: \n\n" ++ format res ++  "There were " ++ show(length res) ++ " optimal alignments!" )
    where res = optAlignments s1 s2 

-- help formatting function
format :: [AlignmentType] -> String
format [] = ""
format (x:xs) = show (strFormat(fst x)) ++ "\n" ++ show (strFormat (snd x)) ++ "\n\n" ++ format xs  
strFormat :: [Char] -> String
strFormat [c] = c : ""
strFormat (c:cs) = c : ' ' :strFormat cs
