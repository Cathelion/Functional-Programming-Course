module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.Maybe

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]

--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
--stateOfMind brian = return id
stateOfMind brain = do
    r <- randomIO :: IO Float
    return (rulesApply (zip (map fst brain) (map (pick r .  snd) brain)))
    
    
rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply list1 xs = maybe [] id (transformationsApply "*" reflect list1 xs) 

reflect :: Phrase -> Phrase
reflect [] = []
reflect (p:ps) | elem p (map fst reflections) = snd (reflections !! (length $ takeWhile (/=p) (map fst reflections))) : reflect ps
               | otherwise = p : (reflect ps)
               
reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") 

prepare2 :: String -> Phrase
prepare2 = words. map toLower

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile [] = []
rulesCompile (x:xs) = [(prepare2 (fst x), map prepare2 (snd x))] ++ rulesCompile(xs)


--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply reducs phr = if (maybe phr id (transformationsApply "*" id reducs phr)) /= phr 
                                then reductionsApply reducs (maybe phr id (transformationsApply "*" id reducs phr))
                                else phr
--reductionsApply _ = id


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute a [] ys = []
substitute a (x:xs) ys = if a==x 
                            then ys ++ (substitute a xs ys)
                            else [x] ++ (substitute a xs ys)


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match wc xs ys = match2 wc 0 xs ys 
                      
match2 :: Eq a => a -> Int -> [a] -> [a] -> Maybe [a]
match2 wc c [] [] = Just []
match2 wc c xs [] = Nothing
match2 wc c [] ys = Nothing
match2 wc c (x:xs) (y:ys) | (x/=wc && x==y) = (match2 wc (-abs(c)) xs ys)
                      | (x/=wc && x/=y) = Nothing
                      | (x==wc && c>=0) = fmap ((++) [y]) (orElse (singleWildcardMatch 1 (x:xs) (y:ys)) (longerWildcardMatch 1 (x:xs) (y:ys)))
                      | otherwise = (orElse (singleWildcardMatch c (x:xs) (y:ys)) (longerWildcardMatch c (x:xs) (y:ys)))

                      
-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => Int -> [a] -> [a] -> Maybe [a]
singleWildcardMatch c (wc:ps) (x:xs) = match2 wc c ps xs
longerWildcardMatch c (wc:ps) (x:xs) = match2 wc c (wc:ps) xs


-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc f xs patTrans = fmap (substitute wc (snd patTrans)) (fmap f (match wc (fst patTrans) xs))     


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply wc f [] xs = Nothing
transformationsApply wc f (p:ps) xs = orElse (transformationApply wc f xs p) (transformationsApply wc f ps xs)


