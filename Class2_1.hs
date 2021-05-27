module Class2_1 where
import Data.List

data Proposition = Var Name 
                 | Proposition :&: Proposition 
                 | Proposition :|: Proposition 
                 | Not Proposition 
                   deriving (Eq,Show)
type Name = String
                   
vars :: Proposition -> [Name]
vars (Var x) = [x]
vars (p :&: q)  = union (vars (p)) (vars (q))   
vars (p :|: q)  = union (vars (p)) (vars (q))
vars (Not p) = vars p


truthValue :: Proposition -> [(String, Bool)] -> Bool
truthValue (Var x) l = maybe False id (lookup x l)
truthValue (p :&: q) l= (truthValue p l) && (truthValue q l)
truthValue (p :|: q) l= (truthValue p l) ||  (truthValue q l)
truthValue (Not p) l= not (truthValue p l) 

possibleArr :: Int -> [[Bool]]
possibleArr 0 = [[]]
possibleArr n = (map (True:) (possibleArr (n-1))) ++ (map (False:) (possibleArr (n-1))) 


tautology :: Proposition -> Bool
tautology p = all (==True) (map (truthValue p) (map (zip (vars p)) (possibleArr (length(vars p)))))
 


{-
mmapAnd :: Maybe Bool -> Maybe Bool -> Maybe Bool
mmapAnd Nothing (Just y) = Nothing
mmapAnd (Just x) Nothing = Nothing
mmapAnd Nothing Nothing = Nothing
mmapAnd (Just x) (Just y) = Just (x && y)

mmapOr :: Maybe Bool -> Maybe Bool -> Maybe Bool
mmapOr (Just True) _ = Just True
mmapOr _ (Just True) = Just True
mmapOr (Just False) Nothing = Nothing
mmapOr Nothing Nothing = Nothing
mmapOr Nothing (Just False) = Nothing
mmapOr (Just x) (Just y) = Just (x || y)

mmapNot :: Maybe Bool -> Maybe Bool
mmapNot (Just True) = Just False
mmapNot (Just False) = Just True
mmapNot Nothing = Nothing

truthValue :: Proposition -> [(String,Bool)] -> Maybe Bool
truthValue (Var x) l = lookup x l
truthValue (p :&: q) l= mmapAnd (truthValue p l) (truthValue q l)
truthValue (p :|: q) l= mmapOr (truthValue p l) (truthValue q l)
truthValue (Not p) l= mmapNot (truthValue p l)  -}
