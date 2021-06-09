module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving (Show)

instance Parse T where
  parse = iter Statement.parse >-> buildProgram where
      buildProgram ss = Program ss
  toString (Program []) = ""
  toString (Program (st:sts)) = Statement.toString st ++ toString (Program sts)

exec :: T -> [Integer] -> [Integer]
exec (Program sts) input = Statement.exec sts Dictionary.empty input 
