module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement = 
    Comment |
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Read String |
    Write Expr.T |
    Skip |
    Block [Statement]
    deriving Show

commenter = accept "--" -# comment #- require "\n" >-> buildComment 
buildComment _ = Comment
    
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

reader = accept "read" -# word #- require ";" >-> buildRead
buildRead w = Read w

writer = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e

iffer = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((e,s1),s2) = If e s1 s2

whiler = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e,s) = While e s

skiper = accept "skip" -# require ";" >-> buildSkip
buildSkip _ = Skip

blocker = accept "begin" -# iter parse #- require "end" >-> buildBlock
buildBlock ss = Block ss


exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
       then exec (thenStmts: stmts) dict input
       else exec (elseStmts: stmts) dict input
exec (Assignment name e : stmts) dict input = 
    let dictN = Dictionary.insert (name, Expr.value e dict) dict 
    in exec stmts dictN input
exec (Read s :stmts) dict input = exec stmts dictN (tail input) where dictN = Dictionary.insert (s, head input) dict
exec (Write e :stmts) dict input = Expr.value e dict : exec stmts dict input
exec (Skip :stmts) dict input = exec stmts dict input
exec (Comment :stmts) dict input = exec stmts dict input
exec (Block ss : stmts) dict input = exec (ss++stmts) dict input   
exec (While cond s1 :stmts) dict input = 
    if (Expr.value cond dict)>0 
       then exec (s1:(While cond s1 :stmts)) dict input 
       else exec stmts dict input
exec [] dict input = []

helpTab :: Int->String
helpTab n  = concat $ replicate n "    "

instance Parse Statement where
  parse = assignment ! reader ! writer ! iffer ! whiler ! skiper ! blocker ! commenter
  toString stat = toStringN 0 stat where
      -- use recursion on level depth for indentation
      toStringN n stat = case stat of
                    Assignment name e -> helpTab n ++ name ++ " := " ++ toString e ++ ";\n"
                    Skip -> helpTab n ++ "skip;\n"
                    Comment -> helpTab n ++"--this was a comment, we lost it \n"
                    Read var -> helpTab n ++ "read " ++ var ++ ";\n"
                    Write e -> helpTab n ++ "write " ++ toString e ++ ";\n"
                    If cond stat1 stat2 -> helpTab n ++ "if " ++ toString cond ++ " then\n" ++ toStringN (n+1) stat1 ++ helpTab n ++ "else\n" ++ toStringN (n+1) stat2
                    While cond stat1 -> helpTab n ++ "while " ++ toString cond ++ " do\n" ++ toStringN (n+1) stat1
                    Block (s:ss) ->helpTab n ++ "begin\n" ++ toStringN (n+1) s ++ concat (map (toStringN (n+1)) ss) ++ helpTab n ++ "end\n"

                    
