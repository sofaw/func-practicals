import Prelude hiding (exp)
import Parse
import Data.Char (isAlpha)

-- Parsing --

data Prog = Prog [Eqn] deriving Show
data Eqn = Eqn Name [Pat] Exp deriving Show
data Exp = Nil | Var Name | App Name [Exp] | Cons Exp Exp deriving Show
data Pat = PNil | PVar Name | PCons Name Name deriving Show
type Name = String

prog :: Parser Prog
prog = Prog .:. many1 eqn

eqn :: Parser Eqn
eqn = Eqn .:. name .*. many pat ..* sym "=" .*. exp

exp :: Parser Exp
exp = Cons .:. app .*. sym ":" *.. exp .|. app .|.  Nil ... nil .|. Var .:. name

app :: Parser Exp
app = App .:. name .*. many arg

arg :: Parser Exp
arg = Nil ... nil .|. Var .:. name .|. sym "(" *.. exp ..* sym ")" 

pat :: Parser Pat
pat = PNil ... nil .|. PVar .:. name .|. PCons .:. sym "(" *.. name ..* sym ":" .*. name ..* sym ")" 

nil :: Parser String
nil = sym "[]"

name :: Parser Name
name = many1 (sat isAlpha) ..* spaces
