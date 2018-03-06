import Prelude hiding (exp)
import Parse
import Pretty
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
exp = Cons .:. app .*. sym ":" *.. exp .|. app .|.  Nil ... parserNil .|. Var .:. name

app :: Parser Exp
app = App .:. name .*. many arg

arg :: Parser Exp
arg = Nil ... parserNil .|. Var .:. name .|. sym "(" *.. exp ..* sym ")" 

pat :: Parser Pat
pat = PNil ... parserNil .|. PVar .:. name .|. PCons .:. sym "(" *.. name ..* sym ":" .*. name ..* sym ")" 

parserNil :: Parser String
parserNil = sym "[]"

name :: Parser Name
name = many1 (sat isAlpha) ..* spaces

-- Printing --
prettyProg :: Int -> Prog -> String
prettyProg w p = pretty w (prog2doc p)

prog2doc :: Prog -> DOC
prog2doc (Prog []) = text ""
prog2doc (Prog (e:eqns)) = (eqn2doc e) <> line <> (prog2doc (Prog eqns)) 

eqn2doc :: Eqn -> DOC
eqn2doc (Eqn n p e) = text (n++" ") <> pats2doc p <> text (" = ") <> exp2doc e

exps2doc :: [Exp] -> DOC
exps2doc [] = nil
exps2doc (e:es) = exp2doc e <> exps2doc es

exp2doc :: Exp -> DOC
exp2doc (Nil) = nil
exp2doc (Var n) = text n
exp2doc (App n exps) = bracket "(" (text (n++" ") <> exps2doc exps) ")" 
exp2doc (Cons e1 e2) = bracket "(" (exp2doc e1 <> text ":" <> exp2doc e2)  ")"

pats2doc :: [Pat] -> DOC
pats2doc [] = nil
pats2doc (p:ps) = pat2doc p <> pats2doc ps

pat2doc :: Pat -> DOC
pat2doc (PNil) = nil
pat2doc (PVar v) = text (" "++v++" ")
pat2doc (PCons n1 n2) = bracket "(" (text (n1++":"++n2)) ")"

isNilOrVar :: Exp -> Bool
isNilOrVar (Nil) = True
isNilOrVar (Var _) = True
isNilOrVar _ = False
