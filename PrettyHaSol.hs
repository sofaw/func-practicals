import System.Environment
import Parse hiding (sym)
import Pretty
import Prelude hiding (exp,pure)
import Data.List (groupBy, intersperse)

-- Grammar, with comments permitted before each eqn:
-- prog  --> eqn+
-- eqn   --> comment* name pat* "=" exp
-- exp   --> app (":" exp)?
-- app   --> name arg*
-- arg   --> "[]" | name | "(" exp ")"
-- pat   --> "[]" | name | "(" name ":" name ")"
-- name  --> alpha+
-- comment --> "--" non-newline*

-- Spaces and line-breaks are permitted between symbols but all
-- and only names starting a new line also start an equation.
-- Comments must be complete lines.

-- Abstract Syntax:

data Prog = Prog [Eqn]
            deriving Show

-- String in Eqn is text of commentary
data Eqn  = Eqn String Name [Pat] Exp
            deriving Show

data Pat  = PNil | PVar Name | PCons Name Name
            deriving Show

data Exp  = Nil | Var Name | App Name [Exp] | Cons Exp Exp
            deriving Show

type Name = String

isApp :: Exp -> Bool
isApp (App _ _)    =  True
isApp _            =  False

isCons :: Exp -> Bool
isCons (Cons _ _)  =  True
isCons _           =  False

-- Parser:

prog :: Parser Prog
prog =  Prog .:. many1 eqn

eqn  :: Parser Eqn
eqn  =  Eqn .:. com .*. name .*. many pat ..* sym "=" .*. exp ..* many1 newLine

com  :: Parser String
com  =  unwords .:. many (sym "--" *.. many (sat (/= '\n')) ..* many1 newLine)

pat  :: Parser Pat
pat  =  PNil  ... sym "[]"
    .|. PVar  .:. name
    .|. PCons .:. sym "(" *.. name ..* sym ":" .*. name ..* sym ")"

exp  :: Parser Exp
exp  =  consOrArg .:. app .*. (Just .:. sym ":" *.. exp .|. pure Nothing)
  where
  consOrArg a (Just ex)  =  Cons a ex
  consOrArg a Nothing    =  a

app :: Parser Exp
app  =  varOrApp .:. name .*. many arg  
  where
  varOrApp n []  =  Var n
  varOrApp n es  =  App n es

arg  :: Parser Exp
arg  =  Nil      ... sym "[]"
    .|. Var      .:. name
    .|. sym "(" *.. exp ..* sym ")"

name :: Parser String
name  =  many1 lower ..* indent

newLine :: Parser Char
newLine  =  char '\n'

space :: Parser Char
space  =  char ' '

indent :: Parser ()
indent  =  () ... many space ..* many (newLine ..* many1 space)  

sym  :: String -> Parser String
sym s  =  string s ..* indent

-- Pretty-printer:

progDoc :: Int -> Prog -> DOC
progDoc w (Prog eqs)  =  stack $ intersperse nil [ stack (map (eqnDoc w) eqs')
                                                 | eqs' <- groupBy sameName eqs ]
    where
    sameName (Eqn _ n1 _ _) (Eqn _ n2 _ _)  =  n1 == n2

eqnDoc :: Int -> Eqn -> DOC
eqnDoc w (Eqn c n ps e)  =  commentary <>
                            nest 2 (fill [appDoc patDoc 0 n ps, text "=", expDoc e])
  where      commentary  =  if null c then nil
                            else stack [ text ("-- " ++ s)
                                       | s <- lines (pretty (w-3) (fillwords c)) ]
                                 <> line

patDoc :: Pat -> DOC
patDoc (PVar n)       =  text n
patDoc PNil           =  text "[]"
patDoc (PCons n1 n2)  =  brac (text n1 <> text " : " <> text n2)

expDoc :: Exp -> DOC
expDoc Nil         =  text "[]"
expDoc (Var n)     =  text n
expDoc (App n xs)  =  appDoc argDoc 2 n xs
expDoc (Cons x y)  =  nest 2 $ fill [headDoc x, text ":", expDoc y]

argDoc :: Exp -> DOC
argDoc e   |  isApp e || isCons e  =  brac (expDoc e)
           |  otherwise            =  expDoc e

appDoc :: (a->DOC) -> Int -> String -> [a] -> DOC
appDoc f i n xs  =  group (text n <> nest i (line <> stack (map f xs)))

headDoc :: Exp -> DOC
headDoc e  |  isCons e   =  brac (expDoc e)
           |  otherwise  =  expDoc e

brac :: DOC -> DOC
brac d  =  text "(" <> d <> text ")"

-- pass w onto progDoc, not just to pretty
prettyProg :: Int -> Prog -> String
prettyProg w p  =  pretty w (progDoc w p)

main  =  do src <- getContents
            [w] <- getArgs
            let Just p  =  parseWith prog src
            putStrLn (prettyProg (read w) p)
