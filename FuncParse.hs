import Parse
import Data.Char (isLower)

data Expr = Var Char | Lam Char Expr | App Expr Expr deriving Show

expr :: Parser Expr
expr = foldl1 App .:. many1 atom .|. Lam .:. sym "\\" *.. var ..* sym "." .*. expr

atom :: Parser Expr
atom = Var .:. var .|. sym "(" *.. expr ..* sym ")"

var :: Parser Char
var = token (sat isLower)
