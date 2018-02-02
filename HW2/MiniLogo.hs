-- Riley Rimer - rimerr - 932-439-548
-- River Hendriksen - hendriri - 932239742
-- Kin-Ho Lam - lamki - 932-435-938
module Minilogo where
import Data.List
import Prelude hiding (Num)

type Num = Int
type Var = String
type Macro = String
    
type Prog = [Cmd]   ---sequence of commands

data Mode = Up   --- pen status
            | Down    
            deriving(Eq,Show)

data Expr   = Vari Var  --- Variable reference
            | Number Int    --- literal number
            | Add Expr Expr --- addition expression
            deriving(Eq, Show)
    
data Cmd    = Pen Mode ---change pen mode
            | Move Expr Expr ---move pen to a new position
            | Define Macro [Var] Prog ---define a macro
            | Call Macro [Expr] ---invoke a macro
            deriving(Eq,Show)

--  define line (x1, y1, x2, y2) {
--      pen up;
--      move (x1, y1);
--      pen down;
--      move (x2, y2);  
--  }

line :: Cmd
line = Define "line"
        ["x1", "y1", "x1", "x2"]
        [
            Pen Up, 
            Move (Vari "x1") (Vari "y1"),
            Pen Down,
            Move (Vari "x2") (Vari "y2")
        ]

--  define nix (x, y, w, h){
--      call line(x, y, x+w, y+h);
--      call line(x+w, y, x, y+h);
--  }

nix :: Cmd
nix = Define "nix" ["x", "y", "w", "h"]
        [Call "line" [Vari "x", Vari "y", Add (Vari "x") (Vari "w"), Add (Vari "y") (Vari "h")],
         Call "line" [Add (Vari "x") (Vari "w"), Vari "y", Vari "x", Add (Vari "y") (Vari "h")]]

steps :: Int -> Prog
steps 0 =   []
steps n =   [
                Call "line" [Number n, Number n, Number (n-1), Number n],
                Call "line" [Number (n-1), Number n, Number (n-1), Number (n-1)]
            ] ++ steps (n-1)

macros :: Prog -> [Macro] -- not sure about this function
macro [] = []
macros (x: xs) = case x of
        Define m _ _ -> m:macros xs
        otherwise  -> macros xs

prettyHelp :: Expr -> String
prettyHelp (Add l r) = prettyHelp l ++ " + " ++ prettyHelp r
prettyHelp (Number n) = show n
prettyHelp (Vari v) = v

pretty :: Prog -> String
pretty [] = ""
pretty (Call n x:xs) = n ++ "call (" ++ intercalate ", " (map prettyHelp x) ++ ");" ++ pretty xs
pretty ((Move l r):xs) = "move (" ++ prettyHelp l ++ ", " ++ prettyHelp r ++ ");" ++ pretty xs
pretty (Define a x p:ps) = "define " ++ a ++ " (" ++ intercalate ", " x ++ ") {" ++ pretty p ++ "};" ++ pretty ps
pretty (Pen Up:xs) = "pen up;" ++ pretty xs
pretty (Pen Down:xs) = "pen down;" ++ pretty xs

--Define a Haskell function optE :: Expr -> Expr that partially evaluates expressions by replacing any additions of literals with the result. 
--For example, given the expression (2+3)+x, optE should return the expression 5+x.
optE :: Expr -> Expr
optE (Add(Number l)(Number r)) = Number (l+r)
optE otherwise = otherwise

--Define a Haskell function optP :: Prog -> Prog that optimizes all of the expressions contained in a given program using optE.
optP :: Prog -> Prog
optP [] = []
optP (x: xs) = case x of
        Move l r -> Move(optE l) (optE r):optP xs
        Define m va p -> Define m va (optP p):optP xs
        Call m exer -> Call m (map optE exer): optP xs
        otherwise -> x:optP xs 


