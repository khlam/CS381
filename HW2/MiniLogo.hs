-- Riley Rimer - rimerr - 932-439-548
-- River Hendriksen - hendriri - 932239742
-- Kin-Ho Lam - lamki - 932-435-938

import Prelude

type Num	=	Int
type Var	=   String
type Macro	=	String
 			
type Prog	=	[Cmd]   ---sequence of commands
 			
data Mode   =	Down   --- pen status
            |   Up	
    deriving(Eq,Show)
     
data Expr	=	Vari Var	--- Variable reference
            |	Number Num	--- literal number
            |	Add Expr Expr	--- addition expression
 			
data Cmd	=	Pen Mode ---change pen mode
            |	Move Expr Expr ---move pen to a new position
            |	Define Macro [Var] Prog ---define a macro
            |	Call Macro [Expr]	---invoke a macro
    deriving(Eq,Show)

--	define line (x1, y1, x2, y2) {
--		pen up;
--		move (x1, y1);
--		pen down;
--		move (x2, y2); 	
--	}

line::Cmd
line = Define "line"
        ["x1", "y1", "x1", "x2"]
        [
            Pen Up, 
            Move (Vari "x1") (Vari "y1"),
            Pen Down,
            Move (Vari "x2") (Vari "y2"),
        ]

--	define nix (x, y, w, h){
--		call line(x, y, x+w, y+h);
--		call line(x+w, y, x, y+h);
--	}

nix::Cmd
nix = Define "nix"
        ["x", "y", "w", "h"]
        [
            Call "line" [Vari "x", Vari "y", Add (Vari "x")(Vari "w"), Add (Vari "y")(Vari "h")],
            Call "line" [Add (Vari "x")(Vari "w"), Vari "y", Vari "x", Add (Vari "y")(Vari "h")]
        ]

steps:: Int -> Prog
steps 0 =	[Pen up]
steps n =	[
				Call "line" [Vari (show x), Vari (show y), Vari (show (x-1)), Vari (show y)],
				Call "line" [Vari (show (x-1)), Vari (show y), Vari (show (x-1)), Vari (show (y-1))]
			] + 
               