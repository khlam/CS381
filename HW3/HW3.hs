-- Riley Rimer - rimerr - 932-439-548
-- River Hendriksen - hendriri - 932239742
-- Kin-Ho Lam - lamki - 932-435-938
module HW3 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--   * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--     functions for generating MiniMiniLogo programs. It contains the type
--     definitions for Mode, Cmd, and Prog.
--   * Render.hs contains code for rendering the output of a MiniMiniLogo
--     program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--   
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen x) (_, st) = ((x, st), Nothing)
cmd (Move x y) (Down, st) = ((Down,(x, y)), Just((st), (x, y)))
cmd (Move x y) (Up, st) = ((Up, (x, y)), Nothing)


-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
prog [] st = (st, [])
prog (h:t) st  = case cmd h st of 
    (x, Just he) -> (\(st, t) -> (st, he:t)) $ prog t x
    (x, Nothing) -> prog t x


--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
-- -- amazing = [Pen Up, Move 9 0, Pen Down, Move 11 0, Move 11 1, Move 9 1, Move 9 0, Move 9 2, Move 7 2, Move 7 1, Move 9 1, Move 9 2, Move 7 2, Move 6 2, Move 6 3, Move 7 3, Move 7 2, Move 7 3, Move 7 4, Move 9 4, Move 9 3, Move 7 3, Pen Up, Move 9 4,
 --Pen Down, Move 11 4, Move 11 5, Move 9 5, Move 9 4]
amazing :: Prog
amazing = (square 10 0) ++ (square 11 0) ++ (square 9 1) ++ (square 8 1) ++ (square 7 2) ++ (square 8 3) ++ (square 9 3) ++ (square 10 4) ++ (square 11 4) ++ (square 12 5) ++ (square 13 5) ++ (square 11 6) ++ (square 10 6) ++ (square 9 6) ++ (square 8 7) ++ (square 7 7) ++ (square 6 7)++ (square 5 7) ++ (square 5 8) ++ (square 5 9) ++ (square 5 10)  ++ (square 5 11)  ++ (square 5 12)  ++ (square 5 13)
  ++ (square 6 13) ++ (square 7 13) ++ (square 8 13) ++ (square 9 13) ++ (square 10 13) ++ (square 11 12) ++ (square 12 12) ++ (square 13 12) ++ (square 14 11) ++ (square 15 11) ++ (square 15 10) ++ (square 16 9) ++ (square 16 8) ++ (square 16 7) ++ (square 17 7)
  ++ (square 16 6) ++ (square 16 5) ++ (square 16 4) ++ square 15 4 ++ square 15 3 ++ square 15 2 ++ square 15 1 ++ square 15 0 ++ square 14 1 ++ square 14 2 ++ square 14 3 ++ square 13 3 ++ square 13 2 ++ square 13 1 ++ square 12 2 ++ square 11 2 
  ++ square 19 0 ++ square 18 1 ++ square 18 2 ++ square 22 0 ++ square 22 1 ++ square 22 2 ++ square 21 3 ++ square 21 4 ++ square 21 5
  ++ square 19 11 ++ square 18 11 ++ square 18 12 ++ square 18 13 ++ square 18 14 ++ square 19 12 ++ square 19 13 ++ square 19 14 ++ square 20 12 ++ square 20 13 ++ square 17 12 ++ square 17 13 
  ++ square 24 11 ++ square 24 12 ++ square 24 13 ++ square 24 14 ++ square 25 14 ++ square 26 14 ++ square 27 14 ++ square 28 14 ++ square 25 15 ++ square 26 15 ++ square 27 15 ++ square 28 13 ++ square 28 12 ++ square 28 11 ++ square 28 10 ++ square 27 10 ++ square 26 10 ++ square 25 10
  ++ square 28 9 ++ square 29 8 ++ square 30 7 ++ square 30 6 ++ square 31 5 ++ square 31 4 ++ square 29 11 ++ square 30 11 ++ square 31 10 ++ square 32 9 ++ square 33 8 ++ square 34 7 ++ square 34 6 ++ square 35 7 
   ++ square 37 0 ++ square 37 1 ++ square 37 2 ++ square 37 3 ++ square 36 4 ++ square 36 5 ++ square 36 6 ++ square 36 7 ++ square 36 8 ++ square 36 9 ++ square 37 10 ++ square 37 11 ++ square 37 12 ++ square 37 13 ++ square 37 14 ++ square 36 15 ++ square 36 16 ++ square 36 17 ++ square 37 17 ++ square 38 16 ++ square 39 16 ++ square 40 16 ++ square 41 16 ++ square 42 16 ++ square 43 16 ++ square 40 17 ++ square 41 17 ++ square 42 17 ++ square 43 17 ++ square 41 18 ++ square 42 18 ++ square 41 19 ++ square 40 20 ++ square 39 20 ++ square 38 21 ++ square 37 21 ++ square 36 22 ++ square 35 22 ++ square 34 22 ++ square 33 22 ++ square 32 23 ++ square 31 23 ++ square 30 23 ++ square 29 24 ++ square 28 24 ++ square 27 24 ++ square 26 24 ++ square 25 24 ++ square 24 24 ++ square 23 24 ++ square 22 23 ++ square 21 23 ++ square 20 23 ++ square 19 22 ++ square 18 22 ++ square 17 22 ++ square 16 22 ++ square 15 21 ++ square 14 21 ++ square 13 20 ++ square 12 20 ++ square 11 19 ++ square 11 18 ++ square 10 18 ++ square 12 17 ++ square 11 17 ++ square 10 17 ++ square 9 17 ++ square 8 16 ++ square 9 16 ++ square 10 16 ++ square 11 16 ++ square 12 16 ++ square 13 16 ++ square 14 16 ++ square 15 17 ++ square 16 17 ++ square 16 16 ++ square 16 15 ++ square 15 14 ++ square 15 13 ++ square 15 12 ++ square 15 11
  ++ square 19 17 ++ square 20 17 ++ square 21 18 ++ square 22 18 ++ square 26 17 ++ square 32 17 ++ square 33 17 ++ square 30 18 ++ square 31 18 ++ square 17 18 ++ square 17 19 ++ square 35 18 ++ square 35 19 ++ square 33 14 ++ square 34 14 ++ square 33 13 ++ square 34 13 ++ square 33 12 ++ square 34 12 ++ square 33 11 ++ square 34 11 ++ square 32 13 ++ square 32 12 ++ square 35 13 ++ square 35 12


square:: Int -> Int -> Prog
square x y = [Pen Up, Move x y, Pen Down, Move (x+1) (y), Move  (x+1) (y +1), Move  (x) (y+1), Move x y] 