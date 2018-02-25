module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState

-- Possible Test Values
--   Not    Test   -- boolean negation
-- | Facing Card   -- am I facing the given cardinal direction?
-- | Clear  Dir    -- can I move in the given relative direction?
-- | Beeper        -- is there a beeper here?
-- | Empty         -- is my beeper bag empty?

-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t) w r = not (test t w r) -- boolean negation
test (Facing c') w (p, c, i) = c == c' -- true if c'direction == c direction
test (Clear c')	w (p, c, i)  = isClear (neighbor (cardTurn c' c) p) w -- true if next pos in dir c is clear
test Beeper w (p, c, i) = hasBeeper p w -- checks if beeper at position p
test Empty w r = isEmpty r -- checks if beeper in robot is empty


--data Stmt = Shutdown                 -- end the program
--          | Move                     -- move forward
--          | PickBeeper               -- take a beeper
--          | PutBeeper                -- leave a beeper
--          | Turn    Dir              -- rotate in place
--          | Call    Macro            -- invoke a macro
--          | Iterate Int  Stmt        -- fixed repetition loop
--          | If      Test Stmt Stmt   -- conditional branch
--          | While   Test Stmt        -- conditional loop
--          | Block   [Stmt]           -- statement block

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)

stmt Move d w (p, c, i) = let np = neighbor c p -- set next position
                        in if isClear np w -- check if next position is clear
                              then OK w (setPos np (p, c, i)) --if clear move
                              else Error ("Blocked at: " ++ show np) -- else err

stmt PutBeeper _ w r = let p = getPos r -- get current position
                        in if not (isEmpty r) -- check if we have peepers
                              then OK (incBeeper p w) (decBag r) -- if yes place
                              else Error "No beeper to put." -- if no err

stmt (Turn d) _ w (p, c, i) = OK w (setFacing (cardTurn d c) (p, c, i)) -- turn

stmt (Call m) d w r = case lookup m d of
                        Nothing -> Error ("Undefined macro: " ++ m)
                        Just s  -> stmt s d w r

stmt (Iterate 0 _) _ w r = OK w r -- if iterate 0
stmt (Iterate n s) d w r = case stmt s d w r of 
                            OK w' r'  -> stmt (Iterate (n-1) s) d w' r' -- iterate
                            otherwise -> otherwise

stmt (If t s s') d w r = if test t w r then stmt s d w r else stmt s' d w r

stmt (Block []) d w r = OK w r -- empty list
stmt (Block (s:ss)) d w r = case stmt s d w r of
                              OK w' r' -> stmt (Block ss) d w' r' -- go down the list
                              otherwise -> otherwise

stmt (While t s) d w r = if test t w r then case stmt s d w r of
                              OK w' r' -> stmt (While t s) d w' r'
                              otherwise -> otherwise
                            else OK w r    
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
