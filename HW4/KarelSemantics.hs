-- Riley Rimer - rimerr - 932-439-548
-- River Hendriksen - hendriri - 932239742
-- Kin-Ho Lam - lamki - 932-435-938
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
test (Not tes) worl robo = not (test tes worl robo) -- test for boolean negation
test (Facing cardin) _ robo = cardin == getFacing robo 
test (Clear deff) wurled (pos, cardin, integer)  = isClear (neighbor (cardTurn deff cardin) pos) wurled
test Beeper wurled (pos, cardin, integer) = hasBeeper pos wurled -- checks if beeper at position p
test Empty wurled robo = isEmpty robo -- checks if beeper in robot is empty


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
stmt Shutdown   _ _ robo = Done robo
stmt PickBeeper _ wurled robo = let pos = getPos robo
                        in if hasBeeper pos wurled
                              then OK (decBeeper pos wurled) (incBag robo)
                              else Error ("There was no beeper to pick at this position: " ++ show pos)

stmt Move deff wurled (pos, cardin, integer) = let nextpos = neighbor cardin pos -- set next position
                        in if isClear nextpos wurled -- check if next position is clear
                              then OK wurled (setPos nextpos (pos, cardin, integer)) --if clear move
                              else Error ("Movement was blocked at: " ++ show nextpos) -- else err

stmt PutBeeper _ wurled robo = let pos = getPos robo -- get current position
                        in if not (isEmpty robo) -- check if we have peepers
                              then OK (incBeeper pos wurled) (decBag robo) -- if yes place
                              else Error "There is no beeper to be entered." -- if no err

stmt (Turn deff) _ wurled (pos, cardin, integer) = OK wurled (setFacing (cardTurn deff cardin) (pos, cardin, integer)) -- turn

stmt (Call macros) deff wurled robo = case lookup macros deff of -- calls macro m on  
                        Nothing -> Error ("Undefined macro: " ++ macros)
                        Just s  -> stmt s deff wurled robo

stmt (Iterate n s) deff wurled robo = if n > 0 then case stmt s deff wurled robo of 
                            OK wurled' robo'  -> stmt (Iterate (n-1) s) deff wurled' robo' -- iterate
                            Error e -> Error e
                            Done robo -> Done robo
                            else OK wurled robo

stmt (If t s s') deff wurled robo = if test t wurled robo then stmt s deff wurled robo else stmt s' deff wurled robo

stmt (Block []) deff wurled robo = OK wurled robo -- empty list
stmt (Block (s:ss)) deff wurled robo = case stmt s deff wurled robo of
                              OK wurled' robo' -> stmt (Block ss) deff wurled' robo' -- go down the list
                              Error e -> Error e
                              Done robo -> Done robo

stmt (While t s) deff wurled robo = if test t wurled robo then case stmt s deff wurled robo of
                              OK wurled' robo' -> stmt (While t s) deff wurled' robo'
                              Error e -> Error e
                              Done robo -> Done robo
                              else OK wurled robo  
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (macros,s) wurled robo = stmt s macros wurled robo
