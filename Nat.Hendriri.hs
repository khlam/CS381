module Nat where

import Prelude hiding (Enum(..), sum)


--
-- * Part 2: Natural numbers
--

-- | The natural numbers.
data Nat = Zero
         | Succ Nat
  deriving (Eq,Show)

-- | The number 1.
one :: Nat
one = Succ Zero

-- | The number 2.
two :: Nat
two = Succ one

-- | The number 3.
three :: Nat
three = Succ two

-- | The number 4.
four :: Nat
four = Succ three


-- | The predecessor of a natural number.
--   
--   >>> pred Zero
--   Zero
--   
--   >>> pred three
--   Succ (Succ Zero)
--   
pred :: Nat -> Nat
pred Zero = Zero
pred (Succ a) = a

-- | True if the given value is zero.
--
--   >>> isZero Zero
--   True
--
--   >>> isZero two
--   False
--
isZero :: Nat -> Bool
isZero Zero = True
isZero _ = False
-- | Convert a natural number to an integer.
--
--   >>> toInt Zero
--   0
--
--   >>> toInt three
--   3
--
toInt :: Nat -> Int
toInt Zero = 0
toInt i = (toInt (pred i)) + 1
-- | Add two natural numbers.
--
--   >>> add one two
--   Succ (Succ (Succ Zero))
--
--   >>> add Zero one == one
--   True
--
--   >>> add two two == four
--   True
--
--   >>> add two three == add three two
--   True
--   
add :: Nat ->  Nat ->  Nat
add Zero i = i
add (Succ a) b =  Succ (add a b)

-- | Subtract the second natural number from the first. Return zero
--   if the second number is bigger.
--
--   >>> sub two one
--   Succ Zero
--   
--   >>> sub three one
--   Succ (Succ Zero)
--
--   >>> sub one one
--   Zero
--
--   >>> sub one three
--   Zero
--
sub :: Nat ->  Nat ->  Nat
sub i Zero = i
sub Zero i = Zero
sub (Succ a) (Succ b) =  Succ(sub a b)
--sub :: Nat -> Nat -> Nat
--sub Zero i = i
--sub Succ(i) Succ(n) = sub(m n)

-- | Is the left value greater than the right?
--
--   >>> gt one two
--   False
--
--   >>> gt two one
--   True
--
--   >>> gt two two
--   False
--
--	 Iterate through Nats, if B reaches zero it is True
-- 	 Otherwise it is False 
gt :: Nat -> Nat -> Bool
gt (Succ a) Zero = True
gt (Succ a) (Succ b) = gt a b
gt _ _ = False
-- | Multiply two natural numbers.
--
--   >>> mult two Zero
--   Zero
--
--   >>> mult Zero three
--   Zero
--
--   >>> toInt (mult two three)
--   6
--
--   >>> toInt (mult three three)
--   9
--	 Using recursion we can think of the example of mul two and three (which is obviously 6)
--	 1. decrement 2 by 1 so: 1 3 = add 3 (mul 1 3)
--	 2. since mul is wrapped in the add decrement 1 to be zero so: 0 3 = add 3 (zero)
--   3. step out one: 3 + 1 = 4
--	 4: step out one: 4 + 2 = 6
--	 Idk if this is how it works but it made sense to me.
--   The real formula is: n * (m + 1) = (n * m) + n
mult :: Nat ->  Nat ->  Nat
mult Zero _ = Zero
mult _  Zero = Zero
mult (Succ a) b = add b (mult a b)
-- | Compute the sum of a list of natural numbers.
--
--   >>> sum []
--   Zero
--   
--   >>> sum [one,Zero,two]
--   Succ (Succ (Succ Zero))
--
--   >>> toInt (sum [one,two,three])
--   6
--	 : is the “prepend” operator:
--	 Returns a list which has x as first element, followed by all elements in xs. 
--	 In other functional languages, this is usually called cons, because it “cons”tructs 
--	 a list recursively by repeated application from an empty list:
-- 	 https://stackoverflow.com/questions/1696751/what-does-the-infix-operator-do-in-haskell
--
--	 eg: sum [one, two, three] = 1 + 2 + 3 + [] = 6 
sum :: [Nat] -> Nat
sum [] = Zero
sum (a:as) = add a (sum as)
--
-- | An infinite list of all of the *odd* natural numbers, in order.
--
--   >>> map toInt (take 5 odds)
--   [1,3,5,7,9]
--
--   >>> toInt (sum (take 100 odds))
--   10000
--
--   Based on Lazy Evaluation slide in Funtional Programming slides 
--   Start at Succ(zero) (1) and and go infinitely (this is represented by the colon operator)
--
--	 MAP: returns a list constructed by appling a function (the first argument) to all items in 
--	 a list passed as the second argument
--	 http://zvon.org/other/haskell/Outputprelude/take_f.html
--
--	 Dot Operator: The primary purpose of the . operator is not to avoid parentheses, but to 
--	 chain functions. It lets you tie the output of whatever appears on the right to the input 
--	 of whatever appears on the left. This usually also results in fewer parentheses, but works differently.
--   https://stackoverflow.com/questions/940382/haskell-difference-between-dot-and-dollar-sign
--
--	 For this problem we have a map which takes (Succ . Succ) as it's first arguement and odds as its second.
--	 Where Succ . Succ is basically saying every other number since you are piping the return value starting at one
--	 to another Succ to get the odd value.   

odds :: [Nat]
odds = Succ(Zero) : map (Succ . Succ) odds