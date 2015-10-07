module Calc where

{-
On day one of your new job as a software engineer, you’ve been
asked to program the brains of the company’s new blockbuster product:
a calculator. But this isn’t just any calculator! Extensive focus
group analysis has revealed that what people really want out of their
calculator is something that can add and multiply integers. Anything
more just clutters the interface.
Your boss has already started by modeling the domain with the
following data type of arithmetic expressions:
data ExprT = Lit Integer
| Add ExprT ExprT
| Mul ExprT ExprT
deriving (Show, Eq)
This type is capable of representing expressions involving integer
constants, addition, and multiplication. For example, the expression
(2 + 3) × 4 would be represented by the value
Mul (Add (Lit 2) (Lit 3)) (Lit 4).
Your boss has already provided the definition of ExprT in ExprT.hs,
so as usual you just need to add import ExprT to the top of your file.
However, this is where your boss got stuck.
-}

import ExprT
import Parser

{-
Exercise 1
Write Version 1 of the calculator: an evaluator for ExprT, with the
signature
eval :: ExprT -> Integer
For example, eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20.
-}
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add exp1 exp2) = eval exp1 + eval exp2
eval (Mul exp1 exp2) = eval exp1 * eval exp2

{-
Exercise 2
The UI department has internalized the focus group data and is
ready to synergize with you. They have developed the front-facing
user-interface: a parser that handles the textual representation of the
selected language. They have sent you the module Parser.hs, which
exports parseExp, a parser for arithmetic expressions. If you pass
the constructors of ExprT to it as arguments, it will convert Strings
representing arithmetic expressions into values of type ExprT. For
example:
*Calc> parseExp Lit Add Mul "(2+3)*4"
Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
*Calc> parseExp Lit Add Mul "2+3*4"
Just (Add (Lit 2) (Mul (Lit 3) (Lit 4)))
*Calc> parseExp Lit Add Mul "2+3*"
Nothing
Leverage the assets of the UI team to implement the value-added
function
evalStr :: String -> Maybe Integer
which evaluates arithmetic expressions given as a String, producing
Nothing for inputs which are not well-formed expressions, and
Just n for well-formed inputs that evaluate to n.

parseExp :: (Integer -> a) -> (a -> a -> a) -> (a -> a -> a) -> String -> Maybe a

test:
evalStr "(2+3)*4"
Just 20
evalStr "2+3*"
Nothing
-}

evalStr :: String -> Maybe Integer
evalStr s = case maybeExp of
  Just exp -> Just (eval exp) -- typeof exp is Integer here because of evalStr's type constraint makes it returns Integer
  Nothing -> Nothing
  -- Lit Add Mul are functions passed to parseExp
  -- maybeExp == Just exp | Nothing
  where maybeExp = parseExp Lit Add Mul s

{-
evalStr s = evalMaybe maybeExp
  where maybeExp = parseExp Lit Add Mul s
        evalMaybe (Just exp) = Just (eval exp)
        evalMaybe Nothing = Nothing
-}


{-
Exercise 3
Good news! Early customer feedback indicates that people really
do love the interface! Unfortunately, there seems to be some disagreement
over exactly how the calculator should go about its calculating
business. The problem the software department (i.e. you) has is that
while ExprT is nice, it is also rather inflexible, which makes catering
to diverse demographics a bit clumsy. You decide to abstract away
the properties of ExprT with a type class.
Create a type class called Expr with three methods called lit, add,
and mul which parallel the constructors of ExprT. Make an instance of
Expr for the ExprT type, in such a way that
mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
== Mul (Add (Lit 2) (Lit 3)) (Lit 4)
Think carefully about what types lit, add, and mul should have. It
may be helpful to consider the types of the ExprT constructors, which
you can find out by typing (for example)
*Calc> :t Lit
at the ghci prompt.
Remark. Take a look at the type of the foregoing example expression:
*Calc> :t mul (add (lit 2) (lit 3)) (lit 4)
Expr a => a
What does this mean? The expression mul (add (lit 2) (lit 3)) (lit 4)
has any type which is an instance of the Expr type class. So writing it
by itself is ambiguous: GHC doesn’t know what concrete type you
want to use, so it doesn’t know which implementations of mul, add,
and lit to pick.
One way to resolve the ambiguity is by giving an explicit type
signature, as in the above example. Another way is by using such an
expression as part of some larger expression so that the context in
which it is used determines the type. For example, we may write a
function reify as follows:
reify :: ExprT -> ExprT
reify = id
To the untrained eye it may look like reify does no actual work!
But its real purpose is to constrain the type of its argument to ExprT.
Now we can write things like
reify $ mul (add (lit 2) (lit 3)) (lit 4)
at the ghci prompt.

*Calc> :t mul (add (lit 2) (lit 3)) (lit 4)
Expr a => a -- it's an Expr type, but what instance? what lit, add, mul to use?

reify $ mul (add (lit 2) (lit 3)) (lit 4)
Mul (Add (Lit 2) (Lit 3)) (Lit 4)
-}

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit -- lit n = Lit n
  add = Add
  mul = Mul

-- its real purpose is to constrain the type of its argument to ExprT.
-- the type definition of reify (the type definition will give the type)
reify :: ExprT -> ExprT
reify = id

{-
The marketing department has gotten wind of just how flexible
the calculator project is and has promised custom calculators to some
big clients. As you noticed after the initial roll-out, everyone loves the
interface, but everyone seems to have their own opinion on what the
semantics should be. Remember when we wrote ExprT and thought
that addition and multiplication of integers was pretty cut and dried?
Well, it turns out that some big clients want customized calculators
with behaviors that they have decided are right for them.
The point of our Expr type class is that we can now write down
arithmetic expressions once and have them interpreted in various
ways just by using them at various types

Make instances of Expr for each of the following types:
• Integer — works like the original calculator
• Bool — every literal value less than or equal to 0 is interpreted
as False, and all positive Integers
are interpreted as True; “addition” is logical or,
“multiplication” is logical and
• MinMax — “addition” is taken to be the max function, while
“multiplication” is the min function
• Mod7 — all values should be in the ranage 0 . . . 6, and
all arithmetic is done modulo 7; for example,
5 + 3 = 1.
The last two variants work with Integers internally, but in order
to provide different instances, we wrap those Integers in newtype
wrappers. These are used just like the data constructors we’ve seen
before.
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)
Once done, the following code should demonstrate our family of
calculators:
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
Try printing out each of those tests in ghci to see if things are
working. It’s great how easy it is for us to swap in new semantics for
the same syntactic expression!
-}

-- Integer is a instance that implements the Expr spec: lit, mul, add
instance Expr Integer where
  lit = id --lit x = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x = x > 0 -- no paren needed here
  add x y = x || y
  mul x y = x && y

-- MinMax
newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax -- lit a = MinMax a
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

-- Mod7
newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer  -- Just (-7)
testBool = testExp :: Maybe Bool -- Just True
testMM = testExp :: Maybe MinMax -- Just (MinMax 5)
testSat = testExp :: Maybe Mod7 -- Just (Mod7 0)
