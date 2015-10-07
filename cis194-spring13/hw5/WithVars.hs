{-# LANGUAGE FlexibleInstances #-}

module WithVars where

{-
For storing mappings from variables to values, you should
use the Data.Map module. Add

import qualified Data.Map as M

at the top of your file. The qualified import means that
**you must prefix M.**
*whenever you refer to things from Data.Map. This is standard
practice, since Data.Map exports quite a few functions with
names that overlap with names from the Prelude. Consult the
Data.Map documentation to read about the operations that are supported
on Maps. http://hackage.haskell.org/
packages/archive/containers/latest/
doc/html/Data-Map.html
-}
import qualified Data.Map as M

-- copied from Calc.hs
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

{-
Some users of your calculator have requested the ability to give
names to intermediate values and then reuse these stored values
later.-}

{-
To enable this, you first need to give arithmetic expressions the
ability to contain variables. Create a new type class HasVars a which
contains a single method var :: String -> a. Thus, types which are
instances of HasVars have some notion of named variables.
-}
class HasVars a where
  var :: String -> a


{-
Start out by creating a new data type VarExprT which is the same
as ExprT but with an extra constructor for variables. Make VarExprT
an instance of both Expr and HasVars. You should now be able to
write things like
*Calc> add (lit 3) (var "x") :: VarExprT
But we can’t stop there: we want to be able to interpret expressions
containing variables, given a suitable mapping from variables
to values.
-}
data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

{-
Implement the following instances:
instance HasVars (M.Map String Integer -> Maybe Integer)
instance Expr (M.Map String Integer -> Maybe Integer)

The first instance says that variables can be interpreted as
functions
  from a mapping of variables to Integer values
     to (possibly)
      Integer values.
(a function that takes a mapping and returns a possible integer)

It should work by looking up the variable in the
mapping. (the mapping in this use case is provided from the [(String, Integer)] list globally)
-}
instance HasVars (M.Map String Integer -> Maybe Integer) where
  -- var :: String -> (M.Map String Integer -> Maybe Integer)
  -- which is a function that takes two arguments: String, the mapping (M.Map String Integer -> Maybe Integer)

  -- var s = lookup s
  -- type of lookup s is (m->i)
  var s m = M.lookup s m  -- var = M.lookup

{-
The second instance says that these same functions can be interpreted
as expressions (by passing along the mapping to subexpressions
and combining results appropriately).

Note: to write these instances you will need to enable the FlexibleInstances language extension by putting
-# LANGUAGE FlexibleInstances #-
as the first line in your file.
-}

-- the expression is like a evaluation map
-- TODO: question: how's var get evaluated?
instance Expr (M.Map String Integer -> Maybe Integer) where
  -- lit i (literal of an integer) should return: (a function that takes a mapping and returns a possible integer)
  -- lit i m = Just i
  -- TODO:  lit i = id (doesn't change the map, returns the original map). No, not id here
  lit i _ = Just i  -- lit i = \m -> Just i

  -- for example, add (lit 1) (lit 2) should be (take a mapping) -> 3
  -- need to get the mapping from x, y, and union two maps
  -- pseudo code: add (m1->i1) (m2->i2) = (union m1 m2)->(i1 + i2)
  -- add :: (M.Map String Integer -> Maybe Integer) -> ((M.Map String Integer -> Maybe Integer) -> (M.Map String Integer -> Maybe Integer))
  -- TODO: what does combine do? combining two Maybe Integers
  -- lift? https://github.com/prakashk/cis-194/blob/master/wk05/Calc-ex06.hs
  add x y = let combine (Just i1) (Just i2) = Just (i1 + i2)
                combine _ _ = Nothing
                in \m -> combine (x m) (y m)

  mul x y = let combine (Just i1) (Just i2) = Just (i1 * i2)
                combine _ _ = Nothing
                in \m -> combine (x m) (y m)
{-
Once you have created these instances, you should be able to test
them as follows:
withVars :: [(String, Integer)]
-> (M.Map String Integer -> Maybe Integer)
-> Maybe Integer
withVars vs exp = exp $ M.fromList vs

*Calc> :t add (lit 3) (var "x")
add (lit 3) (var "x") :: (Expr a, HasVars a) => a -- generic type
*Calc> withVars [("x", 6)] $ add (lit 3) (var "x")
Just 9
*Expr> withVars [("x", 6)] $ add (lit 3) (var "y")
Nothing
*Calc> withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
Just 54
-}

-- withVars takes a [(String, Integer)] variable map (vs), and an expression (that implements Expr and HasVars)
-- withVars limits the exp type
-- the type of exp is: (a higher order function that takes a String,Integer map and returns a Maybe Integer)
withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

{-
*Calc> withVars [("x", 6)] $ add (lit 3) (var "x")
Just 9

(var "x") returns a function: (M.lookup "x") or \m->(M.lookup "x" m)
(lit 3) returns a function: \m -> Just i
add combines the two functions:
combine (\m->(M.lookup "x" m)) (\m -> Just i) = \m -> Just ((M.lookup "x" m) + i)
then apply the map to it.
-}
