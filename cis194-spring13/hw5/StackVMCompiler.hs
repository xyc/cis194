{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module StackVMCompiler where

import StackVM
import Parser

{-
this is a compiler that does code generation for stackVM.
-}



-- copied from Calc.hs
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

{-
The folks down in hardware have finished our new custom CPU,
so we’d like to target that from now on. The catch is that a stackbased
architecture was chosen to save money. You need to write a
version of your calculator that will emit assembly language for the
new processor.
The hardware group has provided you with StackVM.hs, which
is a software simulation of the custom CPU. The CPU supports six
operations, as embodied in the StackExp data type:
data StackExp = PushI Integer
| PushB Bool
| Add
| Mul
| And
| Or
deriving Show
type Program = [StackExp]
PushI and PushB push values onto the top of the stack, which can
store both Integer and Bool values. Add, Mul, And, and Or each pop
the top two items off the top of the stack, perform the appropriate
operation, and push the result back onto the top of the stack. For
example, executing the program
[PushB True, PushI 3, PushI 6, Mul]
will result in a stack holding True on the bottom, and 18 on top of
that.
If there are not enough operands on top of the stack, or if an operation
is performed on operands of the wrong type, the processor
will melt into a puddle of silicon goo. For a more precise specification
of the capabilities and behavior of the custom CPU, consult the
reference implementation provided in StackVM.hs.

Your task is to implement a compiler for arithmetic expressions.
Simply create an instance of the Expr type class for Program, so that
arithmetic expressions can be interpreted as compiled programs. For
any arithmetic expression exp :: Expr a => a it should be the case
that
stackVM exp == Right [IVal exp]
-- Right: correct program
-- Left: corrupt program

Note that in order to make an instance for Program (which is a
type synonym) you will need to enable the TypeSynonymInstances
language extension, which you can do by adding
- # LANGUAGE TypeSynonymInstances # -
(A type synonym is a new name for an existing type like typedef in C)
as the first line in your file. (A type synonym is a new name for an existing type.)

Finally, put together the pieces you have to create a function
compile :: String -> Maybe Program
which takes Strings representing arithmetic expressions and compiles
them into programs that can be run on the custom CPU.
-}

-- what is the result of converting a literal to a program?
-- what is the result of combining two programs by "+"/"*"?
-- parse exp will construct the parse result according to lit, add, mul
-- Program is a type synonym (type Program = [StackExp], like type String = [Char])
instance Expr Program where
  -- lit :: Integer -> Program
  lit x = [PushI x]
  -- add :: Program -> Program
  -- p1, p2, operator
  -- p1, p2 should be returning a value and sit on the bottom of the stack, operator will be pushed onto the top
  -- note that the program sequence here is not the stack
  add p1 p2 = p1 ++ p2 ++ [Add]
  -- mul :: Program -> Program
  mul p1 p2 = p1 ++ p2 ++ [Mul]

compile :: String -> Maybe Program
-- compile s = parseExp lit add mul s
-- eta reduction (to point free style)
compile = parseExp lit add mul

-- Expect:
-- Just [PushI 3,PushI (-4),Mul,PushI 5,Add]
-- the arguments of parseExp should implement lit, add, mul
-- testProgram = parseExp lit add mul "(3 * -4) + 5"
-- testProgram :: Expr a => Maybe a
testProgram :: Maybe Program
testProgram = compile "(3 * -4) + 5"

-- usage: testStackVM $ compile "..."
-- usage: testStackVM testProgram
-- expected: "IVal (-7)"
testStackVM :: Maybe Program -> String
testStackVM (Just program) = case stackVM program of
  Left err -> err
  Right stackVal -> show stackVal
testStackVM Nothing = "Failed to compile"
