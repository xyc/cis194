{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module HW08 where

import Prelude (Show(..), Eq(..), ($), (.), flip)


-- Propositional Logic --------------------------------

-- False, the uninhabited type
data False

-- Logical Not
type Not p = p -> False

-- Logical Disjunction
data p \/ q = Left  p
            | Right q

-- Logical Conjunction
data p /\ q = Conj p q

-- If and only if
type p <-> q = (p -> q) /\ (q -> p)

-- Admit is used to assume an axiom without proof
admit :: p
admit = admit

-- There is no way to prove this axiom in constructive logic, therefore we
-- leave it admitted
excluded_middle :: p \/ Not p
excluded_middle = admit

absurd :: False -> p
absurd false = admit

double_negation :: p <-> Not (Not p)
double_negation = Conj (\p not_p -> not_p p) admit

modus_ponens :: (p -> q) -> p -> q
modus_ponens = ($)

modus_tollens :: (p -> q) -> Not q -> Not p
modus_tollens = flip (.)

material_implication :: (p -> q) <-> (Not p \/ q)
-- The proof has two parts, the forward direction (->) and
--   the backwards direction (<-)
material_implication = Conj dir1 dir2
    where
      -- Case 1: (P -> Q) -> (~P \/ Q)
      dir1 p_imp_q =
          -- There are 2 cases, P and ~P
          case excluded_middle of
            -- SCase 1: P, then Q since P -> Q
            Left  p     -> Right $ p_imp_q p
            -- SCase 2: ~P, then ~P
            Right not_p -> Left not_p
      -- Case 2: (~P \/ Q) -> (P -> Q)
      -- SCase 1: ~P -> (P -> Q)
      dir2 (Left not_p) p =
          -- This is a contradiction since we have both
          -- P and ~P
          absurd $ not_p p
      -- SCase 2: Q -> (P -> Q)
      dir2 (Right q)    _ =
          -- q is a witness for the proposition Q,
          -- therefore we can just return it
          q

-- Exercise 1 -----------------------------------------

{-
it states that if one of P or Q is true, but we know that P is false, then Q must be true.

The proof is relatively straighforward; you should do case analysis on the proposition P ∨ Q.
  In the left case, you can derive a contradiction
  and in the right case, you simply know that Q is true.
-}
disjunctive_syllogism :: (p \/ q) -> Not p -> q
disjunctive_syllogism p_or_q not_p = case p_or_q of
                                        Left p -> absurd $ not_p p
                                        Right q -> q

-- Exercise 2 -----------------------------------------
{-
The proof of this theorem is again by case analysis on the disjunction
hypothesis.
The left case in the hypothesis corresponds to the left case in the conclusion
and the right case in the hypothesis corresponds to the right case in the conclusion.

-}
composition :: (p -> q) \/ (p -> r) -> p -> q \/ r
composition pq_or_pr p = case pq_or_pr of
                            Left pq -> Left $ pq p -- p implies q, p -> q (modus_ponens)
                            Right pr -> Right $ pr p

-- Exercise 3 -----------------------------------------
{-
(P → Q) ↔ (¬Q → ¬P)

The proof of Transposition has two parts; the forward direction and
the backwards direction. In the forward direction, you are essentially
proving:

P → Q
¬Q
---
¬P

This is identical to a theorem that we already proved!

The backwards direction takes a bit more work
¬Q → ¬P
P
---
Q

This can be proven using an application of Modus Tollens, but with
a slight catch. You will have to get P into the form ¬¬P before you
can use Modus Tollens and then you will have to remove the double
negation at the end.

modus_tollens :: (p -> q) -> Not q -> Not p
-}
transposition :: (p -> q) <-> (Not q -> Not p)
transposition = Conj dir1 dir2
  where
    -- pq, not_q implies not_p
    -- dir1 pq not_q = not_q (.) pq
    dir1 = modus_tollens
    dir2 not_q_implies_not_p p = double_negation2_dir2 $ modus_tollens not_q_implies_not_p (double_negation_dir1 p)
      -- double_negation_dir1 :: p -> Not (Not p)
      -- double_negation_dir2 :: Not (Not p) -> p
      where (Conj double_negation_dir1 double_negation2_dir2) = double_negation

-- transposition = Conj dir1 dir2
--   where
--     dir1 = admit
--     dir2 = admit

-- Exercise 4 -----------------------------------------

{-
Prove one of De Morgan’s Laws
¬(P ∨ Q) ↔ (¬P ∧ ¬Q)

you will have to prove the forward and backward directions.
Recall that ¬P is defined as P → ⊥ and you can construct a proposition of type P ∨ Q with either a P or a Q.

Not (p \/ q) :: (p \/ q) -> False

x :: p
Left x :: p V q
not__p_or_q (Left x) :: (p \/ q) -> False

y :: q
Right y :: p \/ q
not__p_or_q (Right x) :: (p \/ q) -> False

(Not p /\ Not q) :: Conj (p -> False) (q -> False)
https://www.reddit.com/r/haskellquestions/comments/3bl8j1/stuck_on_cis194_hw08_ex_3_transposition/
-}
de_morgan :: Not (p \/ q) <-> (Not p /\ Not q)
de_morgan = Conj dir1 dir2
  where
    -- not_disjunction : either (not p or not q)
    -- not_disjunction :: Not (p \/ q)
    -- not_disjunction :: (p \/ q) -> False
    -- P ∨ Q → ⊥
    -- type Not p = p -> False
    -- data p /\ q = Conj p q
    -- data p \/ q = Left p | Right q
    -- either p or q is false, prove (not p) and (not q) is true
    dir1 not__p_or_q = Conj (not__p_or_q . Left) (not__p_or_q . Right)
    -- Conj (\x -> not_p_or_q (Left x)) (\y -> not_p_or_q (Right y))
    dir2 (Conj not_p not_q) (Left x) = not_p x
    dir2 (Conj not_p not_q) (Right y) = not_q y
    -- dir2 (Conj not_p not_q) p_or_q = not_q $ disjunctive_syllogism p_or_q not_p

-- Natural Numbers ------------------------------------

data Nat = O | S Nat
           deriving (Show, Eq)

type family (n :: Nat) + (m :: Nat) :: Nat
type instance O     + m = m
type instance (S n) + m = S (n + m)
infixl 6 +

data Forall n where
    Zero :: Forall O
    Succ :: Forall n -> Forall (S n)

data (n :: Nat) == (m :: Nat) where
    Refl :: n == n
infix 4 ==

type (n :: Nat) /= (m :: Nat) = Not (n == m)
infix 4 /=

data n < m where
  LT_Base :: O < S n
  LT_Rec  :: n < m -> S n < S m

type n >  m = m < n
type n <= m = (n < m) \/ (n == m)
type n >= m = m <= n

-- Weakening Lemma
neq_weaken :: S n /= S m -> n /= m
neq_weaken h_neq Refl = h_neq Refl

{- ********************************************************
   * Theorem: Not Equal Implies Greater Than or Less Then *
   ******************************************************** -}
neq_gt_lt :: Forall n -> Forall m ->
             n /= m -> (n < m) \/ (n > m)
-- The proof is by induction on n and m
-- Base Case 1: both n and m are 0. This is impossible since the hypothesis h
--   states that n /= m
neq_gt_lt Zero  Zero        h = absurd $ h Refl
-- Base Case 2: n == 0 and m > 0. Here we choose the left case, n < m
neq_gt_lt Zero (Succ m)     _ = Left  LT_Base
-- Base Case 3: n > 0 and m == 0. Here we choose the right case, n > m
neq_gt_lt (Succ n) Zero     _ = Right LT_Base
-- Inductive Step: both n and m are greater than 0
neq_gt_lt (Succ n) (Succ m) h_neq =
    -- We generate an induction hypothesis by invoking a recursive call on n,
    -- m, and the weakening hypothesis
    case neq_gt_lt n m (neq_weaken h_neq) of
      -- Case 1: n < m with witness w. This means that S n < S m
      Left  w -> Left  $ LT_Rec w
      -- Case 2: n > m with witness w. This means that S n > S m
      Right w -> Right $ LT_Rec w

-- Exercise 5 -----------------------------------------
{-
  theorem O + n == n
-}
o_plus_n :: O + n == n
o_plus_n = Refl

{-
  n + 0 == n
-}
n_plus_0 :: Forall n -> n + O == n
-- Base Case:
n_plus_0  Zero    = Refl {- :: O + O == O -}
-- Inductive Step:
n_plus_0 (Succ n) = case n_plus_0 n of
                      Refl   {- :: n   + O == n   -} ->
                        Refl {- :: S n + O == S n -}

{-
  0 + n == n + 0
-}
add_zero :: Forall n -> O + n == n + O
add_zero Zero = Refl {- :: O + O == O + O -}
add_zero (Succ n) = case add_zero n of
                      Refl ->
                        Refl

-- Exercise 6 -----------------------------------------

{-
  prove the property that ∀n, n < n + 1 (n is smaller than its successor)

LT_Base :: O < S n
LT_Rec  :: n < m -> S n < S m
-}
n_lt_sn :: Forall n -> n < S n
-- 0 is smaller than successor of any number
n_lt_sn Zero = LT_Base
-- since n is smaller than its successor,
-- according to the recursive definition of "less than" relation
-- n's successor is also smaller than the successor of n's successor
-- LT_Rec $ n_lt_sn n :: S n < S (S n)
n_lt_sn (Succ n) = LT_Rec $ n_lt_sn n

-- Exercise 7 -----------------------------------------

data Even :: Nat -> * where
    E_Zero :: Even O
    E_Rec  :: Even n -> Even (S (S n))

data Odd :: Nat -> * where
    O_One :: Odd (S O)
    O_Rec :: Odd n -> Odd (S (S n))

even_plus_one :: Even n -> Odd (S n)
-- Base Case: The successor of zero is odd
even_plus_one  E_Zero   = O_One
-- Inductive Step: if S (S n) is even then S (S (S n)) is odd
even_plus_one (E_Rec n) = O_Rec $ even_plus_one n

{-
Your job is to prove that the successor of an odd number is even
-}
odd_plus_one :: Odd n -> Even (S n)
-- Base Case: The successor of one is even
odd_plus_one O_One = E_Rec E_Zero
odd_plus_one (O_Rec n) = E_Rec $ odd_plus_one n

-- Exercise 8 -----------------------------------------

succ_sum :: Forall n -> Forall m ->
            S n + S m == S (S (n + m))
succ_sum  Zero    m = Refl
succ_sum (Succ n) m = case succ_sum n m of
                        Refl -> Refl

{-
 (n+1) + (n+1) == (n + n + 2) , (S (S (n+n)))

E_Rec  :: Even n -> Even (S (S n))
if n is even, then n + 2 is even
-}
double_even :: Forall n -> Even (n + n)
double_even Zero = E_Zero
double_even (Succ n) = case succ_sum n n of
                          Refl -> E_Rec $ double_even n
