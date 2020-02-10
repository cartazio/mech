{-# LANGUAGE GADTs, RankNTypes, MultiParamTypeClasses, TypeFamilies  #-}
-- {-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveDataTypeable, ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE TypeInType, TypeOperators #-}

{- |   This module adapts the 4 connective algebra of
  full classical linear logic
Negation switches polarity, and for simplicity, we restrict to negation normal form,
IE: negation only on the leaves

  -}
module Mech.Protocol
  --( Formula(..)
  --                    ,Negate

  --                  )
  where
import Data.Kind
--import Control.Category
--import Data.Semigroupoid
import Data.Proxy



infixr 7 :+
infixr 7 :*
infixr 7 :^
-- infixr 7 :|

-- We only allow negated normal form, so negations are only on leaves
data Formula  (t :: Type ) where
    (:+) ::  Formula t -> Formula t -> Formula t    -- ^ sum
    (:*) ::  Formula t -> Formula t -> Formula t   -- ^ product
    --(:|) ::  Formula t -> Formula t -> Formula t   -- ^ par (classical sum/or)
    -- we treat negation on product as multiplicative inverse, so  not ( A:*B) == not A :* not B == A `Par` B
    -- which collapes away Par.
    (:^) ::  Formula t -> Formula t -> Formula t  -- ^ choice / with
    In   ::  t -> Formula t  -- positive position, ie "give/Send"
    Neg  ::  t -> Formula t -- negative postion, ie "take/Receive"
type Neg = 'Neg  -- '
type In = 'In  -- '
type (:+) a b =  a ':+  b
type (:*) a b =  a ':* b
-- type (:|) a b =  a ':| b
type (:^) a b =  a ':^  b
type family Negate  x   where
  Negate ('In t) = 'Neg t
  Negate ('Neg t)= 'In t
  Negate (x ':* y) = Negate x ':* Negate y
  Negate (x ':+ y) = Negate x ':^ Negate y
  Negate (x ':^ y) = Negate x ':+ Negate y


data TrivialCat :: forall (x :: Type ) .  Formula x -> Formula x -> Type   where
  TrivialCat :: forall a b  na .  ( a ~ Negate na , na ~ Negate a) => Proxy na -> Proxy  b -> TrivialCat a b


--- | This probably is wrong for nested formulae, fix that
--- Idea: have the choice tree build up a  N->M function of the selections ?
--- One tricky bit : how does choice leak into this and or Par?
--- this kinda looks like it might need to be a monad transformer?
{-
naively, for simple things we want to roughly split our way of thinking
between push models and pull models



consider f to be a streaming thing from A to B, g from B to C,
aka f is upstream, g is down stream
in Push land, we have  f -> g , f acts, , then g reacts, f then may resume

in pull land, we have f -> g, g acts, then f reacts, then g may resumed

-}

data PureChoice :: (Formula Type )  -> Type -> Type  where
     Give :: x  -> PureChoice (In x) ()
     Receive :: PureChoice (Neg x) x
     SumLeftPrf :: x ->  PureChoice (In x) () ->  PureChoice ( In x :+ y) ()
     SumLeftRft :: PureChoice (Neg x)  x ->  PureChoice ( Neg x :+  y) x
     SumRight :: PureChoice (In y) yres -> PureChoice (x :+  In y) yres
     PairBothPfPf  :: PureChoice (In x) xres -> PureChoice (In y) yres -> PureChoice (In x :*  In y) ()

data HList :: [Type] -> Type where
   HNil :: HList '[]
   HCons :: forall (t :: Type) (ts :: [Type]) . t -> HList ts -> HList (t ': ts)

data Mode = Act | React
type Act = 'Act
type React = 'React
{-
the acting/initiating side (ie upstream in push, downstream in pull)
  selects

-}
--data ProofT



data TermT :: Formula Type -> Mode ->  (Type  -> Type) -> Type -> Type where


data GForm :: Type -> Type where
    And :: forall (a :: Type ) . GForm a -> GForm a


--data InteracT (p :: Mode) (m :: Type -> Type )


