{-# LANGUAGE GADTs, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveDataTypeable, ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables, DataKinds  #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Mech.Agent(
  AgenT(AgenT)
  ,runAgenT
  ,Polarity(..)
  ,type IN
  ,type OUT
  ,ReversePolarity(..)
  ,MechStep(..)
  )
   where



--import Data.Coerce
--import Data.Semigroup as S
--import Data.Monoid as M
--import Data.Semigroupoid
--import Control.Category
--import Data.Proxy
--import Data.Semigroup



import Prelude hiding ((.),id)
import qualified Data.Machine.Type as MT
import Data.Profunctor.Unsafe ((#.))

data Polarity = IN | OUT

--- these are just so that folks needed use datakinds unless they want to
type IN = 'IN
type OUT = 'OUT

{-
maybe an AgenT is just a fancy PlanT ;)

-}

newtype AgenT {- k o m a -}
  (rx :: Polarity -> * -> * -> * )
  (tx :: Polarity -> * -> * -> * )
  (m :: * -> * ) {- the model of effects/sequencing /io etc -}
  (a :: * ) {- the "return value "-}
   where
    AgenT ::   { runAgenT :: (forall r.
          (a -> m r) ->                                     -- Done a
          --(o -> m r -> m r) ->                              -- Yield o (Plan k o a)
          (forall g h. tx 'OUT  g h -> g  ->  ( h -> m r) -> m r -> m r) ->
           -- ^ yield === transmit mode, addressing info, response/answer callback/continuation,  failure c
          (forall g h. rx 'IN  g h  -> g  ->  ( h -> m r) -> m r -> m r) ->
           -- ^  await ===  receive mode, receipt info, info callback/continuation,  failure c
          m r ->                                            -- Fail
          m r)
           } -> AgenT rx tx m a


 {--}
-- (forall z. (z -> m r) -> k z -> m r -> m r)

data ReversePolarity :: ( Polarity -> * -> * -> * ) -> Polarity -> * -> * -> * where
  RevOut :: (f 'IN  a b) -> ReversePolarity f 'OUT a b
  RevIn  :: (f 'OUT a b) -> ReversePolarity f 'IN a b

--data SimplePosix :: Polarity -> *  -> * -> * where
--  AtomicBroacast :: (String,ByteString) -> SimplePosix 'OUT (String,ByteString) Bool
  --ReceiveBroadCast ::
  --Receive


data MechStep --  (m  :: (* -> * ) )
              (rx :: Polarity -> *  -> * -> * )
              (tx :: Polarity -> *  -> * -> * )
              (r :: * )
          where
    MStop :: MechStep rx tx r
    MTransmit :: forall rx tx a b r.
        tx 'OUT a b
          -> a     {- address and payload -}
          -> ( b  -> r)      {- uusually () -> r , could be "success/failure"-}
          ->  MechStep rx tx r  -- AKA YIELD
          -- ^ transmit will usually be like posix send or friends
    MReceive :: forall a b tx rx  r .
       rx 'IN a b
       -> a                     {- who you're listening for -}
       -> ( b {- value -} -> r )
       -> MechStep rx tx r
        -- ^ await / Receive transmission of type b , with argument info a
        -- usually like

    --MReceive  :: rx 'IN  a b -> a -> (b -> r) ->  MechStep rx tx r -- await

{-

newtype MachineT m k o = MachineT { runMachineT :: m (Step k o (MachineT m k o)) }
-- Note: A 'Machine' is usually constructed from 'Plan', so it does not need to be CPS'd.

data Step k o r
  = Stop
  | Yield o r
  | forall t. Await (t -> r) (k t) r


-- | Compile a machine to a model.
construct :: Monad m => PlanT k o m a -> MachineT m k o
construct m = MachineT $ runPlanT m
  (const (return Stop))
  (\o k -> return (Yield o (MachineT k)))
  (\f k g -> return (Await (MachineT #. f) k (MachineT g)))
  (return Stop)

newtype PlanT k o m a = PlanT
  { runPlanT :: forall r.
      (a -> m r) ->                                     -- Done a
      (o -> m r -> m r) ->                              -- Yield o (Plan k o a)
      (forall z. (z -> m r) -> k z -> m r -> m r) ->    -- forall z. Await (z -> Plan k o a) (k z) (Plan k o a)
      m r ->                                            -- Fail
      m r
  }

-- A @'Plan' k o a@ can be used as a @'PlanT' k o m a@ for any @'Monad' m@.
--
-- It is perhaps easier to think of 'Plan' in its un-cps'ed form, which would
-- look like:
--
-- @
-- data 'Plan' k o a
--   = Done a
--   | Yield o (Plan k o a)
--   | forall z. Await (z -> Plan k o a) (k z) (Plan k o a)
--   | Fail
-- @
type Plan k o a = forall m. PlanT k o m a

-- | Deconstruct a 'Plan' without reference to a 'Monad'.
runPlan :: PlanT k o Identity a
        -> (a -> r)
        -> (o -> r -> r)
        -> (forall z. (z -> r) -> k z -> r -> r)
        -> r
        -> r
runPlan m kp ke kr kf = runIdentity $ runPlanT m
  (Identity . kp)
  (\o (Identity r) -> Identity (ke o r))
  (\f k (Identity r) -> Identity (kr (runIdentity . f) k r))
  (Identity kf)
{-# INLINE runPlan #-}
-}
