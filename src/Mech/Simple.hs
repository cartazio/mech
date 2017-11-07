{-# LANGUAGE ScopedTypeVariables, DataKinds, KindSignatures, GADTs  #-}
module Mech.Simple where

import Mech.Agent

import qualified Data.Machine.Type as MT
import Data.Profunctor.Unsafe ((#.))

{- | `Mech.Agent.Simple` models the exact vocab / power of machines,
  as a language / instance of the Mech EDSL.
  When a `Simple` machine is translated into a ``
  -}
data Simple :: ( * -> *) -> * ->  Polarity -> * -> * -> * where
  SimpAwait :: forall k  t  . k t -> Simple k t 'IN () t
  SimpYield :: Simple k o 'OUT o ()


simplePlansConstructSimpleMachine  :: forall k o m a  rxo  txk  .
    Monad m =>
    AgenT (Simple k rxo) (Simple txk o)  m a ->
    MT.MachineT m k o
simplePlansConstructSimpleMachine  m = MT.MachineT $ runAgenT m
  (const (return MT.Stop))
  (\ oOut oVal resumek _failk ->
    case oOut of
      SimpYield ->  return (MT.Yield oVal (MT.MachineT (resumek () ))))
  (\  witness _boring successF   failure  ->
      case witness of
        SimpAwait kwit ->
          return (MT.Await (MT.MachineT #. successF ) kwit
                     (MT.MachineT failure)))

  (return MT.Stop)

