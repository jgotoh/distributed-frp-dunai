-- | Some additional functions related to BearRiver.

{-# LANGUAGE RankNTypes #-}
module FRP.BearRiver.Extra
  ( edgeJust
  , embedSF
  )
where

import           Network.Common
import           Control.Monad.Trans.MSF
import           Control.Monad.Trans.MSF.Except
                                               as MSF
import           Data.Functor.Identity
import           Data.Maybe
import           Data.MonadicStreamFunction.InternalCore
import           FRP.BearRiver           hiding ( edgeJust )

-- | 'edge' function for 'Maybe' values. Taken from https://hackage.haskell.org/package/Yampa-0.13.1/docs/src/FRP.Yampa.EventS.html#edgeJust
edgeJust :: Monad m => SF m (Maybe a) (Event a)
edgeJust = edgeBy isJustEdge (Just undefined)
 where
  isJustEdge Nothing  Nothing     = Nothing
  isJustEdge Nothing  ma@(Just _) = ma
  isJustEdge (Just _) (   Just _) = Nothing
  isJustEdge (Just _) Nothing     = Nothing

-- | Apply an SF to a list of inputs and delta time values.
embedSF :: Monad m => SF m a b -> [(DTime, a)] -> m [b]
embedSF _  []              = return []
embedSF sf ((dt', a) : as) = do
  (b, sf') <- runReaderT (unMSF sf a) dt'
  bs       <- embedSF sf' as
  return (b : bs)

