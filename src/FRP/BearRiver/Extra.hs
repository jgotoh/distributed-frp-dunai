-- | Some additional functions related to BearRiver.

{-# LANGUAGE RankNTypes #-}
module FRP.BearRiver.Extra
  (
  embedSF
  )
where

import           Control.Monad.Trans.MSF
import           Data.MonadicStreamFunction.InternalCore
import           FRP.BearRiver

-- | Apply an SF to a list of inputs and delta time values.
embedSF :: Monad m => SF m a b -> [(DTime, a)] -> m [b]
embedSF _  []              = return []
embedSF sf ((dt', a) : as) = do
  (b, sf') <- runReaderT (unMSF sf a) dt'
  bs       <- embedSF sf' as
  return (b : bs)

