module Data.MonadicStreamFunction.TimeWarp
  ( consCap
  , selectSF
  , warpSF
  , toWarpSF
  , module Numeric.Natural
  )
where

import           Data.MonadicStreamFunction.InternalCore
import           Numeric.Natural
import           Data.List

-- bounded cons x:xs. If length of x:xs exceeds maximum length, surplus elements at the tail are discarded.
consCap :: Natural -> a -> [a] -> [a]
consCap n a as = genericTake n $ a : as

-- Selects a MSF based on input n.
-- If n == 0 returns sf, else first, second... elements of cs.
-- On n > 0, element of an earlier time are discarded.
selectSF
  :: Monad m
  => Natural
  -> MSF m a b
  -> [MSF m a b]
  -> m (MSF m a b, [MSF m a b])
selectSF n sf cs
  | n == 0 = return (sf, cs)
  | n > 0 = case (drop (fromIntegral n) (sf : cs)) of
    sf' : cs' -> return (sf', cs')
    []        -> error $ "sf at index " ++ show n ++ " does not exist"
  | otherwise = error
    "can not happen, because n either equals 0 or is greater than 0"

-- TODO rename to warpMSF
-- Convert to an MSF that saves its last continuations and is able to revert its state to a previous continuation. Selection is based on arrow input. Maximum number of last continuations is limited to n.
-- Is only necessary for temporally directed MSFs.
-- When passing x=0 as input, the sf will use its standard continuation.
-- Warping is irreversible. To catch up to future iterations, values have to be recalculated, because input could have changed.
-- warpSF :: Monad m => Natural -> MSF m a b -> MSF m (Natural, a) b
warpSF :: Monad m => Natural -> MSF m a b -> MSF m (Natural, a) b
warpSF n sf = feedback [] $ toWarpSF n sf

-- Should be used in conjunction with feedback to get previous continuations in following iterations.
-- Returns its last continuations (size is limited to maxCs) and the current output.
toWarpSF
  :: Monad m
  => Natural
  -> MSF m a b
  -> MSF m ((Natural, a), [MSF m a b]) (b, [MSF m a b])
toWarpSF maxCs sf = MSF $ \((n, a), cs) -> do
  -- sf':= which sf to apply based on n, cs':= updated conts
  (sf', cs') <- (selectSF n sf cs)
  -- apply sf', returns b and next continuation
  (b  , c  ) <- unMSF sf' a
  let cs'' = consCap maxCs sf' cs'
  return ((b, cs''), toWarpSF maxCs c)

