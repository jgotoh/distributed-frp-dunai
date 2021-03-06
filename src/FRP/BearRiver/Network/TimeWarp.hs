{-# LANGUAGE TupleSections #-}

-- | This module provides a variant of the 'reactimate' function that uses Time Warp Synchronisation for use in server applications. 'reactimateTimeWarp' is important here, the rest is just exported for unit tests.

module FRP.BearRiver.Network.TimeWarp
  ( reactimateTimeWarp
  , stepSF
  , timeWarpStep
  , addProcessed
  , performRollback
  , rollbackInputs
  , processInputs'
  , processedAfterRollback
  , ProcessedInput(..)
  , rollbackSF
  )
where
import           Prelude                 hiding ( takeWhile
                                                , take
                                                , span
                                                , dropWhile
                                                , pi
                                                , all
                                                )
import           GHC.Exts
import           Control.Monad.Trans.MSF hiding ( step )
import           Data.Functor.Identity
import           Data.Maybe
import           Data.MessageBuffer
import           Data.MonadicStreamFunction.Network.TimeWarp
import           Data.Monoid
import           Numeric.Natural
import           FRP.BearRiver
import           Network.Common
import           Control.Monad.Trans.MSF.Except
                                               as MSF
                                         hiding ( step )
import           Data.MonadicStreamFunction.InternalCore
                                                ( MSF(..) )

-- | Convert to a 'SF' that saves its last 'n' continuations and is able to revert its state to a previous continuation. Selection is based on arrow input.
-- When passing 'x=0' as input, the sf will use its standard continuation.
-- Warping is irreversible. To catch up to future iterations, values have to be recalculated, because input could have changed.
-- This function does not allow side effectful SFs.
-- Note that it does not need to be manually called. When using reactimateTimeWarp, every SF is transformed by it.
rollbackSF :: Natural -> SF Identity a b -> SF Identity (Natural, a) b
rollbackSF = rollbackMSF

-- | Used in TimeWarp synchronisation to save processed sf inputs.
newtype ProcessedInput a msg = ProcessedInput (FrameNr, (a, [msg]))
  deriving Show

instance Eq (ProcessedInput a msg) where
  (==) (ProcessedInput (x, _)) (ProcessedInput (y, _)) = x == y

instance Ord (ProcessedInput a msg) where
  (<=) (ProcessedInput (x, _)) (ProcessedInput (y, _)) = x <= y

instance HasFrameAssociation (ProcessedInput a msg) where
  getFrame (ProcessedInput (x, _)) = x

-- | A version of 'reactimate' using Time Warp.
-- 'nin': type that is used as network input for 'sf'
-- 'any': result of sending states action. Will be discarded
-- 'senseI' is an initial sense action at time = 0
-- 'sense', gather input, calculate 'DTime'
-- 'actuate' output, render
-- 'sf' actual FRP. Will automatically rollback, if a message for an earlier frame arrives.
-- 'netin' action to receive commands
-- 'netout' action to send states
-- 'maxFrames' maximum number of frames that is saved for rollback
-- To detect, whether a message for an earlier frame arrives, network input 'nin' needs to be an instance of 'HasFrameAssociation'. 'Ord' instance is necessary to sort messages by 'FrameNr'.
-- Note that rollbackSF will be used in reactimateTimeWarp to transform 'sf'. So there is no need to manually transform it.
reactimateTimeWarp
  :: (Show a, Show nin, Monad m, HasFrameAssociation nin, Ord nin)
  => m a                                 -- ^ first sense
  -> (Bool -> m (DTime, Maybe a))        -- ^ sense
  -> (Bool -> b -> m Bool)               -- ^ actuate
  -> SF Identity (a, [nin]) b            -- ^ sf
  -> m [nin]                             -- ^ receive
  -> ((FrameNr, b) -> m any)             -- ^ send
  -> Natural                             -- ^ maximum number of frames to rollback
  -> m ()
reactimateTimeWarp senseI sense actuate sf' netin netout maxFrames = do
  let sf = rollbackSF maxFrames sf'
  MSF.reactimateB $ feedback mempty $ proc ((), unprocessed) -> do
    frameNr <- count -< ()
    (dt, input) <- senseSF senseI sense -< ()
    newMessages <- toMessageBuffer ^<< arrM (const netin) -< ()
    allUnprocessed <- arr (uncurry (<>)) -< (newMessages, unprocessed)
    (frameMessages, nextMessages) <-
      arr (\(n, xs) -> span ((<= n) . getFrame) xs) -< (frameNr, allUnprocessed)
    state' <-
      sfIO $ stepSF sf maxFrames -< (dt, (frameNr, (input, frameMessages)))
    exit' <- actuateSF actuate -< state'
    _     <- withSideEffect netout -< (frameNr, state')
    returnA -< (exit', nextMessages)
  return ()

-- | An MSF that performs rollbacks when necessary.
-- Uses feedback to save last n inputs
stepSF
  :: (Show a, Show msg, Monad m, Ord msg, HasFrameAssociation msg)
  => MSF m (Natural, (a, [msg])) b
  -> Natural
  -> MSF m (FrameNr, (a, MessageBuffer msg)) b
stepSF sf maxFrames = feedback mempty $ timeWarpStep sf maxFrames

-- | An MSF that performs rollbacks when necessary.
-- Saved inputs (sensed and commands) have to be manually passed into it, see 'stepSF'.
timeWarpStep
  :: (Show a, Show msg, Monad m, Ord msg, HasFrameAssociation msg)
  => MSF m (Natural, (a, [msg])) b -- Natural is FrameDt!
  -> Natural
  -> MSF
       m
       ( (FrameNr, (a, MessageBuffer msg))
       , MessageBuffer (ProcessedInput a msg)
       )
       (b, MessageBuffer (ProcessedInput a msg))
timeWarpStep sf maxFrames = MSF $ \((n, (a, qi)), pi) -> do
  let mT0 = getFrame <$> firstMessage qi
  ((b, sf'), pi') <- case mT0 of
    Nothing -> do -- qi is empty
      (b, sf') <- unMSF sf (0, (a, []))
      let pi' = addProcessed pi n (a, qi)
      return ((b, sf'), pi')
    Just t0
      | t0 < n -> do
        let pi' = processedAfterRollback pi n a qi  -- all processed inputs after rollback
            qi' = rollbackInputs pi' t0 n -- input when rolling back
        msf <- performRollback sf qi'
        return (msf, pi')
      | t0 == n -> do-- processInput sf pi n (0, (a, qi)) -- step
        (b, sf') <- unMSF sf (0, (a, toList qi))
        let pi' = addProcessed pi n (a, qi)
        return ((b, sf'), pi')
      | t0 > n -> error "t0 > n, inputs from the future can not be processed"
      | otherwise -> error "should not happen"
  return ((b, takeTail maxFrames pi'), timeWarpStep sf' maxFrames)

-- | Adds a single input '(a, msg)' for a frame to an existing buffer
addProcessed
  :: (Ord msg)
  => MessageBuffer (ProcessedInput a msg)
  -> FrameNr
  -> (a, MessageBuffer msg)
  -> MessageBuffer (ProcessedInput a msg)
addProcessed pi frame (a, ms) =
  pi <> singleton (ProcessedInput (frame, (a, toList ms)))

-- | Adds 'a' to 'pi', then merges 'qi' into corresponding 'ProcessedInputs' (inputs with matching FrameAssociation/ FrameNr)
processedAfterRollback
  :: (HasFrameAssociation msg, Ord msg)
  => MessageBuffer (ProcessedInput a msg)
  -> FrameNr
  -> a
  -> MessageBuffer msg
  -> MessageBuffer (ProcessedInput a msg)
processedAfterRollback pi n a qi = do
  let pi' = pi <> singleton (ProcessedInput (n, (a, [])))
  mergeB pi' qi

-- | Convertes sublist of ProcessedInput starting at 't0' to unprocessed input.
rollbackInputs
  :: MessageBuffer (ProcessedInput a msg)
  -> FrameNr
  -> FrameNr
  -> [(Natural, (a, [msg]))]
rollbackInputs pi t0 t =
  let pi' = toList $ dropWhile ((< t0) . getFrame) pi
      dt  = t - t0
      dts = pure dt <> repeat 0
  in  zipWith inputWithDt pi' dts

-- Converts a ProcessedInput to a MSF input with its frame delta set to n'.
inputWithDt :: ProcessedInput a msg -> FrameNr -> (Natural, (a, [msg]))
inputWithDt (ProcessedInput (_, x)) n' = (n', x)

-- | Applies a rolled back MSF to a list of inputs. Returns the last returned value and continuation.
performRollback
  :: Monad m
  => MSF m (Natural, (a, [msg])) b
  -> [(Natural, (a, [msg]))]
  -> m (b, MSF m (Natural, (a, [msg])) b)
performRollback sf xs = do
  bs <- processInputs' sf xs
  return $ last bs

-- | Applies a rolled back MSF to a list of inputs. Returns list of all returned values and continuations.
processInputs'
  :: Monad m
  => MSF m (Natural, (a, [msg])) b
  -> [(Natural, (a, [msg]))]
  -> m [(b, MSF m (Natural, (a, [msg])) b)]
processInputs' _  []       = return []
processInputs' sf (a : as) = do
  b@(_, sf') <- unMSF sf a
  bs         <- processInputs' sf' as
  return (b : bs)

senseSF
  :: (Monad m, Num a1) => m c -> (Bool -> m (a1, Maybe c)) -> MSF m a (a1, c)
senseSF senseI sense = switch' (senseFirst senseI) (senseRest sense)

senseFirst :: (Monad m, Num a1) => m b -> MSF m a2 ((a1, b), Event b)
senseFirst senseI = constM senseI >>> arr (\x -> ((0, x), Event x))

senseRest :: Monad m => (Bool -> m (c, Maybe c')) -> c' -> MSF m a (c, c')
senseRest sense a = constM (sense True) >>> (arr id *** keepLast a)

keepLast :: Monad m => a -> MSF m (Maybe a) a
keepLast a = MSF $ \ma -> let a' = fromMaybe a ma in return (a', keepLast a')

switch' :: Monad m => MSF m a (b, Event c) -> (c -> MSF m a b) -> MSF m a b
switch' sf' = MSF.switch (sf' >>> second (arr eventToMaybe))

-- Consume/render
actuateSF :: Monad m => (Bool -> a -> m c) -> MSF m a c
actuateSF actuate = arr (True,) >>> arrM (uncurry actuate)

sfIO :: Monad m => MSF (ReaderT r Identity) a b -> MSF m (r, a) b
sfIO sf = morphS (return . runIdentity) (runReaderS sf)

-- | Merge buffer 'qi' into 'pi'.
-- Every element with equal framenumbers in 'pi' and 'qi' will be merged.
-- Elements in 'qi', which do not have a corresponding element in 'pi' are discarded.
mergeB
  :: (Ord msg, HasFrameAssociation msg)
  => MessageBuffer (ProcessedInput a msg)
  -> MessageBuffer msg
  -> MessageBuffer (ProcessedInput a msg)
mergeB pi qi = mergeMatches pi qi (\p q -> getFrame p == getFrame q) mergePiQi

-- For every a in as: checks if there is an element b in bs where p a b is true. If true, applies merge function to both matching elements.
-- If first list is empty, so will the result. If second list is empty, as will be returned.
mergeMatches
  :: (Ord a, Ord b)
  => MessageBuffer a
  -> MessageBuffer b
  -> (a -> b -> Bool)
  -> (a -> b -> a)
  -> MessageBuffer a
mergeMatches as bs p f = case toList as of
  a : as' -> mergeMatch a bs p f <> mergeMatches (fromList as') bs p f
  []      -> as

mergeMatch
  :: (Ord a, Ord b)
  => a
  -> MessageBuffer b
  -> (a -> b -> Bool)
  -> (a -> b -> a)
  -> MessageBuffer a
mergeMatch a bs p f = case toList bs of
  []      -> singleton a
  b : bs' -> if p a b
    then mergeMatch (f a b) (fromList bs') p f -- recursive call with merged a to be able to further merge into a
    else mergeMatch a (fromList bs') p f

mergePiQi :: ProcessedInput a msg -> msg -> ProcessedInput a msg
mergePiQi (ProcessedInput (n, (a, msgs))) m =
  ProcessedInput (n, (a, msgs ++ [m]))
