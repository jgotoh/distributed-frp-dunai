module FRP.BearRiver.TimeWarp
  ( reactimateTimeWarp
  , stepSF
  , timeWarpStep
  , addProcessed
  , performRollback
  , rollbackInputs
  , processInputs'
  , processedAfterRollback
  , HasFrameAssociation(..)
  , ProcessedInput(..)
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
import           Data.Monoid
import           Numeric.Natural
import           FRP.BearRiver
import           Network.Common
import           Control.Monad.Trans.MSF.Except
                                               as MSF
                                         hiding ( step )
import           Data.MonadicStreamFunction.InternalCore
                                                ( MSF(..) )

-- Type that are associated with a specific numbered frame
class HasFrameAssociation a where
  -- returns its FrameNr
  getFrame :: a -> Natural

-- Used in TimeWarp synchronisation to save processed sf inputs.
newtype ProcessedInput a msg = ProcessedInput (FrameNr, (a, [msg]))
  deriving Show

instance Eq (ProcessedInput a msg) where
  (==) (ProcessedInput (x, _)) (ProcessedInput (y, _)) = x == y

instance Ord (ProcessedInput a msg) where
  (<=) (ProcessedInput (x, _)) (ProcessedInput (y, _)) = x <= y

instance HasFrameAssociation (ProcessedInput a msg) where
  getFrame (ProcessedInput (x, _)) = x

-- TODO doc
-- code generation from proc syntax can lead to loss of performance apparently
-- see https://stackoverflow.com/questions/45260173/proc-syntax-in-haskell-arrows-leads-to-severe-performance-penalty
reactimateTimeWarp
  :: (Monad m, HasFrameAssociation p1, Ord p1)
  => m a -- first sense
  -> (Bool -> m (DTime, Maybe a)) -- sense
  -> (Bool -> b -> m Bool) -- actuate
  -> SF Identity (FrameNr, (a, [p1])) b -- sf
  -> m [p1] -- netin, no need to manually sort by frameNr
  -> ((FrameNr, b) -> m b2) -- netout
  -> Natural -- maximum number of frames to rollback
  -> m ()
reactimateTimeWarp senseI sense actuate sf netin netout maxFrames = do
  MSF.reactimateB $ feedback mempty $ proc ((), unprocessed) -> do
    frameNr <- count -< ()
    (dt, input) <- senseSF senseI sense -< ()
    newMessages <- toMessageBuffer ^<< arrM (\_ -> netin) -< ()
    allUnprocessed <- arr (uncurry (<>)) -< (newMessages, unprocessed)
    (frameMessages, nextMessages) <-
      arr (\(n, xs) -> span ((<= n) . getFrame) xs) -< (frameNr, allUnprocessed)
    state' <-
      sfIO $ stepSF sf maxFrames -< (dt, (frameNr, (input, frameMessages)))
    exit' <- actuateSF actuate -< state'
    _     <- withSideEffect netout -< (frameNr, state')
    returnA -< (exit', nextMessages)
  return ()

-- uses feedback to save last n inputs
-- decides whether to rollback and use previous inputs
stepSF
  :: (Monad m, Ord msg, HasFrameAssociation msg)
  => MSF m (Natural, (a, [msg])) b
  -> Natural
  -> MSF m (FrameNr, (a, MessageBuffer msg)) b
stepSF sf maxFrames = feedback mempty $ timeWarpStep sf maxFrames

-- mprint :: Monad m => [Char] -> m ()
-- mprint x = return $ trace x ()

timeWarpStep
  :: (Monad m, Ord msg, HasFrameAssociation msg)
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

      pi'      <- return $ addProcessed pi n (a, qi)
      return ((b, sf'), pi')
    Just t0
      | t0 < n -> do
        let pi' = processedAfterRollback pi n a qi  -- all processed inputs after rollback
            qi' = rollbackInputs pi' t0 n -- input when rolling back
        msf <- performRollback sf qi'
        return (msf, pi')
      | t0 == n -> do-- processInput sf pi n (0, (a, qi)) -- step
        (b, sf') <- unMSF sf (0, (a, toList qi))
        pi'      <- return $ addProcessed pi n (a, qi)
        return ((b, sf'), pi')
      | t0 > n -> error "t0 > n, inputs from the future can not be processed"
      | otherwise -> error "i dont even" -- on empty list?!
  return ((b, take maxFrames pi'), timeWarpStep sf' maxFrames)

-- Adds a single input (a, msg) for a frame to an existing buffer
addProcessed
  :: (Ord msg)
  => MessageBuffer (ProcessedInput a msg)
  -> FrameNr
  -> (a, MessageBuffer msg)
  -> MessageBuffer (ProcessedInput a msg)
addProcessed pi frame (a, ms) =
  pi <> singleton (ProcessedInput (frame, (a, toList ms)))

-- Adds a to pi, then merges qi into corresponding ProcessedInputs (inputs with matching FrameAssociation/ FrameNr)
processedAfterRollback
  :: (HasFrameAssociation msg, Ord msg)
  => MessageBuffer (ProcessedInput a msg)
  -> FrameNr
  -> a
  -> MessageBuffer msg
  -> (MessageBuffer (ProcessedInput a msg))
processedAfterRollback pi n a qi = do
  let pi' = pi <> singleton (ProcessedInput (n, (a, [])))
  mergeB pi' qi -- TODO too old messages are implicitly thrown away when using mergeB

-- Convertes sublist of ProcessedInput starting at t0 to unprocessed input.
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

-- Applies a MSF to a list of inputs. Returns the last returned value and continuation.
performRollback
  :: Monad m
  => MSF m (Natural, (a, [msg])) b
  -> [(Natural, (a, [msg]))]
  -> m (b, MSF m (Natural, (a, [msg])) b)
performRollback sf xs = do
  bs <- processInputs' sf xs
  return $ last bs

-- Applies a MSF to a list of inputs. Returns list of returned values and continuations.
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
senseFirst senseI = constM senseI >>> (arr $ \x -> ((0, x), Event x))

senseRest :: Monad m => (Bool -> m (c, Maybe c')) -> c' -> MSF m a (c, c')
senseRest sense a = constM (sense True) >>> (arr id *** keepLast a)

keepLast :: Monad m => a -> MSF m (Maybe a) a
keepLast a = MSF $ \ma -> let a' = fromMaybe a ma in return (a', keepLast a')

switch' :: Monad m => MSF m a (b, Event c) -> (c -> MSF m a b) -> MSF m a b
switch' sf' sfC = MSF.switch (sf' >>> second (arr eventToMaybe)) sfC

-- Consume/render
actuateSF :: Monad m => (Bool -> a -> m c) -> MSF m a c
actuateSF actuate = arr (\x -> (True, x)) >>> arrM (uncurry actuate)

sfIO :: Monad m => MSF (ReaderT r Identity) a b -> MSF m (r, a) b
sfIO sf = morphS (return . runIdentity) (runReaderS sf)

-- merge buffer qi into pi.
-- Every element with equal framenumbers in pi and qi will be merged.
-- Elements in qi, which do not have a corresponding element in pi are discarded.
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

-- TODO should be basically some kind of fold
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
