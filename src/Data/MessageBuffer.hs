-- | Datatype for sorted list of messages, basically wraps 'SL.SortedList' for internal use in Time Warp synchronisation.

{-# LANGUAGE OverloadedLists, TypeFamilies #-}

module Data.MessageBuffer
    ( MessageBuffer(..)
    , toMessageBuffer
    , firstMessage
    , insertMessage
    , takeWhile
    , span
    , singleton
    , dropWhile
    , fromList
    , take
    , takeTail
    , size
    ) where

import Prelude hiding (takeWhile, take, span, dropWhile, tail)
import qualified Data.SortedList as SL
import Data.Semigroup
import Numeric.Natural
import GHC.Exts

-- | A sorted list of messages.
data MessageBuffer a = MessageBuffer (SL.SortedList a)
  deriving (Eq, Show)

instance Ord a => IsList (MessageBuffer a) where
  type (Item (MessageBuffer a)) = a
  fromList xs = MessageBuffer $ fromList xs
  toList (MessageBuffer xs) = toList xs

instance Ord a => Semigroup (MessageBuffer a) where
  (<>) (MessageBuffer xs) (MessageBuffer ys) = MessageBuffer $ xs <> ys

instance Ord a => Monoid (MessageBuffer a) where
  mempty = MessageBuffer $ fromList []
  mappend = (<>)

-- | List to 'MessageBuffer'.
toMessageBuffer :: Ord a => [a] -> MessageBuffer a
toMessageBuffer = MessageBuffer . SL.toSortedList

-- | Sorted insertion.
insertMessage :: Ord a => a -> MessageBuffer a -> MessageBuffer a
insertMessage x (MessageBuffer xs) = MessageBuffer $ SL.singleton x <> xs

-- | See 'SL.takeWhile'
takeWhile :: (a -> Bool) -> MessageBuffer a -> MessageBuffer a
takeWhile p (MessageBuffer xs) = MessageBuffer $ SL.takeWhile p xs

-- | See 'SL.span'
span :: (a -> Bool) -> MessageBuffer a -> (MessageBuffer a, MessageBuffer a)
span p (MessageBuffer xs) = let (xs',ys) = SL.span p xs in (MessageBuffer xs', MessageBuffer ys)

-- | Safely returns a buffer's head.
firstMessage :: MessageBuffer a -> Maybe a
firstMessage (MessageBuffer xs) = if null xs then Nothing else Just . Prelude.head $ SL.fromSortedList xs

-- | Singleton buffer.
singleton :: a -> MessageBuffer a
singleton = MessageBuffer . SL.singleton

-- | See 'SL.dropWhile'
dropWhile :: (a -> Bool) -> MessageBuffer a -> MessageBuffer a
dropWhile p (MessageBuffer xs) = MessageBuffer $ SL.dropWhile p xs

-- | See 'SL.take'
take :: Natural -> MessageBuffer a -> MessageBuffer a
take n (MessageBuffer xs) = MessageBuffer $ SL.take (fromIntegral n) xs

-- | Size of the buffer.
size :: Ord a => MessageBuffer a -> Int
size (MessageBuffer xs) = length $ toList xs

-- | Returns the last 'n' elements.
takeTail ::  Natural -> MessageBuffer a -> MessageBuffer a
takeTail n (MessageBuffer xs) = MessageBuffer $ (SL.reverseDown . SL.take (fromIntegral n) . SL.reverse) xs


