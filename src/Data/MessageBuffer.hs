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

-- A sorted list of messages.
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

toMessageBuffer :: Ord a => [a] -> MessageBuffer a
toMessageBuffer = MessageBuffer . SL.toSortedList

insertMessage :: Ord a => a -> MessageBuffer a -> MessageBuffer a
insertMessage x (MessageBuffer xs) = MessageBuffer $ SL.singleton x <> xs

takeWhile :: (a -> Bool) -> MessageBuffer a -> MessageBuffer a
takeWhile p (MessageBuffer xs) = MessageBuffer $ SL.takeWhile p xs

span :: (a -> Bool) -> MessageBuffer a -> (MessageBuffer a, MessageBuffer a)
span p (MessageBuffer xs) = let (xs',ys) = SL.span p xs in (MessageBuffer xs', MessageBuffer ys)

firstMessage :: MessageBuffer a -> Maybe a
firstMessage (MessageBuffer xs) = if null xs then Nothing else Just . Prelude.head $ SL.fromSortedList xs

singleton :: a -> MessageBuffer a
singleton = MessageBuffer . SL.singleton

dropWhile :: (a -> Bool) -> MessageBuffer a -> MessageBuffer a
dropWhile p (MessageBuffer xs) = MessageBuffer $ SL.dropWhile p xs

take :: Natural -> MessageBuffer a -> MessageBuffer a
take n (MessageBuffer xs) = MessageBuffer $ SL.take (fromIntegral n) xs

-- drops the head, returns the rest.
-- tail :: MessageBuffer a -> MessageBuffer a
-- tail = undefined
-- Return the suffix remaining after dropping the longest prefix of elements that satisfy the given condition.

size :: Ord a => MessageBuffer a -> Int
size (MessageBuffer xs) = length $ toList xs

takeTail ::  Natural -> MessageBuffer a -> MessageBuffer a
takeTail n (MessageBuffer xs) = MessageBuffer $ (SL.reverseDown . SL.take (fromIntegral n) . SL.reverse) xs


