module Cases.Prelude
( 
  module Exports,

  bug,
  bottom,

  (?:),
  (|>),
  (<|),
  (|$>),
)
where

-- base
-------------------------
import BasePrelude as Exports

-- custom
-------------------------
import qualified Debug.Trace.LocationTH

bug = [e| $(Debug.Trace.LocationTH.failure) . (msg <>) |]
  where
    msg = "A \"cases\" package bug: " :: String

bottom = [e| $bug "Bottom evaluated" |]

(?:) :: Maybe a -> a -> a
maybeA ?: b = fromMaybe b maybeA
{-# INLINE (?:) #-}

(|>) :: a -> (a -> b) -> b
a |> aToB = aToB a
{-# INLINE (|>) #-}

(<|) :: (a -> b) -> a -> b
aToB <| a = aToB a
{-# INLINE (<|) #-}

-- | 
-- The following are all the same:
-- fmap f a == f <$> a == a |> fmap f == a |$> f
-- 
-- This operator accomodates the left-to-right operators: >>=, >>>, |>.
(|$>) = flip fmap
{-# INLINE (|$>) #-}
