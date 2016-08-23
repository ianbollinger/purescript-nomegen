-- Copyright 2016 Ian D. Bollinger
--
-- Licensed under the MIT license <LICENSE or
-- http://opensource.org/licenses/MIT>. This file may not be copied, modified,
-- or distributed except according to those terms.

module Nomegen.Categorical
  ( Categorical
  , categorical
  , observe
  , sample
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.Eff.Random (RANDOM, randomRange)
import Data.Foldable as Foldable
import Data.List as List
import Data.List ((:))
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Set (Set)
import Data.Tuple (Tuple(..))
import Nomegen.Crash (crash)

-- | A categorical distribution.
data Categorical a = Categorical
  { counts :: Map a Number
  , total :: Number
  }

-- | Create a categorical distribution with a given support and Dirichlet prior.
categorical :: forall a. Ord a => Set a -> Number -> Categorical a
categorical support prior
  | Set.size support == 0 = unsafeThrow "Must be given a non-empty set."
  | otherwise =
    Categorical { counts: counts, total: Foldable.sum (Map.values counts) }
    where
    counts = Map.fromFoldable (Set.map (\x -> Tuple x prior) support)

-- | Add an event to the given categorical distribution.
observe :: forall a. Ord a => Number -> a -> Categorical a -> Categorical a
observe count event (Categorical { counts, total }) =
  Categorical
    { counts: Map.update (\x -> Just (x + count)) event counts
    , total: total + count
    }

-- | Sample the given categorical distribution.
sample :: forall a e. Categorical a -> Eff (random :: RANDOM | e) a
sample (Categorical { counts, total }) = do
  sample' <- randomRange 0.0 total
  pure (go sample' (Map.toList counts))
  where
  go sample' = case _ of
    Tuple event count : xs
      | sample' <= count || List.null xs -> event
      | otherwise -> go (sample' - count) xs
    List.Nil -> crash "Encountered an empty categorical distribution."
