-- Copyright 2016 Ian D. Bollinger
--
-- Licensed under the MIT license <LICENSE or
-- http://opensource.org/licenses/MIT>. This file may not be copied, modified,
-- or distributed except according to those terms.

module Nomegen.MarkovModel
  ( MarkovModel
  , MarkovElement
  , markovModel
  , observe
  , sample
  , generate
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Data.Foldable as Foldable
import Data.List as List
import Data.List (List, (:), (..))
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe, fromMaybe')
import Data.Set as Set
import Data.Set (Set)
import Data.Unfoldable as Unfoldable
import Nomegen.Categorical as Categorical
import Nomegen.Categorical (Categorical)
import Nomegen.Crash (crash)

data MarkovModel a = MarkovModel
  { support :: Set (MarkovElement a)
  , order :: Int
  , prior :: Number
  , prefix :: List (MarkovElement a)
  , counts :: Map (List (MarkovElement a)) (Categorical (MarkovElement a))
  }

data MarkovElement a
  = Body a
  | Boundary

derive instance eqMarkovElement :: Eq a => Eq (MarkovElement a)
derive instance ordMarkovElement :: Ord a => Ord (MarkovElement a)

markovModel :: forall a. Ord a => Set a -> Int -> Number -> MarkovModel a
markovModel support order prior = MarkovModel
  { support: Set.insert Boundary (Set.map Body support)
  , order: order
  , prior: prior
  , prefix: Unfoldable.replicate order Boundary
  , counts: Map.empty
  }

observe :: forall a. Ord a => Number -> List a -> MarkovModel a -> MarkovModel a
observe count sequence model@(MarkovModel model') =
  Foldable.foldr
    outerFold
    model
    (0 .. (List.length sequence' - model'.order - 1))
  where
  sequence' =
    model'.prefix <> map Body sequence <> Unfoldable.singleton Boundary
  outerFold i model@(MarkovModel model') =
    Foldable.foldr innerFold model (tails context)
    where
    context = List.take model'.order (List.drop i sequence')
    event =
      fromMaybe'
        (\_ -> crash "Could not index list.")
        (List.index sequence' (i + model'.order))
    innerFold prefix model@(MarkovModel model') =
      MarkovModel
        (model' { counts = Map.insert prefix categorical' model'.counts })
      where
      categorical' =
        Categorical.observe count event (categorical prefix model)
  tails = go List.Nil
    where
    go xs = case _ of
      List.Nil -> xs
      ys@(_ : ys') -> go (ys : xs) ys'

generate
  :: forall a e
   . Ord a
  => MarkovModel a
  -> Eff (random :: RANDOM | e) (List a)
generate model@(MarkovModel model') = do
  first <- sample model'.prefix model
  result <- go (Unfoldable.singleton first)
  pure (map unwrap result)
  where
  go x = do
    y <- sample x model
    case y of
      Boundary -> pure x
      _ -> go (x `List.snoc` y)
  unwrap = case _ of
    Body x -> x
    Boundary -> crash "Encountered a malformed sample."

sample
  :: forall a e
   . Ord a
  => List (MarkovElement a)
  -> MarkovModel a
  -> Eff (random :: RANDOM | e) (MarkovElement a)
sample context model@(MarkovModel model') =
  Categorical.sample (categorical (backOff context') model)
  where
  backOff = case _ of
    xs@(_ : xs') | not (xs `Map.member` model'.counts) -> backOff xs'
    x -> x
  context'
    | contextLength > model'.order =
      List.drop (contextLength - model'.order) context
    | contextLength < model'.order =
      Unfoldable.replicate (model'.order - contextLength) Boundary <> context
    | otherwise = context
  contextLength = List.length context

categorical
  :: forall a
   . Ord a
  => List (MarkovElement a)
  -> MarkovModel a
  -> Categorical (MarkovElement a)
categorical context (MarkovModel model) =
  fromMaybe
    (Categorical.categorical model.support model.prior)
    (Map.lookup context model.counts)
