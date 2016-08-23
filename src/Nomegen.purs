-- Copyright 2016 Ian D. Bollinger
--
-- Licensed under the MIT license <LICENSE or
-- http://opensource.org/licenses/MIT>. This file may not be copied, modified,
-- or distributed except according to those terms.

module Nomegen
  ( NameGenerator
  , nameGenerator
  , generate
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array as Array
import Data.Char as Char
import Data.Foldable as Foldable
import Data.List as List
import Data.List ((:))
import Data.Set as Set
import Data.Set (Set)
import Data.String as String
import Nomegen.MarkovModel as MarkovModel
import Nomegen.MarkovModel (MarkovModel, markovModel)

newtype NameGenerator = NameGenerator (MarkovModel Char)

nameGenerator :: Int -> Number -> Set String -> NameGenerator
nameGenerator order prior names =
  NameGenerator (Foldable.foldr fold model names)
  where
  model = markovModel support order prior
  support = Foldable.foldMap (Set.fromFoldable <<< String.toCharArray) names
  fold = MarkovModel.observe 1.0 <<< List.fromFoldable <<< String.toCharArray

generate :: forall e. NameGenerator -> Eff (random :: RANDOM | e) String
generate (NameGenerator model) = do
  name <- MarkovModel.generate model
  pure case name of
    x : xs -> String.fromCharArray (Array.fromFoldable (Char.toUpper x : xs))
    List.Nil -> ""
