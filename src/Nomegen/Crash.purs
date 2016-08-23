-- Copyright 2016 Ian D. Bollinger
--
-- Licensed under the MIT license <LICENSE or
-- http://opensource.org/licenses/MIT>. This file may not be copied, modified,
-- or distributed except according to those terms.

module Nomegen.Crash where

import Prelude
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

crash :: forall a. String -> a
crash message =
  unsafeThrow
    ("An internal error has occurred: "
    <> message
    <> "\nPlease report to "
    <> "https://github.com/ianbollinger/purescript-nomegen/issues/")
