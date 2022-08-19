module Main where

import Prelude

import Control.Monad.Reader (runReader)
import Data.Array (range)
import Data.String (joinWith)
import Effect (Effect)
import FizzBuzz (fizzbuzz)
import Jelly (Component, ch, el, launchApp, text)

main :: Effect Unit
main = launchApp root unit

root :: Component Unit
root = el "pre" do
  ch $ text $ pure $ joinWith "\n" $ map (runReader fizzbuzz) $ range 1 100
