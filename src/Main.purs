module Main where

import Prelude

import Data.Array (catMaybes, fold, range)
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Jelly (Component, ch, el, launchApp, text)

main :: Effect Unit
main = launchApp root unit

root :: Component Unit
root = el "pre" $ ch $ text $ pure $ joinWith "\n" $ map fizzBuzz $ range 1 100

fizzBuzzRules :: Array (Int /\ String)
fizzBuzzRules =
  [ 3 /\ "Fizz"
  , 5 /\ "Buzz"
  ]

fizzBuzz :: Int -> String
fizzBuzz i =
  let
    matches = mapFlipped fizzBuzzRules \(divisor /\ word) ->
      if i `mod` divisor == 0 then Just word else Nothing
  in
    case catMaybes matches of
      [] -> show i
      words -> fold words
