module FizzBuzz where

import Prelude

import Control.Monad.Reader (Reader, asks, runReader)
import Data.Array (range)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)

fizz :: Reader Int (Maybe String)
fizz = asks \n ->
  if n `mod` 3 == 0 then Just "Fizz"
  else Nothing

buzz :: Reader Int (Maybe String)
buzz = asks \n ->
  if n `mod` 5 == 0 then Just "Buzz"
  else Nothing

fallback :: Reader Int (Maybe String -> String)
fallback = asks $ show >>> fromMaybe

fizzbuzz :: Reader Int String
fizzbuzz = fallback <*> fizz <> buzz

test :: String
test = joinWith "\n" $ map (runReader fizzbuzz)
  $ range 1 30
