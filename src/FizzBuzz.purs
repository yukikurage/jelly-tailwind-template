module FizzBuzz where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Monad.Reader (class MonadAsk, Reader, ReaderT, ask, asks, runReader)
import Data.Array (range)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)

newtype FizzBuzzM a = FizzBuzzM (Int -> Maybe a)

instance Semigroup a => Semigroup (FizzBuzzM a) where
  append (FizzBuzzM f) (FizzBuzzM g) = FizzBuzzM (\n -> f n <> g n)

instance Semigroup a => Monoid (FizzBuzzM a) where
  mempty = FizzBuzzM (\_ -> Nothing)

instance Functor FizzBuzzM where
  map f (FizzBuzzM g) = FizzBuzzM (map f <<< g)

instance Apply FizzBuzzM where
  apply (FizzBuzzM f) (FizzBuzzM g) = FizzBuzzM (\n -> f n <*> g n)

instance Applicative FizzBuzzM where
  pure = FizzBuzzM <<< const <<< Just

instance Bind FizzBuzzM where
  bind (FizzBuzzM f) g = FizzBuzzM
    (\n -> f n >>= \a -> case g a of FizzBuzzM h -> h n)

instance Monad FizzBuzzM

instance MonadAsk Int FizzBuzzM where
  ask = FizzBuzzM Just

instance Alt FizzBuzzM where
  alt (FizzBuzzM f) (FizzBuzzM g) = FizzBuzzM (\n -> f n <|> g n)

runFizzBuzzM :: forall a. FizzBuzzM a -> Int -> Maybe a
runFizzBuzzM (FizzBuzzM f) = f

fizz :: Reader Int (Maybe String)
fizz = asks \n ->
  if n `mod` 3 == 0 then Just "Fizz"
  else Nothing

buzz :: Reader Int (Maybe String)
buzz = asks \n ->
  if n `mod` 5 == 0 then Just "Buzz"
  else Nothing

fallback :: Reader Int (Maybe String -> String)
fallback = do
  n <- ask
  pure $ fromMaybe (show n)

fizzbuzz :: Reader Int String
fizzbuzz = fallback <*> fizz <> buzz

test :: String
test = joinWith "\n" $ map (runReader fizzbuzz)
  $ range 1 30
