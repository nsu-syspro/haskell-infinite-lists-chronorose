{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task2 where

import Prelude hiding (filter)

-- | Infinite stream of elements
data Stream a = Stream a (Stream a)

instance (Show a) => Show (Stream a) where
  show stream =
    let go :: Int -> Stream a -> String
        go i (Stream a s)
          | i <= 0 = ""
          | i == 1 = show a
          | otherwise = show a <> ", " <> go (i - 1) s
     in go 10 stream

instance Foldable Stream where
  foldMap f (Stream a s) = f a <> foldMap f s

-- | Converts given list into stream
--
-- If the list is finite then it is continued
-- with given value repeated infinitely
--
-- Usage example:
--
-- >>> fromList 0 [1,2,3]
-- [1,2,3,0,0,0,0,0,0,0]
-- >>> fromList undefined [1..]
-- [1,2,3,4,5,6,7,8,9,10]
fromList :: a -> [a] -> Stream a
fromList v [] = let stream = Stream v stream in stream
fromList v (x : xs) = Stream x $ fromList v xs

-- | Builds stream from given seed value by applying given step function
--
-- Step function produces a pair of the next element in stream and updated seed value.
--
-- Usage example:
--
-- >>> unfold (\x -> (x, x-1)) 5
-- [5,4,3,2,1,0,-1,-2,-3,-4]
-- >>> unfold (\x -> (abs x, x-1)) 5
-- [5,4,3,2,1,0,1,2,3,4]
unfold :: (b -> (a, b)) -> b -> Stream a
unfold f b =
  let (a, b2) = f b
   in Stream a (unfold f b2)

-- | Returns infinite stream of natural numbers (excluding zero)
--
-- First 10 natural numbers:
--
-- >>> nats
-- [1,2,3,4,5,6,7,8,9,10]
nats :: Stream Integer
nats = unfold (\x -> (x, x + 1)) 1

-- | Returns infinite stream of fibonacci numbers (starting with zero)
--
-- First 10 fibonacci numbers:
--
-- >>> fibs
-- [0,1,1,2,3,5,8,13,21,34]
fibs :: Stream Integer
fibs = unfold (\(x, y) -> (x, (y, x + y))) (0, 1)

-- | Returns infinite stream of prime numbers
--
-- First 10 prime numbers:
--
-- >>> primes
-- [2,3,5,7,11,13,17,19,23,29]
primes :: Stream Integer
primes = unfold sieve (fromList 0 [2 ..])

-- | One step of Sieve of Eratosthenes
-- (to be used with 'unfoldr')
--
-- Returns next prime number from given stream
-- and strikes out all multiples of this prime
-- from the rest of the stream
--
-- Usage example:
--
-- >>> sieve $ fromList 0 [2..]
-- (2,[3,5,7,9,11,13,15,17,19,21])
-- >>> sieve $ snd $ sieve $ fromList 0 [2..]
-- (3,[5,7,11,13,17,19,23,25,29,31])
sieve :: Stream Integer -> (Integer, Stream Integer)
sieve (Stream a s) = (a, filter (\x -> x `mod` a /= 0) s)

filter :: (a -> Bool) -> Stream a -> Stream a
filter p (Stream a s) =
  if p a
    then Stream a (filter p s)
    else filter p s
