module Main where

import Stream
import Control.Monad.Par

-- Calculate the n-th Fibonacci number
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Use streams to calculate the sum of even fibonacci numbers.
pipeline :: [Integer] -> Integer
pipeline xs =
  runPar $ do
    s0 <- streamFromList 10 xs   -- emit in batches with max size of of 10
    s1 <- streamFilter even s0   -- only stream even numbers
    s2 <- streamLimiter 2 s1     -- limit batch size to 2 for mapping
    s3 <- streamMap fib s2       -- calc fib number
    sm <- streamFold (+) 0 s3    -- streaming sum op
    return sm                    -- result

-- Calculate and print the sum of some even Fibonacci numbers.
main :: IO ()
main = do
  putStrLn $ show (pipeline [1..36])

