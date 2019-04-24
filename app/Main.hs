module Main where

import Stream
import Control.Monad.Par

-- Calculate the n-th Fibonacci number
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Use streams to calculate the sum of even fibonacci numbers.
pipeline :: Int -> [Integer] -> Integer
pipeline n xs =
  runPar $ do
    s0 <- streamFromList n xs              -- emit list in batches of size `n`
    s1 <- streamFilter even (n `div` 2) s0 -- emit even numbers + halve batch size
    s2 <- streamMap fib n s1               -- calc fib number
    sm <- streamFold (+) 0 s2              -- streaming sum op
    return sm                              -- result

-- Calculate and print the sum of some even Fibonacci numbers.
main :: IO ()
main = do
  putStrLn $ show (pipeline 10 [1..36])
