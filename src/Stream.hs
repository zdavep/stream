{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Stream where

import Control.DeepSeq
import Control.Monad.Par

data IList a
  = Nil
  | Cons a (IVar (IList a))
  | Fork (Par ()) (IList a)

type Stream a = IVar (IList a)

instance NFData a => NFData (IList a) where
  rnf Nil = ()
  rnf (Cons a b) = rnf a `seq` rnf b
  rnf (Fork op a) = seq op $ rnf a

-- Convert a list into a batched stream.
streamFromList :: NFData a => Int -> [a] -> Par (Stream a)
streamFromList n xs = do
  os <- new
  fork $ loop n xs os
  return os
  where
    loop _ [] o = put o Nil
    loop 0 (h:t) o = do
      v <- new
      let op = loop n t v
      put o (Fork op (Cons h v))
    loop i (h:t) o = do
      v <- new
      put o (Cons h v)
      loop (i - 1) t v

-- Apply a fold operation to a stream yielding a single result.
streamFold :: (a -> b -> a) -> a -> Stream b -> Par a
streamFold fn !acc is = do
  il <- get is
  case il of
    Nil -> return acc
    Cons h t -> streamFold fn (fn acc h) t
    Fork op (Cons h t) -> fork op >> streamFold fn (fn acc h) t

-- Apply a map function to a stream producing another stream.
streamMap :: NFData b => (a -> b) -> Stream a -> Par (Stream b)
streamMap fn is = do
  os <- new
  fork $ loop is os
  return os
  where
    loop i o = do
      il <- get i
      case il of
        Nil -> put o Nil
        Cons h t -> proc h t o
        Fork op (Cons h t) -> fork op >> proc h t o
    proc h t o = do
      v <- new
      put o (Cons (fn h) v)
      loop t v

-- Filter a stream using a predicate function producing another stream.
streamFilter :: NFData a => (a -> Bool) -> Stream a -> Par (Stream a)
streamFilter p is = do
  os <- new
  fork $ loop is os
  return os
  where
    loop i o = do
      il <- get i
      case il of
        Nil -> put o Nil
        Cons h t -> proc h t o
        Fork op (Cons h t) -> fork op >> proc h t o
    proc h t o
      | p h = do
        v <- new
        put_ o (Cons h v)
        loop t v
      | otherwise = loop t o

-- Rate limit a stream
streamLimiter :: NFData a => Int -> Stream a -> Par (Stream a)
streamLimiter n is = do
  os <- new
  fork $ loop n is os
  return os
  where
    loop x i o = do
      il <- get i
      case il of
        Nil -> put o Nil
        Cons h t -> proc x h t o
        Fork op (Cons h t) -> fork op >> proc x h t o
    proc x h t o
      | x > 0 = putCons x h t o
      | otherwise = putFork h t o
    putCons x h t o = do
      v <- new
      put_ o (Cons h v)
      loop (x - 1) t v
    putFork h t o = do
      v <- new
      let op = loop n t v
      put_ o (Fork op (Cons h v))
