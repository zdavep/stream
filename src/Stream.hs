{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Stream
  ( streamFromList,
    streamFold,
    streamMap,
    streamFilter,
  )
where

import           Control.DeepSeq
import           Control.Monad.Par

-- IList is a parallel data structure.
data IList a = Nil
    | Cons a (IVar (IList a))
    | Fork (Par ()) (IList a)

-- Stream is an IList that can be read (blocking) and written in parallel.
type Stream a = IVar (IList a)

-- Describe how to fully evaluate an IList
instance NFData a => NFData (IList a) where
  rnf Nil         = ()
  rnf (Cons a b)  = rnf a `seq` rnf b
  rnf (Fork op a) = seq op $ rnf a

-- Convert a list into a batched stream.
streamFromList :: NFData a => Int -> [a] -> Par (Stream a)
streamFromList n xs = do
  os <- new
  fork $ loop n xs os
  return os
  where
    loop _ [] o = put o Nil
    loop 0 (h : t) o = do
      v <- new
      let op = loop n t v
      put o (Fork op (Cons h v))
    loop i (h : t) o = do
      v <- new
      put o (Cons h v)
      loop (i - 1) t v

-- Apply a fold operation to a stream yielding a single result.
streamFold :: (a -> b -> a) -> a -> Stream b -> Par a
streamFold fn !acc is = do
  il <- get is
  case il of
    Nil                -> return acc
    Cons h t           -> streamFold fn (fn acc h) t
    Fork op (Cons h t) -> fork op >> streamFold fn (fn acc h) t

-- Apply a map function to a stream producing another stream.
streamMap :: NFData b => (a -> b) -> Int -> Stream a -> Par (Stream b)
streamMap fn n is = do
  os <- new
  fork $ loop n is os
  return os
  where
    loop x i o = do
      il <- get i
      case il of
        Nil                -> put o Nil
        Cons h t           -> proc x h t o
        Fork op (Cons h t) -> fork op >> proc x h t o
    proc x h t o
      | x > 0 = do
        v <- new
        put o (Cons (fn h) v)
        loop (x - 1) t v
      | otherwise = do
        v <- new
        let op = loop n t v
        put o (Fork op (Cons (fn h) v))

-- Filter a stream using a predicate function producing another stream.
streamFilter :: NFData a => (a -> Bool) -> Int -> Stream a -> Par (Stream a)
streamFilter p n is = do
  os <- new
  fork $ loop n is os
  return os
  where
    loop x i o = do
      il <- get i
      case il of
        Nil                -> put o Nil
        Cons h t           -> proc x h t o
        Fork op (Cons h t) -> fork op >> proc x h t o
    proc x h t o
      | x > 0 && p h = do
        v <- new
        put_ o (Cons h v)
        loop (x - 1) t v
      | x <= 0 && p h = do
        v <- new
        let op = loop n t v
        put_ o (Fork op (Cons h v))
      | otherwise =
        loop (x - 1) t o
