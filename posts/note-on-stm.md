------------
title: Notes on Failing to Understand Software Transactional Memory
author: Arnaud Bailly 
date: 2016-05-22
------------

I am writing some library to easily implement event sourced services in Haskell based on previous experience at Capital Match, and while doing so I rewrote a simple file-based event store. This store communicates with core service using [TBQueue](https://hackage.haskell.org/package/stm-2.4.4.1/docs/Control-Concurrent-STM-TBQueue.html), a bounded queue implemented over Haskell's STM. It took me couple of hours on Friday to solve a [BlockedIndefinitelyOnSTM](http://hackage.haskell.org/package/base-4.8.2.0/docs/Control-Exception-Base.html#t:BlockedIndefinitelyOnSTM) bug I was facing while testing this simple store. So today I posted a [question about STM](http://stackoverflow.com/questions/37376419/what-is-the-precise-reason-i-got-blocked-on-stm) on Stack Overflow, as I did not have a clear intuition on why my code was failing, hence why my fix was correct.

The code of the store boils down to the following simple model.

First some useful imports...

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad            (forever)
import           Hevents.Eff
import           System.IO
```

The store dequeues some *operation* from a given queue, writes the operation's string to `stdout` then put back the length of the written string into a [TMVar](http://hackage.haskell.org/package/stm-2.4.4.1/docs/Control-Concurrent-STM-TMVar.html), which models communicating the result of the operation back to the caller. 

```haskell
data Op = Op String (TMVar Int)

storerun :: TBQueue Op -> IO ()
storerun q = do
  h <- openFile "store.test" ReadWriteMode
  hSetBuffering h NoBuffering
  forever $ do
    Op s v <- atomically $ readTBQueue q
    hPutStrLn h s
    atomically $ putTMVar v (length s)
```

The `main` function is responsible for creating the jobs queue, starting the "store" in a separate thread then reading lines from `stdin` and feeding them as "operations" for the store. 

```haskell
main :: IO ()
main = do
  q <- newTBQueueIO 100
  _ <- async $ storerun q
  storeInput q
  where
    storeInput q = forever $ do
      l <- getLine
      v <- newEmptyTMVarIO
      r <- atomically $ do
        writeTBQueue q (Op l v)
        takeTMVar v
```

This code deadlocks because STM are actually - surprise! - **transactions**: They do all of their operations, or nothing, and they are serialized. Hence the following block:

```haskell
r <- atomically $ do
  writeTBQueue q (Op l v)
  takeTMVar v
```

...can succeeds *if and only if* it can **atomically** put an operation in the queue and read the result back from `v`. Which of course is not possible because the result is put back after the operation is read from the queue in another transaction. The correct code is:

```haskell
atomically $ writeTBQueue q (Op l v)
r <- atomically $ takeTMVar v
```

Pretty obvious, in retrospect. As the person who answered my question on SO, there is no way for two STM transactions to *exchange* information.
