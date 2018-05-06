{-
# race-tmvar-async-exception

This code demonstrates potential bug in GHC runtime where runtime stalls at
killThread and falls into high CPU.

## Summary of the issue

When you successfully reproduced it, you will see

* Reproduces with +RTS -Nx where x > 1
* Does NOT reproduce with +RTS -N1
* Program stalls after 'A' printed
* High CPU based on given -Nx
    * If you have 4 core machine with hyper thread enabled and you give -N8,
      CPU utilization will be 100%
* Does NOT reproduce if TMVar is replaced by MVar

This program intentionally creates race condition between TMVar operation and
asynchronous exception.  In one side, putTMVar from external thread attempts to
unblock a thread blocked by takeTMVar.  On the other side, asynchronous
exception ThreadKilled thrown by external thread attempts to interrupt the same
blocked thread.

I guess when those operation happen at in parallel in SMP environment, GHC
runtime falls into high CPU.
-}


module Main where

import           Control.Concurrent           (forkIO, killThread, threadDelay)
import           Control.Concurrent.STM.TMVar (newEmptyTMVarIO, putTMVar,
                                               takeTMVar)
import           Control.Monad                (forM_, replicateM, void)
import           Control.Monad.STM            (atomically)
import           System.IO                    (hFlush, stdout)

putStrFlush s = putStr s *> hFlush stdout
putCharFlush c = putChar c *> hFlush stdout

main :: IO ()
main = do
    let volume = 1000
    forM_ [1..1000] $ \i -> do
        putStrFlush $ show i ++ " "

        -- Spawn massive number of threads.
        threads <- replicateM volume $ do
            trigger <- newEmptyTMVarIO
            tid <- forkIO $ void $ atomically $ takeTMVar trigger
            pure (trigger, tid)

        -- Make sure all threads are spawned.
        threadDelay 30000

        -- Let threads start to exit normally.
        forkIO $ forM_  threads $ \(trigger, _) -> threadDelay 1 *> (atomically $ putTMVar trigger ())

        -- Concurrently kill threads in order to create race.
        -- TMVar operation and asynchronous exception can hit same thread simultaneously.
        -- Adjust threadDelay if you don't reproduce very well.
        threadDelay 1000
        forM_ threads $ \(_, tid) -> do
            putCharFlush 'A'
            killThread tid      -- When the issue reproduced, this killThread doesn't return.
            putCharFlush '\b'
