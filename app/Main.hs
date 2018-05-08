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
    * CPU won't be 100% if you gave x smaller than available hardware threads
      of your platform.
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

import           Control.Concurrent           (forkIO, killThread, threadDelay, forkIOWithUnmask)
import           Control.Concurrent.STM.TMVar (newEmptyTMVarIO, putTMVar,
                                               takeTMVar)
import           Control.Exception            (AsyncException (..), catch, mask_)
import           Control.Monad                (forM, forM_, replicateM, void)
import           Control.Monad.STM            (atomically)
import           System.Environment (getArgs)
import           System.IO                    (hFlush, stdout)

data ExitReason = Completed | ByException | Killed deriving (Eq)
putStrFlush s = putStr s *> hFlush stdout
putCharFlush c = putChar c *> hFlush stdout

main :: IO ()
main = do
    args <- getArgs
    case args of
        (x:_)   | x == "v"  -> reproVerbose
                | otherwise -> reproSimple
        _                   -> reproSimple


reproSimple = do
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
        forkIO $ forM_ threads $ \(trigger, _) -> threadDelay 1 *> atomically (putTMVar trigger ())

        -- Concurrently kill threads in order to create race.
        -- TMVar operation and asynchronous exception can hit same thread simultaneously.
        -- Adjust threadDelay if you don't reproduce very well.
        threadDelay 1000
        forM_ threads $ \(_, tid) -> do
            putCharFlush 'A'
            killThread tid      -- When the issue reproduced, this killThread doesn't return.
            putCharFlush '\b'


reproVerbose = do
    let volume = 1000
    forM_ [1..1000] $ \i -> do
        putStrFlush $ show i ++ " "

        -- Spawn threads.
        threads <- replicateM volume $ do
            trigger <- newEmptyTMVarIO
            monitor <- newEmptyTMVarIO
            tid <- mask_ $ forkIOWithUnmask $ \unmask ->
                (do
                    unmask (void $ atomically $ takeTMVar trigger)
                    atomically $ putTMVar monitor Completed
                )
                `catch`
                (\e -> case e of
                    ThreadKilled -> atomically $ putTMVar monitor Killed
                    _            -> atomically $ putTMVar monitor ByException
                )
            pure (trigger, tid, monitor)

        -- Make sure all threads are spawned.
        threadDelay 100000
        -- Let threads start to exit normally.
        forkIO $ forM_ threads $ \(trigger, _, _) -> threadDelay 1 *> (atomically $ putTMVar trigger ())
        -- Concurrently kill threads in order to create race.
        -- TMVar operation and asynchronous exception can hit same thread simultaneously.
        -- Adjust threadDelay if you don't reproduce very well.
        threadDelay 1000
        forM_ threads $ \(_, tid, _) -> do
            putCharFlush 'A'
            killThread tid      -- When the issue reproduced, this killThread doesn't return.
            putCharFlush '\b'

        -- Validate and show if the above exection was actually racy.
        reports <- forM threads $ \(_, _, monitor) -> atomically $ takeTMVar monitor
        validateReport volume reports

validateReport :: Int -> [ExitReason] -> IO ()
validateReport volume reports = do
    if length reports /= volume
    then putStrLn $ "Expect: length reports == " ++ show volume ++ "  Got: " ++ show (length reports)
    else pure ()
    let normalCount = length . filter ((==) Completed) $ reports
        killedCount = length . filter ((==) Killed) $ reports
    if normalCount == 0
    then putStrLn "Expect: normalCount non-0  Got: 0"
    else pure ()
    if killedCount == 0
    then putStrLn "Expect: killedCount non-0  Got: 0"
    else pure ()
    if normalCount + killedCount /= volume
    then putStrLn $ "Expect: normalCount + killedCount == " ++ show volume ++ "  Got: " ++ show (normalCount + killedCount)
    else pure ()
    putStr $ "(" ++ show killedCount ++ ") "
    hFlush stdout
