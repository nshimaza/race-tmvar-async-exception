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
