# Message Passing Concurrency

[ToC]

## About Actor Model

Professor Carl Hewitt published the famous pager *Actor model of computation* in 1974. In the thesis, he elaborates that:

An Actor is a computational entity that, in response to a message it receives, can concurrently:
  - send a finite number of messages to other Actors;
  - create a finite number of new Actors;
  - designate the behavior to be used for the next message it receives.

See: [Actor Model of Computation](https://arxiv.org/vc/arxiv/papers/1008/1008.1459v8.pdf)

With the rise of multi-core computing and large-scale distributed systems, the Actor Model is becoming increasingly important because of its natural concurrent, parallel, and distributed.

## Process and Mailbox

An actor in Hamler/Erlang is defined as a process, which works like an OS process. Each process has its own memory, composed of a mailbox, a heap, a stack and a process control block(PCB) with information about the process.

![Process](https://www.hamler-lang.org/images/process.png)

Processes in Erlang are very lightweight. We can create millions of processes on a running Erlang virtual machine.

A process is identified by a Pid. Other processes can send messages to a process via Pid.

## A Ping/Pong Example

```haskell
import Prelude
import Control.Process (selfPid)

go :: IO ()
go = do
  self <- selfPid
  pid <- spawn loop
  pid ! (self, :ping)
  receive
    :pong -> println "Pong!"
  pid ! :stop

loop :: IO ()
loop =
  receive
    (from, :ping) -> do
      println "Ping!"
      from ! :pong
      loop
    :stop -> exit
```

## Spawn a new process

In hamler, a new process is created via the `spawn` functions, which are defined in `Control.Process.Spawn` module.

```haksell
-- | Create a process
spawn :: forall a. IO a -> Process Pid

-- | Create and link a process
spawnLink :: forall a. IO a -> Process Pid

-- | Create and monitor a process
spawnMonitor :: forall a. IO a -> Process (Pid, Ref)
```

## Send/Receive message

```haskell
go :: IO ()
go = do
  pid <- spawn recv
  pid ! :msg

recv :: IO ()
recv = receive x -> printf "recv: %s" (showAny x)
```

## Selective Receive

```haskell
go :: IO ()
go = do
  pid <- spawn selectiveRecv
  pid ! :bar
  pid ! :foo

selectiveRecv :: Process ()
selectiveRecv = do
  receive :foo -> println "foo"
  receive :bar -> println "bar"
```

## Receive .. after

```haskell
go :: IO ()
go = do
  pid <- spawn recvAfter
  pid ! :foo

recvAfter :: Process ()
recvAfter =
  receive
    :bar -> println "recv bar"
  after
    1000 -> println "timeout"
```

## Registeration

```haskell
import Control.Process (register, whereis, unregister)

go :: IO ()
go = do
  pid <- spawn proc
  register :server pid
  res <- whereis :server
  unregister :server

proc :: Process ()
proc = receive _ -> return ()
```

## Linking

```haskell
import Control.Process (killProc, trapExit)

go :: forall a. Process a
go = do
  trapExit true
  pid <- spawn proc
  link pid
  killProc pid
  recv

recv :: forall a. Process a
recv = receive msg -> return msg

proc :: Process ()
proc = receive _ -> return ()
```

## Monitoring

```haskell
import Control.Process (killProc, trapExit)

import Control.Process (killProc)

go :: forall a. IO a
go = do
  pid <- spawn proc
  ref <- monitor pid
  killProc pid
  recv

recv :: forall a. Process a
recv = receive msg -> return msg

proc :: Process ()
proc = receive _ -> return ()
```

## Process Termination

```haskell
import Control.Process

-- Exit the current process
exit

do
  pid <- spawn proc
  exitProcWith pid :reason

proc :: Process ()
proc = receive _ -> return ()
```

