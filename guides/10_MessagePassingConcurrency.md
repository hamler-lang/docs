# Message Passing Concurrency

[ToC]

## About Actor Model

Professor Carl Hewitt published the famous paper [*Actor model of computation*](https://arxiv.org/vc/arxiv/papers/1008/1008.1459v8.pdf) in 1974. In the thesis, he elaborates that:

An Actor is a computational entity that, in response to a message it receives, can concurrently:

- send a finite number of messages to other Actors;
- create a finite number of new Actors;
- designate the behavior to be used for the next message it receives.

With the rise of multi-core computing and large-scale distributed systems, the Actor Model is becoming increasingly important because of its natural concurrent, parallel, and distributed.

## Process and Mailbox

An actor in Hamler/Erlang is defined as a process, which works like an OS process. Each process has its own memory, composed of a mailbox, a heap, a stack and a process control block(PCB) with information about the process.

![Process](https://www.hamler-lang.org/images/process@1x.png)

Processes in Erlang are very lightweight. We can create millions of processes on a running Erlang virtual machine.

A process is identified by a Pid. Other processes can send messages to a process via Pid.

## A Ping/Pong Example

```haskell
import Prelude
import Control.Process (selfPid)

go :: Process ()
go = do
  self <- selfPid
  pid <- spawn loop
  pid ! (self, :ping)
  receive
    :pong -> println "Pong!"
  pid ! :stop

loop :: Process ()
loop =
  receive
    (from, :ping) -> do
      println "Ping!"
      from ! :pong
      loop
    :stop -> return ()
```

## Spawn a new process

In Hamler, a new process is created via the `spawn` functions, which are defined in `Control.Process.Spawn` module.

```haskell
-- | Create a process
spawn :: forall a. IO a -> Process Pid

-- | Create and link a process
spawnLink :: forall a. IO a -> Process Pid

-- | Create and monitor a process
spawnMonitor :: forall a. IO a -> Process (Pid, Ref)
```

## Send/Receive message

```haskell
go :: Process ()
go = do
  pid <- spawn recv
  pid ! :msg

recv :: Process ()
recv = receive x -> printf "recv: %s" (showAny x)
```

## Selective Receive

```haskell
go :: Process ()
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
go :: Process ()
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

## Registered Processes

A process can be registered under a name, which has an Atom type.

```haskell
import Control.Process (register, whereis, unregister)

go :: Process ()
go = do
  pid <- spawn proc
  register :name pid
  res <- whereis :name
  unregister :name

proc :: Process ()
proc = receive _ -> return ()
```

## Linking

Two processes can be linked to each other.

```haskell
import Prelude
import Control.Process (killProc, trapExit)

go :: forall a. Process a
go = do
  trapExit true
  pid <- spawn (receive _ -> return ())
  link pid
  killProc pid
  receive msg -> return msg
```

## Monitoring

One process `Pid1` can monitor another process `Pid2`. If `Pid2` terminates,  `Pid1` will receive a 'DOWN' message from `Pid2`.

```haskell
import Prelude
import Control.Process (killProc)

go :: forall a. IO a
go = do
  pid <- spawn proc
  ref <- monitor pid
  killProc pid
  receive msg -> return msg

proc :: Process ()
proc = receive _ -> return ()
```

## Process Termination

```haskell
import Prelude
import Control.Process (isAlive, exitProc)

-- Exit the current process
exit :normal

go :: Process Boolean
go = do
  pid <- spawn (receive _ -> return ())
  exitProc pid :reason
  isAlive pid
```

