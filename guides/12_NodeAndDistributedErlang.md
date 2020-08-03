# Node and Distributed Erlang

[ToC]

## Distributed Erlang/OTP

Hamler is compiled to Erlang/OTP, which is a concurrent, fault-tolerant, distributed programming platform. A distributed Erlang/OTP system consists of a number of Erlang runtime systems called `node`. Nodes connect to each other with TCP/IP sockets and communicate by Message Passing.

![DistributedNodes](https://www.hamler-lang.org/images/distributed-nodes.png)

## Connect Nodes

An Erlang runtime system called `node` is identified by a unique name like email address. Erlang nodes communicate with each other by the name.

Start Erlang `epmd` for registering node name first:

```shell
epmd -daemon
```

Start `n1@127.0.0.1` on a Hamler REPL console:

```shell
hamler repl
> import Control.Distributed.Node
> import Control.Distributed.NetKernel
> start :"n1@127.0.0.1"
```

Start `n2@127.0.0.1` on another Hamler REPL console, then connect to the `n1@127.0.0.1` node:

```shell
hamler repl
> import Control.Distributed.Node
> import Control.Distributed.NetKernel
> start :"n2@127.0.0.1"
> connectNode :"n1@127.0.0.1"
true
> nodes
['n1@127.0.0.1']
```

## RPC

-- TODO: ...

