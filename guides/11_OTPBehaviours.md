# OTP Behaviours

[ToC]

## Overview

Hamler implements OTP Behaviours based on Type classes.

## GenServer Behaviour

### Client-Server Model

See: [Erlang gen_server Behaviour](https://erlang.org/doc/design_principles/gen_server_concepts.html)

![Client-server model](https://erlang.org/doc/design_principles/clientserver.gif)

### GenServer Typeclass

```haskell
class GenServer req rep st | req -> rep, rep -> st, st -> req where
  handleCall :: HandleCall req rep st
  handleCast :: HandleCast req rep st
```

### Server Example

```haskell
module Demo.Server
  ( start
  , inc
  , dec
  , query
  ) where

import Prelude
import Control.Behaviour.GenServer
  ( class GenServer
  , HandleCall
  , HandleCast
  , Init
  , startLinkWith
  , initOk
  , call
  , cast
  , noReply
  , reply
  , shutdown
  )
import System.IO (println)

data Request = Inc | Dec | Query
data Reply = QueryResult Integer
data State = State Integer

name :: Atom
name = :server

start :: Process Pid
start = startLinkWith name (init 20)

-----------------------------------------------------------------------------
-- | Server API
-----------------------------------------------------------------------------

inc :: Process ()
inc = cast name Inc

dec :: Process ()
dec = cast name Dec

query :: Process Integer
query = do
  QueryResult i <- call name Query
  return i

-----------------------------------------------------------------------------
-- | Server callbacks
-----------------------------------------------------------------------------

instance GenServer Request Reply State where
  handleCall = handleCall
  handleCast = handleCast

init :: Integer -> Init Request State
init n = initOk (State n)

handleCall :: HandleCall Request Reply State
handleCall Query _from (State i) = do
  println "Call: Query"
  reply (QueryResult i) (State i)
handleCall _req _from st =
  shutdown :badRequest st

handleCast :: HandleCast Request Reply State
handleCast Inc (State n) = do
  println "Cast: Inc"
  noReply $ State (n+1)
handleCast Dec (State n) = do
  println "Cast: Dec"
  noReply $ State (n-1)
handleCast _ st = noReply st
```

### Start a Server process

```haskell
-- | Start a standalone Server process.
start :: forall req rep st. GenServer req rep st => (Init req st) -> Process Pid
startWith :: forall req rep st. GenServer req rep st => Name -> (Init req st) -> Process Pid

-- | Start a Server process as part of a supervision tree.
startLink :: forall req rep st. GenServer req rep st => (Init req st) -> Process Pid
startLinkWith :: forall req rep st. GenServer req rep st => Name -> (Init req st) -> Process Pid
```

### Init callback

```haskell
-- | Init Result
data InitResult req st
  = InitOk st (Maybe (Action req))
    -- ^ {ok, State}
  | InitIgnore
    -- ^ ignore
  | InitStop ExitReason
    -- ^ {stop, Reason}

-- | Init callback
type Init req st = Process (InitResult req st)
```

### HandleCall and HandleCast

```haskell
-- | HandleCall callback
type HandleCall req rep st
  = req -> From -> st -> Process (Reply req rep st)

-- | HandleCast callback
type HandleCast req rep st
  = req -> st -> Process (Reply req rep st)
```

### Client APIs

```haskell
-- | Synchronous call to the server process.
call :: forall req rep. Name -> req -> Process rep

-- | Sends an asynchronous request to the server process.
cast :: forall req. Name -> req -> Process ()
```

## GenStatem Behaviour

### Event-Driven FSM

See: [Erlang gen_statem Behaviour](https://erlang.org/doc/design_principles/statem.html)

```
State(S) x Event(E) -> Actions(A), State(S')
```

### GenStatem Typeclass

```haskell
class GenStatem e s d | e -> s, s -> d, d -> e where
  handleEvent :: HandleEvent e s d
```

### CodeLock Example

```haskell
module Demo.FSM.CodeLock
  ( name
  , start
  , push
  , stop
  ) where

import Prelude

import Control.Behaviour.GenStatem
  ( class GenStatem
  , Action(..)
  , EventType(..)
  , Init
  , OnEvent
  , initOk
  , handleWith
  , unhandled
  )
import Control.Behaviour.GenStatem as FSM

data Event = Button Integer | Lock
data State = Locked | Opened
data Data = Data
  { code :: [Integer]
  , length :: Integer
  , buttons :: [Integer]
  }

instance Eq State where
  eq Locked Locked = true
  eq Opened Opened = true
  eq _ _ = false

instance GenStatem Event State Data where
  handleEvent = handleWith [(Locked, locked), (Opened, opened)]

name :: Atom
name = :code_lock

start :: [Integer] -> Process Pid
start code = FSM.startLinkWith name (init code)

push :: Integer -> Process ()
push n = FSM.cast name (Button n)

stop :: Process ()
stop = FSM.stop name

init :: [Integer] -> Init Event State Data
init code = initOk Locked d
  where d = Data $ { code = reverse code
                   , length = length code
                   , buttons = []
                   }

locked :: OnEvent Event State Data
locked Cast (Button n) (Data d) =
  let buttons = take d.length [n|d.buttons]
   in if buttons == d.code then
        let actions = [StateTimeout 1000 Lock] in
            FSM.nextWith Opened (Data d{buttons = []}) actions
      else FSM.keep (Data d{buttons = buttons})

locked t e d = unhandled t e Locked d

opened :: OnEvent Event State Data
opened Cast (Button _) d = FSM.keep d

opened Timeout Lock d = do
  println "Timeout Lock"
  FSM.next Locked d

opened t e d = unhandled t e Opened d
```

### Start a FSM process

```haskell
-- | Start a standalone FSM process
start :: forall e s d. GenStatem e s d => (Init e s d) -> Process Pid
startWith :: forall e s d. GenStatem e s d => Name -> (Init e s d) -> Process Pid

-- | Start a FSM process as part of a supervision tree.
startLink :: forall e s d. GenStatem e s d => (Init e s d) -> Process Pid
startLinkWith :: forall e s d. GenStatem e s d => Name -> (Init e s d) -> Process Pid
```

### Init callback

```haskell
-- | Init Result
data InitResult e s d
  = InitOk s d [Action e]
    -- ^ {ok, State, Actions}
  | InitIgnore
    -- ^ ignore
  | InitStop ExitReason
    -- ^ {stop, Reason}

-- | Init Action
type Init e s d = Process (InitResult e s d)
```

### HandleEvent callback

```haskell
-- | Event Type
data EventType
  = Call From | Cast | Info
    -- ^ external event type
  | Timeout
    -- ^ timeout event type
  | Internal
    -- ^ internal

-- | Statem Transition
data Transition e s d
  = Keep d [Action e]
  | Next s d [Action e]
  | Repeat d [Action e]
  | Shutdown ExitReason d

type HandleEvent e s d = EventType -> e -> s -> d -> Process (Transition e s d)

-- | On Event
type OnEvent e s d = EventType -> e -> d -> Process (Transition e s d)

-- | Handle with state functions.
handleWith :: forall e s d. [(s, OnEvent e s d)] -> HandleEvent e s d
```

### Client APIs

```
call :: forall req rep. Name -> req -> Process rep

cast :: forall msg. Name -> msg -> Process ()
```

## GenEvent Behaviour

### Event Handling Principles

See: [Erlang gen_event Behaviour](https://erlang.org/doc/design_principles/events.html)

### GenEvent Typeclass

```haskell
class GenEvent e st | e -> st, st -> e where
  handleEvent :: HandleEvent e st
```

### Event Manager Example

```haskell
module Demo.Event
  ( Event(..)
  , start
  , notify
  ) where

import Prelude

import Control.Behaviour.GenEvent
  ( class GenEvent
  , Init
  , initOk
  , HandleEvent
  , startLinkWith
  )
import Control.Behaviour.GenEvent as E

data Event = EventA | EventB
data State = State [Event]

instance GenEvent Event State where
  handleEvent = handleEvent

name :: Atom
name = :event

start :: Process Pid
start = startLinkWith name init

notify :: Event -> Process ()
notify = E.notify name

init :: Init State
init = initOk (State [])

handleEvent :: HandleEvent Event State
handleEvent e (State events) = do
  println "Event"
  return $ State [e|events]
```

### Start a Event Manager process

```haskell
-- | Start a standalone Event Manager process.
start :: forall e st. GenEvent e st => (Init st) -> Process Pid
startWith :: forall e st. GenEvent e st => Name -> (Init st) -> Process Pid

-- | Start a Event Manager process as part of a supervision tree.
startLink :: forall e st. GenEvent e st => (Init st) -> Process Pid
startLinkWith :: forall e st. GenEvent e st => Name -> (Init st) -> Process Pid
```

### Init callback

```haskell
data InitResult st
  = InitOk st
  | InitOkHib st
  | InitError ExitReason

-- | Init callback
type Init st = Process (InitResult st)
```

### HandleEvent Callback

```haskell
-- | HandleEvent callback
type HandleEvent e st = e -> st -> Process st
```

### Client APIs

```haskell
notify :: forall e. Name -> e -> Process ()

syncNotify :: forall e. Name -> e -> Process ()
```

## Supervisor

### Supervision Tree

See: [Erlang Supervisor Behaviour](https://erlang.org/doc/design_principles/sup_princ.html)

## Example

```haskell
module Demo.Sup (start) where

import Prelude

import Demo.Event as Event
import Demo.Server as Server
import Demo.FSM.PushButton as FSM
import Control.Behaviour.Supervisor
  ( Init
  , initOk
  , Strategy(..)
  , childSpec
  , startSupWith
  )

name :: Atom
name = :sup

start :: Process Pid
start = startSupWith name init

init :: Init
init = initOk (OneForOne, 10, 100)
  [ childSpec "Demo.Event" Event.start
  , childSpec "Demo.Server" Server.start
  , childSpec "Demo.Statem" FSM.start
  ]

```

### Start a Supervisor process

```haskell
-- Start a supervisor process.
startSup :: Init -> Process Pid

-- Start a supervisor with name.
startSupWith :: Name -> Init -> Process Pid
```

### Init callback

```haskell
type SupFlags = (Strategy, Intensity, Integer)

-- | Init Result
data InitResult
  = InitOk SupFlags [ChildSpec]
    -- ^ {ok, State}
  | InitIgnore
    -- ^ ignore

-- | Init callback
type Init = Process InitResult
```

### Restart Strategy

See: [Erlang Restart Strategy](https://erlang.org/doc/design_principles/sup_princ.html#restart-strategy)

```haskell
data Strategy
  = OneForAll
  -- ^ Restart all child processes if one terminated.
  | OneForOne
  -- ^ Restart only the child processs terminated.
  | RestForOne
  -- ^ TODO: comment...
  | SimpleOneForOne
  -- ^ TODO: comment...
```

**OneForOne**

**OneForAll**

**OneForRest**

**SimpleOneForOne**
