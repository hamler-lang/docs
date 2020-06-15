# FAQ

## Language

## What does the Hamler Logo mean?

A: Lambda (**λ**) + **E**rlang VM.

![hamler-logo](https://avatars2.githubusercontent.com/u/49756617?s=200&v=4)

### Elixir is already there, running on Erlang VM, how does this differ?

1. Elixir is dynamically typed, but Hamler is statically typed. Since Hamler's compiler is adapted from purescript, it has a powerful type system.
2. We prefer Haskell style syntax.
3. Elixir compiles to Erlang AST. Hamler compiles to CoreErlang IR.

### What's the difference between Hamler and Puerl?

Firstly, purerl translates PureScript to Erlang source code directly, and Hamler compile source code to CoreErlang IR.

Secondary, we modified the some frontend (CST, AST and CoreFn) of PureScript to make the syntax of Hamler more like Haskell. At the same time we are trying to make its syntax more approachable for Erlang users.



### What's the difference between Hamler and Haskell?

- Strict vs Lazy evaluation
- Hamler running on Erlang VM - Beam

### How does hot code reloading work with static types, if the data types change between reloads? What's the upgrade story?

### I looked at the "Cheatsheet" and it indeed looks like a different backend for purescript, similar to purerl. I don't know why they say hamlerlang is a new programming language. Maybe there are big differences that I'm not seeing at a glance.

A:

TODO:

## Compiler

### Is the compiler forked from [PureScript][PureScriptSite] project?

Yes. The compiler is forked from [PureScript][PureScriptSite] 0.13.6

[PureScriptSite]: https://www.purescript.org/

## Install

### Is there a windows install method?

Not yet. Coming soon.

## Tools and IDE

### Does that means the IDE support is there?

A: TODO


## Milestones

### Is **Hamler** used by emq in any product already?

A: No. Hamler 0.1 is far from being used in product applications. We plan to introduce Hamler 1.0 in emqx v7.0 release.

