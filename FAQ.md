# FAQ

## Language

## What does the Hamler Logo mean?

A: Lambda (λ) + Erlang VM.

![hamler-logo](https://avatars2.githubusercontent.com/u/49756617?s=200&v=4)

### Elixir is already there. ,running on Erlang VM, how does this differ?

TODO:

### What's the difference between Hamler and Puerl?

TODO:

First, purerl translates PureScript to Erlang source code directly, and hamler compile source code to CoreErlang IR.

Second, we modified the CST, AST and CoreFn of PureScript to make the hamler syntax is more like Haskell.

The hamler compiler is forked from PureScript compiler 0.13.6.

### What's the difference between Hamler and Haskell?

TODO:

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

TODO:

## Tools and IDE

### Does that means the IDE support is there?

A: TODO


## Milestones

### Is **Hamler** used by emq in any product already?

A: No. Hamler 0.1 is far from being used in product applications. We plan to introduce Hamler 1.0 in emqx v7.0 release.

