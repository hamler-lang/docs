# The Hamler Language FAQ

## 1. About the Language

## 1.1 What does the Hamler Logo mean?

**λE** = Lambda (**λ**) + **E**rlang VM.

![hamler-logo](https://www.hamler-lang.org/images/hamler@2x.png)

### 1.2 Elixir is already there, running on Erlang VM, how does this differ?

**Hamler** is a completely different language from **Elixir**, though they are both compiled to beam bytecode and running on Erlang VM.

1. **Elixir** is a dynamically-typed functional programming language. **Hamler** is a strongly-typed functional language with type checking at compile-time.

2. **Elixir** has Ruby-like syntax, and the community mainly comes from Ruby On Rails. **Hamler** has Haskell and ML-style syntax, the core development team comes from Erlang and Haskell communities.

3. The design of **Hamler** compiler is also different from **Elixir**. **Hamler** source code is compiled to CoreErlang, while **Elixir** code is compiled to Erlang AST.

### 1.3 What are the differences between Hamler and Puerl?

**Purerl** is a backend of PureScript language, which translates PureScript to Erlang source code.

The **Hamler** compiler v0.1 is forked from PureScript v0.13.6. PureScript's Eco-system is mostly tailored for Javascript and NodeJS which is too limited for Erlang/OTP ecosystem if we just make **Hamler** a backend of PureScript.

We introduced many Erlang data types as primary types, such as Erlang atoms, binaries, tuples, lists and maps. And at the same time, we have to modify the whole frontend (CST, AST, CoreFn and CodeGen) of PureScript to support expressions/syntax sugar from Erlang/OTP, such as binary match, list comprehensions, map pattern match...

### 1.4 What's the difference between Hamler and Haskell?

**Hamler** is strictly evaluated, compiled to beam bytecode and running on Erlang VM, though it has the Haskell-style syntax.

### 1.5 How does hot code reloading work with static types, if the data types change between reloads? What's the upgrade story?

The strongly-typed Hamler source code is compiled to dynamic CoreErlang finally. So, we think the hot-upgrade feature of Erlang should still work.


## 2. About the Compiler

### 2.1 Is the compiler forked from [PureScript][PureScriptHamler] project?

Yes. Forked from [PureScript][PureScriptHamler] 0.13.6

[PureScriptHamler]: https://github.com/hamler-lang/purescript


## 3. Install Packages

### 3.1 Is there a Windows install method?

Not yet. Coming soon.


## 4. Tools and IDE

### 4.1 Does that mean the IDE support is there?

IDE support, such as Vim, Emacs and VSCode, is still at the planning stage.


## 5. Milestones

### Is **Hamler** used by [**EMQ X**][EmqxGithub] project in any product already?

No. Hamler v0.1 is far from being used in product applications. We are planning to introduce Hamler 1.0 in [**EMQ X**][EmqxGithub] v7.0 release.

[emqxGithub]: https://github.com/emqx/

