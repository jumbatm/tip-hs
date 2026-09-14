# tip

> [!CAUTION]
> **Beginner Haskell code ahead**
>
> Reading this codebase may cause severe discomfort to experienced functional
> programmers. May cause dizziness, drowsiness, and/or degradation of ability
> to write clean, idiomatic Haskell code. Proceed at own risk.

A Haskell implementation of TIP, the Tiny Imperative Programming Language, as described in Anders Møller and Michael I. Schwartzbach's book _Static Program Analysis_ [1].

##  Implementation status

As this is just a learning project, development is mainly driven by my own curiosity. The broad plan is to implement an interpreter, then continue onto implementing a dataflow analysis framework and a few analyses just to explore their implementation. Pretty neat if it boils down to `fix f` for some `f` :).

### Parser

- [x] Parser Monad and Applicative
- [x] Source location tracking
- [x] Error message generation, labelling
- [x] Parse all .tip files from original repo
- [ ] Continuation passing style parser (a la Parsec)
- [ ] Golden file testing for AST

### Interpreter

- [ ] Interpreter monad / applicative
- [ ] Run on all example programs
- [ ] Golden file testing for interpreter state

### Static Analysis

- [ ] Implement typechecking
- [ ] Implement dataflow framework
- [ ] Implement a few static analyses (TBD which)


## References
1. Møller, A., & Schwartzbach, M. I. (2012). Static program analysis. Notes. Feb.

