# [DipDup](https://github.com/AlephAlpha/DipDup)

__DipDup__ is a stack-based esoteric programming language inspired by [Underload](http://esolangs.org/wiki/Underload) and based on [Joy](http://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language).

[Joy](https://en.wikipedia.org/wiki/Joy_(programming_language)), invented by Manfred von Thun in 2001, is a stack-based, concatenative, purely functional programming language. Dipdup is a subset of Joy. From the 202 commands in Joy, I chose four: `dip`, `dup`, `pop`, and `cons`, and represented them with four symbols: `^`, `_`, `!`, and `:`.

## Stack

Dipdup is a stack-based language. Everything in the stack is a list. Every command is a function that maps stacks to stacks. The initial stack is an infinite stack of emepty lists. After the execution, the top of the stack will be printed (without the outmost brackets).

## Commands

### `dup`

Duplication. If the original stack is `...ba` (the top of the stack is written on the right hand side), the new stack will be `...baa`. `dup` is written as `_` in DipDup.

### `pop`

Here it means drop or discard. It won't return or print the item it pop. If the original stack is `...ba`, the new stack will be `...b`. `pop` is written as `!` in DipDup.

### `cons`

Like `cons` is Lisp, or `:` is Haskell. If the original stack is `...b[a]`, the new stack will be `...[ba]`. `cons` is written as `:` in DipDup.

### `dip`

This one is a little complicated. If the original stack is `...cb[a]`, it will first pop `b` and `[a]`, then execute `[a]` as a program on the stack `...c`, and finally push `b` back. `dip` is written as `^` in DipDup.

### Lists

A list is a list of lists or commands, written inside square brackets, without any space or other separator between its items. A list can be thought of as a program or a quoted function, so it can act on the stack with the help of `dip`. In a program, a list is pushed onto the stack.

### No-ops

Every character except `^`, `_`, `!`, `:`, `[` and `]` is a no-op.

## Examples

### Hello, World!

```
[Hello, World!]
```

### Quine

```
[_:]_:
```

## Some Useful Snippets

### Execute

```
_^!
```

Like `i` in Joy or `^` in Underload. If the original stack is `...b[a]`, it will execute `a` as a program on the stack `...b`.

### Swap

```
[]:^
```

Like `swap` in Joy or `~` in Underload. If the original stack is `...cba`, the new stack will be `...cab`.

## Turing Completeness

To show that DipDup is Turing complete, it suffices to implement the three primitive combinators in [combinatory logic](http://esolangs.org/wiki/Combinatory_logic): `I`, `K` and `S`.

### `I` combinator

```
[]
```

### `K` combinator

```
[[[!]^]:]
```

### `S` combinator

```
[[[[[_]^^]^_^!_^!]::]:]
```

This proof is inspired by [a proof of the Turing completeness of Underload](http://esolangs.org/wiki/Underload#Unlambda_to_Underload).

## About This Interpreter

This interpreter is written in Haskell. It depends on two packages: [`base`](http://hackage.haskell.org/package/base) and [`haskeline`](http://hackage.haskell.org/package/haskeline).

Running the interpreter without any argument will start the REPL. You can also add a filename as the argument, and the interpreter will run the program in the file and print the result.
