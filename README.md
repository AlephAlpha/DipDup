# [DipDup](https://github.com/AlephAlpha/DipDup)

__DipDup__ is a stack-based esoteric programming language inspired by [Underload](http://esolangs.org/wiki/Underload) and based on [Joy](http://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language).

[Joy](https://en.wikipedia.org/wiki/Joy_(programming_language)), invented by Manfred von Thun in 2001, is a stack-based, concatenative, purely functional programming language. Dipdup is a subset of Joy. From the 202 commands in Joy, I chose four: `dip`, `dup`, `pop`, and `cons`, and denote them by four symbols: `^`, `_`, `!`, and `:`. 

## Commands

### `dup`

Duplication. If the original stack is `...ba` (the top of the stack is written on the right hand side), the new stack will be `...baa`. `dup` is written as `_` in DipDup.

### `pop`

Here it means drop or discard. It won't return or print the item it pop. If the original stack is `...ba`, the new stack will be `...b`. `pop` is written as `!` in DipDup.

### `cons`

Like `cons` is Lisp, or `:` is Haskell. If the original stack is `...b[a]`, the new stack will be `...[ba]`. `cons` is written as `:` in DipDup.

### `dip`

This one is a little complecated. If the original stack is `...c[b]a`, then it will first pop `a`, then execute `b` as a program on the stack `...c`, and finally push `a` back. `dip` is written as `^` in DipDup.

### list

A list is a list of lists or commands, written inside square brackets. There is no space or any other separator between items in a list. In the program, a list will be pushed onto the stack.

### No-ops

Every character except `^`, `_`, `!`, `:`, `[` and `]` is a no-op.

## Stack

Dipdup is a stack-based language. Everything in the stack is a list. The initial stack is an infinite stack of emepty lists. After the execution, the top of the stack will be printed.

##Examples

### Hello, World!

```
[Hello, World!]
```

### Quine

```
[_:]_:
```

<!---
## Some Useful Snippets

### Execute

```
_^!
```

Like `i` in Joy or `^` in Underload.

### Swap

```
[]:^
```

Like `swap` in Joy or `~` in Underload.

## Natural numbers

A natural number `[N]` is a quoted function such that `X [P] N` returns `X [P] (N-1) P`.

### Zero

```
[!]
```

### One

```
[[!][_]^^_^!]
```

### Two

```
[[[!][_]^^_^!][_]^^_^!]
```

### Succ

```
[[_]^^_^!]:
```

### Add

```
[[[[_]^^_^!]:]]^_^!
```

### Multuiply

```
[[[!]]^]^[[[[[[_]^^_^!]:]]^_^!]:]^_^!
```

## Bools

### False

```
[!]
```

### True

```
[[!]^]
```
--->

## Interpreter

This interpreter is written in Haskell. It depends on two packages: [`base`](http://hackage.haskell.org/package/base) and [`haskeline`](http://hackage.haskell.org/package/haskeline).

Run the interpreter without any argument will start the REPL. You can also add a filename as the argument, and the interpreter will run the program in the file and print the result.
