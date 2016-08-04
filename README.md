# [DipDup](https://github.com/AlephAlpha/DipDup)

__DipDup__ is a stack-based esolang inspired by [Joy](http://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language) and [Underload](http://esolangs.org/wiki/Underload).

I think it's Turing complete, but don't know how to prove it.

## Commands

| Command | Name | Description |
|:--:|:--:|--|
| `^` | `dip` | `X [P]` → `... X` : Save `X`, execute `[P]`, push `X` back. |
| `_` | `dup` | `X` → `X X` : Duplicate `X`. |
| `!` | `pop` | `X` →  : Discard `X`. |
| `:` | `cons` | `X [A]` → `[XA]` : Prepend `X` to `[A]`.  |
| `[X]` | | → `[X]` : Push `[X]`. |

Other commands are no-ops.

## Examples

### Execute

```
_^!
```

### Swap

```
[]:^
```

### Hello, World!

```
[Hello, World!]
```

### Quine

```
[_:]_:
```

## Natural numbers

A natural number `[N]` is a quoted function that `X [P] N` returns `X [P] (N-1) P`.

Please add an issue if you find a better way to represent natural numbers.

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

Please add an issue if you find a better way to represent bools.

### False

```
[!]
```

### True

```
[[!]^]
```
