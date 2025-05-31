# Lisp++ - Lisp With More Brackets

Have you ever looked at your Lisp-like language and thought "there's not enough brackets here"? No longer! This library provides a transpiler from a Lisp-like language (B-Lisp) to a version of Brainfuck where all the characters are brackets (Bracketfuck). Finally, you can program in Lisp without all those pesky non-bracket characters getting in the way.

Ships with an interpreter for Bracketfuck so that you can test your programsas well as a standard library (`blisp/stdlib`) which is statically linked in every B-Lisp program.

Written in Haskell using cabal. To try:
- `git clone` this directory
- `cabal build`
- `cabal run blisp -- <file-name.blisp> -o <file-name.b>` to compile from Lisp to Bracketfuck
- `cabal run bracketfuck -- <file-name.b> <tape-size>` to execute your file.

## Example:
```
(print (+ 3 3))
```

compiles to:

```
[)](((>[)](((<>[)<(>]<{
```

which outputs:

```
6
Successfully terminated.
```

See `blisp/README.md` and `bracketfuck/README.md` for details (and more examples) on B-Lisp and Bracketfuck respectively. PRs are more than welcome, though I have no idea why you'd want to submit one.
