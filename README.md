# Lisp++ - Lisp With More Brackets

Have you ever looked at your Lisp-like language and thought "there's not enough brackets here"? No longer! This library provides a transpiler from a Lisp-like language (B-Lisp) to a version of Brainfuck where all the characters are brackets (Bracketfuck). Finally, you can program in Lisp without all those pesky non-bracket characters getting in the way.

Ships with an interpreter for Bracketfuck so that you can test your programs.

Written in Haskell using cabal. To try:
- `git clone` this directory
- `cabal build`
- `cabal run compile -- <file-name.lisp>` to compile from Lisp to Bracketfuck
- `cabal run bracketfuck -- <file-name.b> <tape-size>` to execute your file.

See `compile/README.md` and `bracketfuck/README.md` for details on B-Lisp and Bracketfuck respectively.