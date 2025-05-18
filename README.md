# Lisp++ - Lisp With More Brackets

Have you ever looked at your Lisp-like language and thought "there's not enough brackets here"? No longer! This library provides a transpiler from a Lisp-like language to a version of Brainfuck where all the characters are brackets (Bracketfuck). Finally, you can program in Lisp without all the pesky non-bracket characters getting in the way.

Ships with an interpreter for Bracketfuck so that you can test your programs.

Written in Haskell using cabal. To try, `git clone` this directory and run `cabal run compile -- <file-name.lisp>` to compile from Lisp to Brainfuck, and `cabal run bracketfuck -- <file-name.b>` to execute your file. See `app/compile/README.md` and `app/bracketfuck/README.md` for details on both languages.