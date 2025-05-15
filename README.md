# Lisp++ - Lisp With More Brackets

Have you ever looked at your Lisp-like language and thought "there's not enough brackets here"? No longer! This library provides a transpiler from a Lisp-like language to a version of Brainfuck where all the characters are brackets. Finally, you can program in Lisp without all the pesky non-bracket characters getting in the way.

Ships with an interpreter for this flavour of Brainfuck so that you can test your programs.

Written in Haskell. To try, `git clone` this directory and run `cabal run compile <file-name.lisp>` to compile from Lisp to Brainfuck, and `cabal run exec <file-name.fuck>` to execute your file. See `app/lisp++/README.md` and `app/brainfuck/README.md` for details on both languages.