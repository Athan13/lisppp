# B-Lisp

This is a pretty standard (if simplified) Lisp-like language that compiles into Bracketfuck. It supports the following operations:
- Arithmetic operations over the integers (mod 256).
- Boolean operations, with 0 being falsey and all other integers being truthy.
- Variable assignment using `let` bindings (i.e. `(let (x 3) (+ x x))` gives 6).
- Basic I/O: reading from `stdin` and writing to `stdout` using the `read-byte` and `print` functions.
- Basic control flow:
    - Do blocks of the form `(do (action1) (action2) ...)`.
    - If-then-else conditionals of the form `(if (cond) (then-case) (else-case))`.
    - General **non-recursive** functions.
    - **Tail-recursive** functions strictly of the form `(define f (args) (if (cond) (return-value) (f args')))`.

Upcoming features (no promises):
- Lisp-style lists with `cons`, `car`, and `cdr`.
- General recursive functions.

Full details of the grammar and syntax are in `parser/lexer.x` and `parser/parser.y`. Every B-Lisp program is statically linked with the B-Lisp standard library in `stdlib`, which also has a few useful examples of the language.

My eternal gratitude to the [Esolang wiki](https://esolangs.org/wiki/Brainfuck_algorithms) for their description of existing Brainfuck algorithms for key operations.