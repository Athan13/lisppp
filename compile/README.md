# B-Lisp

This is a pretty standard (if simplified) Lisp-like language that compiles into Bracketfuck. It supports the following operations:
- Arithmetic operations over the integers (technically, over $\mathbb{F}_{256}$).
- Variable assignment using `let` bindings (i.e. `(let (x 3) (+ x x))` gives 6).
- Basic I/O: reading from `stdin` and writing to `stdout` using the `read-byte` and `print-byte` functions.
- Basic control flow:
    - Do blocks of the form `(do (action1) (action2) ...)`.
    - If-then-else conditionals of the form `(if (cond) (then-case) (else-case))`.
    - While loops of the form `(while (cond) (do))`.
    - **Non-recursive** function calls of the form `(define (args) (body))`. All functions are of the form $(\mathbb{F}_{256})^n \to \mathbb{F}_{256}$, where $n$ is the number of arguments.

Upcoming features (no promises):
- Lisp-style lists with `cons`, `car`, and `cdr`.
- Recursive function calls.

Examples are provided in the `examples` directory, while full details of the grammar and syntax are in `parser`.