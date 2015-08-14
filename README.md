Evil ML
=======

Evil ML is a joke compiler from ML to **C++ template language**
(not ordinary C++ code).

C++ template is a **higher-order pure functional** programming language
traditionally used for **compile-time** computation, while its syntax is
verbose and hard to use.
[ML](https://en.wikipedia.org/wiki/ML_%28programming_language%29),
a higher-order functional programming language, is simple, practical and
easy to understand, so that we jokingly implemented this compiler. You can
easily use black magic in C++ template programming.

P.S. `constexpr` (supported C++11 or above) is useful. Why don't you use it?

Features
--------

- [OCaml](http://ocaml.org)-like higher-order pure functional language
  (Hindley-Milner polymorphism, no value restriction).
- Type inference is performed. Most type annotations are automatically inferred.
- Variant, a flexible and strong data structure, is supported.
- You can write raw C++ code in `(*! ... *)` in top level.

Difference from OCaml:

- Strings have type `char list` (type `string` does not exist).
- Module system and separate compilation are not supported.
- User-defined operators are not allowed.
- `type` keyword in top level can only define *variant types*. You cannot
  declare aliases of types and records.
- Pattern match is only performed by `match`. Patterns cannot appear in formal
  arguments and l.h.s. of let bindings.
- Exhaustivity checking of pattern matching is not implemented. (future work)
- Identifiers are defined as regular expression `[a-zA-Z_][a-zA-Z0-9_]*`.
  Primes cannot be used, and names that begin `__ml_` are
  reserved by this compiler. Identifiers of data constructors begin capital
  letters.
- Top-level shadowing of identifiers (variables, types, and constructors) is
  prohibited.

Install
-------

```
./configure
make
make install
```

Usage
-----

You can compile `foo.ml` as follows:

```
evilml foo.ml
```

Demo: quick sort
----------------

[examples/quicksort/qsort.ml](examples/quicksort/qsort.ml) implements quick sort
of a list of 8 elements. You can compile the ML program into C++ template as
[online demo](http://akabe.github.io/evilml/).

1. Check the check box of "Generate stand-alone code (embedding evilml.hpp)"
2. Push the button "Compile"
3. Copy and paste the generated C++ code into file `qsort.cpp`
4. Try to compile and run it:

```
$ g++ qsort.cpp
$ ./a.out
1  2  3  4  5  6  7  8
```

In order to make sure that sorting is executed in compile time,
we suggest to use `g++ -S qsort.cpp` and open `qsort.s`:

```asm
...
	movl	$1, 4(%esp)   ; pass 1 to printf
	movl	$.LC0, (%esp)
	call	printf
	movl	$2, 4(%esp)   ; pass 2 to printf
	movl	$.LC0, (%esp)
	call	printf
	movl	$3, 4(%esp)   ; pass 3 to printf
	movl	$.LC0, (%esp)
	call	printf
	movl	$4, 4(%esp)   ; pass 4 to printf
	movl	$.LC0, (%esp)
	call	printf
	movl	$5, 4(%esp)   ; pass 5 to printf
	movl	$.LC0, (%esp)
	call	printf
	movl	$6, 4(%esp)   ; pass 6 to printf
	movl	$.LC0, (%esp)
	call	printf
	movl	$7, 4(%esp)   ; pass 7 to printf
	movl	$.LC0, (%esp)
	call	printf
	movl	$8, 4(%esp)   ; pass 8 to printf
	movl	$.LC1, (%esp)
	call	printf
...
```

(Of course, you can use `std::cout` to print integers in `qsort.cpp`,
 however we make use of `printf` for readable assembly code.)

Bugs
----

- `let rec diverge _ = diverge ()` should be infinite loop, but generated C++
  code causes compilation error. `let rec diverge n = diverge (n+1)` passes C++
  compilation. (I don't know the formal definition of reduction rules of C++
  template expressions.)
- C++03 template prohibits operation of float-point values, so that this
  compiler outputs wrong code.
