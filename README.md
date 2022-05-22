<div align="center">

# schedgehog
[Hedgehog](https://hedgehog.qa/) implemented in MIT Scheme


<img width=200 src="https://raw.githubusercontent.com/hedgehogqa/haskell-hedgehog/5a03c900bf3d765ec5bf5739e4fc5d8fa0f4f0fd/img/hedgehog-logo-grey.png" />

</div>

This is an implementation of the property-based testing library Hedgehog as
a project for MIT's 6.945 (Adventures in Advanced Symbolic Programming) class.

In software engineering, testing code is considered to be a vital part of
delivering quality software. However, writing unit tests is extremely time
consuming, and it is easy for the programmer to miss certain cases for more
complicated functions. Therefore, it is desirable to try to automate this
processes either partially, or entirely.

Schedgehog, based on existing Haskell work like
[QuickCheck](https://hackage.haskell.org/package/QuickCheck) or
[Hedgehog](https://hedgehog.qa/),
that allows users to formulate properties of programs and then automatically
runs tests based on the properties. Properties that users want to test will be
specified via a DSL embedded in Scheme; these are represented as predicates
(functions returning booleans) along with typing information of the inputs.

Test data is generated based on this typing information. These are checked
against the predicates, and in the case that a failing counterexample is found,
the counterexample is simplified via a build-in extensible rule-based system.

## Basic usage

### Defining properties
Properties are specified with the macro `forall`. Properties are functions
with typed arguments that return a boolean.

```scheme
(define prop:addition-commutativity
  (forall ((x integer) (y integer))
          (= (+ x y) (+ y x))))
```

Primitive types include `integer`, `boolean`, and `float`. A full
list of built-in primitive types are in `src/generator-instances.scm`.
There are also dependent types, like `linear` or `range`, that take
in values as arguments.

```scheme
(define prop:addition-bounds
  (forall ((x (range 10 30)) (y (range 10 30)))
          (< 20 (+ x y) 60)))
```

These types can be combined with type constructors to produce more complex
types. Some examples of type constructors include `pair` and `list`.

```scheme
(define prop:sum-linear
  (forall ((xs (list integer)) (factor integer))
          (equal? (* factor (apply + xs))
                  (apply + (map (lambda (x) (* factor x)) xs)))))
```

### Checking properties
The main interface for checking a property is with the function `check`.
This runs the function against several random test cases. It then prints
the result of the test, along with a counterexample if found.

```scheme
(check prop:addition-commutativity)
; seed: ...
; ok 100 tests
(check prop:addition-bounds)
; seed: ...
; failed after 82 tests, 0 shrinks:
; ((x 10) (y 10))
```

The function `check-with-config` allows checking a property with a custom
configuration (which contains the maximum number of shrinks and tests to run),
size (which determines the size of generated inputs), and seed.

```scheme
(check-with-config
  (make-config 1000 1000)
  1000
  (make-random-state #t)
  prop:addition-commutativity)
; ok 1000 tests
```

## Generators
### Overview

The file `examples/generator.scm` contains extensive examples of the
library of built-in generators.

A generator represents a source of values for randomly checking a property,
along with the way in which values in the type shrink.
Intuitively, it can be thought of as a structure containing two components:
- A generating function `(size seed) -> value`
- A shrinking function `(value) -> [list value]`

Internally, a generator is actually a wrapper around just one function
`(size seed) -> [lazy-tree value]`, a function returning a lazy tree of values.
To create a generator without dealing with lazy tree internals,
we can call library functions that abstract away these details.

Suppose one has a generating function `gen`. The simplest way to make a
generator would be to call `(make-generator-without-shrinking gen)`,
which returns a generator object using the generating function that does not
have a shrinker.
```scheme
(define gen-1 (make-generator-without-shrinking (lambda (size seed) 1)))
```

We can then use the generator with the `generate` function,
which optionally receives a size and seed as parameters.
```scheme
(print-lazy-tree (generate gen-1))
```

We can register a generator with a type by calling `set-generator!`. This
allows it to be called with `arbitrary`, a generic function.

```scheme
(set-generator! 'type-1 gen-1)
(print-lazy-tree (generate (arbitrary 'type-1)))
(check (forall (x type-1) (= x 1)))
```

To replace the shrink function of a generator `gen` with a specified shrink
function `shrink`, we call `(replace-shrinking shrink gen)`.

```scheme
(define (shrink-fn value) (case value ((1) (list 0)) ((0) (list))))
(define gen-2 ((replace-shrinking shrink-fn) gen-1))
(print-lazy-tree (generate gen-2))
```

We can also create parametrized generators. The function `arbitrary`, when
given a list of symbols, will instead search for a handler for the first symbol,
to be called on the list of the remaining symbols as arguments, expecting a
generator returned.

```scheme
(define (gen-constant val)
  (make-generator-without-shrinking (lambda (size seed) val)))
(set-generator! 'type-constant (lambda (val) (gen-constant val)))
(print-lazy-tree (generate (arbitrary '(type-constant 1))))
```



## Repository structure

- `src` schedgehog source code
  - `property.scm`
  - `arbitrary.scm`
- `test` tests for schedgehog

<!---
# Acknowledgements
This project was built by Jason Chen, CJ Quines, and Matthew Ho.
Thanks Gerald Jay Sussman for teaching 6.945 etc etc
-->
