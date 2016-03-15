An implementation of dimension type system by Kennedy [1] in OCaml.

This library provides type inferrence algorithm only for polynomials.
In order to infer dimention types of some program language, you may
extract polynomials from a program construct constraints, and execute
type inferrence.  See `example/imp.ml` and `lib/solver.mli` for more
details.

The type inferrence algorithm implemented in this library basically
follows [1].  The differences are

  - polymorphic types is not supported
  - some heuristics are implemented (see comments in `lib/solver.ml`
    for details)

## Install

```
$ git clone https://github.com/SoftwareFoundationGroupAtKyotoU/dimtype.git
$ make
$ make install
```

We provide an option to install using OPAM:

```
$ opam pin add dimtype https://github.com/SoftwareFoundationGroupAtKyotoU/dimtype.git
$ opam install dimtype
```

## Documentation

```
$ make doc
$ open dimtype.docdir/index.html in a web browser
```

## Example

```
$ ./configure --enable-example
$ make
$ ./imp.byte
```

See `example/imp.ml`.

## Reference

[1] Andrew Kennedy. "Dimension Types" Proceedings of the 5th European Symposium on Programming: Programming Languages and Systems (1994): 348-362.
