Hollow heap
===========

This repository contains an implementation
of [hollow heaps](http://cs.au.dk/~tdh/papers/Hollow-heaps.pdf) in
Ocaml.

Hollow heaps are an imperative brand of heaps with O(1) `insert`,
`merge`, `find_min`, and `decrease_key` (all the complexity is born by
`delete_min`). It has a relatively simple implementation, and is
supposed to have very good performance.

## Status ##

This package is still completely untested.

- [X] Type-checks
- [X] API & code documentation
- [X] Tests & CI
- [ ] Publish API documentation
- [ ] Benchmark
- [ ] Package to Opam

