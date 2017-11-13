------------
title: CodeMesh 2017 - Takeaways
author: Arnaud Bailly
date: 20167-11-13
------------

here are some key takeaways from this year's [codemesh](http://www.codemesh.io/codemesh2017) conference. i must say i never had such an intense conference, with lot of fascinating and mind-blowing sessions which bred lots of new or old ideas in my mind. thanks a lot to the organisers and the amazing speakers which made this event memorable.

* How to apply simple machine learning techniques and speculative execution to optimise repeated executions of programs or program fragments, caching pre-computed execution path to skip some computations,
* Using _session types_ to design applicative protocols as _finite state machines_ enforced by the type system hence statically verified, with some encoding examples in haskel and idris,
* An answer to the classical [fizzbuzz](http://wiki.c2.com/?FizzBuzzTest) interview test where one the y combinator and church numerals from scratch using a pure untyped λ-calculus, in the spirit of the now famous piece by Aphyr [Typing the technical interview](https://aphyr.com/posts/342-typing-the-technical-interview),
* Why it is actually a good idea to model `IO` as a monad even in an impure language like scala, as it gives the compiler the capability to verify and enforce proper use of _side effects_ statically,
* How messy and imprecise the _Computer Science Metanotation_, a language which is ubiquitous in PLT research papers to define _formal systems_, has become over the past 30 or 40 years,
* How to write (potentially infinite) _towers of interpreters_, with reflection and reification capabilities between each layer, and collapse the resulting tower in a single-pass compiler. This year's [favorite](http://lampwww.epfl.ch/~amin/), ex aequo with Tomas Petricek's talk,
* Whence does Haskell come and what's the history of Miranda, where one learns there was no λ-calculus nor first-class functions in McCarthy's LISP and the Programming Languages' history is far from the linear reconstruction we often are told it is,
* Would aliens be able to understand  λ-calculus which amounts to asking classical philosophy of mathematics question: Are mathematical objects _discovered_ from the the land of eternal truths they are part of or _invented_ hence subjects to historical and sociological contingencies. With fascinating references to works by Lakatos and Lakoff on the history and sociology of mathematics,
* What benefits one can reap from abstracting concurrent IOs with [Haxl](https://github.com/facebook/Haxl/), a great library developed at Facebook by [Simon Marlow](http://simonmar.github.io/),
* How to derive _implementations_ of functions from examples thanks to [Barliman](https://github.com/webyrd/Barliman/) and logic programming with [miniKanren](http://minikanren.org/),
* The use of the concept of  _Feedback Loops_ and _Feedback Structures_ to design and develop complex self-regulated distributed systems.
