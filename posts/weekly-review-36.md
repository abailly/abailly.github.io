------------
title: Weekly Review - Week 36
author: Arnaud Bailly 
date: 2017-09-04
------------

This post is a summary of my activities related to coding and software in the past week. Its purpose is both to serve as a high-level personal log and as a potential source of interesting (or not so interesting) links. Entries are provided in no particular order with minimal comments...

[solo-io/squash: The debugger for microservices](https://github.com/solo-io/squash) 

: A tool to coordinate debugging of a bunch of processes running inside docker containers. Only for gdb and dlv based programs with VS Code editor

[jrclogic/SMCDEL: A symbolic model checker for Dynamic Epistemic Logic.](https://github.com/jrclogic/smcdel) 

: I have been fascinated by model-checking and non standard logics since I discovered this in 2000. Too bad I never invested the time to genuinely master that field. Looks like past years have seen much advance in the field and this MC for epistemic logic interested me because it's written in Haskell and implements BDD, something I might need for work I am currently doing on distributed systems testing.

[apfelmus - Reasoning about space leaks with space invariants](https://apfelmus.nfshost.com/blog/2013/08/21-space-invariants.html) 

: Thanks to my coworker [Alex Babkin](https://www.linkedin.com/in/alexbabkin/) for this post. It helped me get a deeper understanding of laziness and space-time tradeoffs in Haskell.

[Monad transformers, free monads, mtl, laws and a new approach](https://ocharles.org.uk/blog/posts/2016-01-26-transformers-free-monads-mtl-laws.html) 

: An interesting approach to offer the best of both free monads/effects and monad transformers. Materialized in the [transformers-eff](https://ocharles.org.uk/blog/posts/2016-04-23-transformers-eff.html)  package

[Asymptotic Improvement Of Computations Over Free Monads](http://www.janis-voigtlaender.eu/papers/AsymptoticImprovementOfComputationsOverFreeMonads.pdf) 

: An important stop in my journey towards better understanding of free monads. 

[Handlers in action](http://homepages.inf.ed.ac.uk/slindley/papers/handlers.pdf) 

: An alternative approach to composable effects and transformers, based on a custom DSL for expressing effects. Uses TH's quasi-quoters which I am not a big fan of...

[Parameterized Effects](http://okmij.org/ftp/Haskell/extensible/param-eff.pdf) 

: Follow-up over _extensible effects_ paper taking into account parametric monads. The goal is to be able to embed and check state transitions at the type level making it impossible to write things like reading a file before opening it

[A concurrent perspective on smart contracts | the morning paper](https://blog.acolyer.org/2017/08/30/a-concurrent-perspective-on-smart-contracts/) 

: This week's TMP was dedicated to a series of papers on Smart Contracts, a subject I have a keen interest in given that I am currently working for a company developing a platform for Smart Contracts! Also of interest is [Adding concurrency to smart contracts](https://blog.acolyer.org/2017/08/31/adding-concurrency-to-smart-contracts/) and [Step by step towards creating a safe smart contract: lessons from a cryptocurrency lab | the morning paper](https://blog.acolyer.org/2017/09/01/step-by-step-towards-creating-a-safe-smart-contract-lessons-from-a-cryptocurrency-lab/) 

[Why3](http://why3.lri.fr/) 

: A platform for theorem-prover-based program verification providing a "front-end" on top of 2 dozens theorem proving tools, including Coq.

[Tweag I/O - Compact normal forms + linear types = efficient network communication](https://www.tweag.io/posts/2017-08-24-linear-types-packed-data.html) 

: Nice insights on _linear types_ from the people implementing them in GHC. Did I say how cool the people at http://tweag.io were?

[jship - Monad Transformer Commutativity](https://jship.github.io/posts/2017-08-27-monad-transformer-commutativity.html) 

: Understanding how to stack your monad transformers

[Web Design - The First 100 Years](http://idlewords.com/talks/web_design_first_100_years.htm) 

: Read this great talk thanks to my friend Bernard. Contains some important insights on how much we suck at predicting the future and an implicit call to arms to steer the future of the internet in the _right_ direction

[barrucadu/dejafu: Systematic concurrency testing meets Haskell.](https://github.com/barrucadu/dejafu) 

: Discovered this package thanks to Mathieu Boespflug from tweag.io! A great tool to systematically check concurrent programs.

[Review: Information Effects :: Reasonably Polymorphic](http://reasonablypolymorphic.com/blog/information-effects) 

: Mind-boggling blog post on how to implement a language to define typed reversible computations in Haskell

[Partial Order Reduction](http://www.cs.utexas.edu/users/mckinley/papers/bpor-oopsla-2013.pdf) 

: State explosion is the bane of model-checkers. This paper proposes algorithms (implemented in dejafu) to reduce the number of states to explore based on some notion of _equivalence relations_ between states.
