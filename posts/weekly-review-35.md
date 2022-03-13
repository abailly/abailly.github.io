---
title: Weekly Review - Week 35
author: Arnaud Bailly 
date: 2017-08-28
---

This post is a summary of my activities related to coding and software in the past week. Its purpose is both to serve as a high-level personal log and as a potential source of interesting (or not so interesting) links. Entries are provided in no particular order with minimal comments...

[Efficient Immutable Collections](https://michael.steindorfer.name/publications/phd-thesis-efficient-immutable-collections.pdf) 

: Immutables collections paves the way for efficient program transformations and parallelization, but they might lead to inefficiencies from garbage collection or repeated traversals. This thesis provides techniques to build efficient immutable collections on the JVM.

[GitHub - ipfs/ipfs: IPFS - The Permanent Web](https://github.com/ipfs/ipfs) 

: Discovered this fascinating project which is the foundation of numerous other projects, like [gx](https://github.com/whyrusleeping/gx) a package management tool.

[End To End Testing With Hspec (Almost)](https://vadosware.io/post/end-to-end-testing-with-hspec-almost/) 

: How to (not) use Haskell and Hspec to write end-to-end testing using [hspec](http://hackage.haskell.org/package/hspec) and [hs-webdriver](https://hackage.haskell.org/package/webdriver). Looks like recent changes in webdriver broke Haskell's library...

[Interactions of individual and pair programmers with an intelligent tutoring system for computer science | the morning paper](https://blog.acolyer.org/2017/08/16/interactions-of-individual-and-pair-programmers-with-an-intelligent-tutoring-system-for-computer-science/)

: *tl;dr*: We discovered that while both groups exhibited significant learning gains, pair programmers completed problems more quickly, relied less heavily on system-provided examples, coded more efficiently, and showed higher signs of engagement.

[On the design of distributed programming models | the morning paper](https://blog.acolyer.org/2017/08/17/on-the-design-of-distributed-programming-models/) 

: Another gem from Adrian Colyer's treasure trove: What's needed in languages to fully support distributed systems programming?

[Growing a protocol | the morning paper](https://blog.acolyer.org/2017/08/23/growing-a-protocol/) 

: This paper triggered some research on distributed systems testing, along with:

* [Lineage-driven Fault Injection | the morning paper](https://blog.acolyer.org/2015/03/26/lineage-driven-fault-injection/): Analysing programs execution to inject _interesting_ errors, e.g. ones that have high probability of triggering bugs
* [SAMC](https://www.usenix.org/system/files/conference/osdi14/osdi14-paper-leesatapornwongsa.pdf): A tool to drive testing through model-checking with specific techniques to reduce state explosion
* [osrg/namazu: 鯰: Programmable fuzzy scheduler for testing distributed systems](https://github.com/osrg/namazu): Fuzz testing of distributed systems
* [Parallel Data Lab Project: dbug: Systematic and Scalable Testing of Concurrent Systems](http://www.pdl.cmu.edu/dbug/): Another model-checking/testing tool interposing itself between processes and OS/libraries to catch system calls and inject errors
* [Concurrent Systematic Testing at Scale](http://www.pdl.cmu.edu/PDL-FTP/associated/CMU-PDL-12-101.pdf): Paper from the dbug project
* [The Mace Project | MaceSystems](http://www.macesystems.org/mace/): A language and toolset to design and implement distributed systems
* [LMC](https://infoscience.epfl.ch/record/151495/files/EPFL_TH4858.pdf): Solves the state explosion problem by analysing and tracking local state instead of the global state

[Yet Another Visit to Paxos](https://www.zurich.ibm.com/~cca/papers/pax.pdf)

: One the numerous Paxos papers, this time introducing crash-tolerance and Byzantine failure tolerance. Lead to PBFT-Smart algorithm.

[Hire Like the Israeli Military](http://www.countingcalculi.com/features/smart_hiring/) 

: Detailed article on practical application of Daniel Kahneman's hiring techniques from [Thinking, Fast and Slow](https://en.wikipedia.org/wiki/Thinking,_Fast_and_Slow). In the book, Kahneman advocates the use of a simple rating system based on questions to identify observable behaviours of hired persons and objectivize their past experience.

[haskell - The Pause monad - Stack Overflow](https://stackoverflow.com/questions/10236953/the-pause-monad) 

: One article from a series I read this weekend trying to acquire a deeper understanding of Free monad construction and how to implement pause/resume on top of Free

* [Coyoneda is just the Free Functor : haskelltil](https://www.reddit.com/r/haskelltil/comments/4ea7er/coyoneda_is_just_the_free_functor/): [Coyoneda](https://hackage.haskell.org/package/kan-extensions-5.0.2/docs/Data-Functor-Coyoneda.html) functor reifies functor application...
* [From Hask Till Dawn: Coyoneda and fmap fusion](http://alpmestan.com/posts/2017-08-17-coyoneda-fmap-fusion.html): ... a technique that can be used to make traversal of functorial structures and repeated fmap applications more efficient
* [Applicative Effects in Free Monads](https://elvishjerricco.github.io/2016/04/08/applicative-effects-in-free-monads.html) 
* [More on Applicative Effects in Free Monads](http://elvishjerricco.github.io/2016/04/13/more-on-applicative-effects-in-free-monads.html) 

[The Comonad.Reader » Free Monads for Less (Part 1 of 3): Codensity](http://comonad.com/reader/2011/free-monads-for-less/) 

: A serie of article from Ed Kmett on the Free monad construction and how to make it more efficient, followed by:

* [The Comonad.Reader » Free Monads for Less (Part 2 of 3): Yoneda](http://comonad.com/reader/2011/free-monads-for-less-2/) 
* [The Comonad.Reader » Free Monads for Less (Part 3 of 3): Yielding IO](http://comonad.com/reader/2011/free-monads-for-less-3/) 
