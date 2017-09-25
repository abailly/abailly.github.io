------------
title: Weekly Review - Week 39
author: Arnaud Bailly 
date: 2017-09-25
------------

This post is a summary of my activities related to coding and software in the past week. Its purpose is both to serve as a high-level personal log and as a potential source of interesting (or not so interesting) links. Entries are provided in no particular order with minimal comments...

[Bringing the web up to speed with WebAssembly | the morning paper](https://blog.acolyer.org/2017/09/18/bringing-the-web-up-to-speed-with-webassembly/)

: Another great read from TMP, this time about the low-level _lingua franca_ of browser-based computations. Lead me to also read [languagengine - Blog - Differentiating Functional Programs](http://languagengine.co/blog/differentiating-functional-programs/) and the original specification: [spec/pldi2017.pdf at master · WebAssembly/spec](https://github.com/WebAssembly/spec/blob/master/papers/pldi2017.pdf)


[To type or not to type: quantifying detectable bugs in JavaScript | the morning paper](https://blog.acolyer.org/2017/09/19/to-type-or-not-to-type-quantifying-detectable-bugs-in-javascript/)

: Can static typing helps in getting rid of bugs? This paper provides a resounding _Yes!_ answer.

[Parsing with Derivatives](http://matt.might.net/papers/might2011derivatives.pdf)

: One thing that always amazes me is how seemingly unrelated concepts of mathematics have deep and useful practical applications to computing. This paper shows how to apply the notion of _derivatives_ of a function to generate parsers for context-free grammars, following a technique pioneered by Brzozowski in [Derivatives of Regular Expressions](http://maveric.uwaterloo.ca/reports/1964_JACM_Brzozowski.pdf).

[Totally Free](https://personal.cis.strath.ac.uk/conor.mcbride/TotallyFree.pdf)

: I am deeply indebted to my co-worker Alex Babkin for pointing me at this paper that links in very interesting ways to a lot of work I have been doing in the past with _Free monads_ and _Effects_. I have translated the Agda code to Haskell and this helped me see the similarities between the `General` type McBride introduces and the various constructions for Free monads, notably exposed by Ed Kmett in a [series of blog post](http://comonad.com/reader/2011/free-monads-for-less/).

[If I'd Known What We Were Starting | Ray Dillinger | Pulse | LinkedIn](https://www.linkedin.com/pulse/id-known-what-we-were-starting-ray-dillinger)

: "ICOs are a scam" says one of the original inventor and developer of Bitcoin. Interesting insights into the chaotic history of the "blockchain"...

[Why Dependent Types Matter](http://www.cs.nott.ac.uk/~psztxa/publ/ydtm.pdf)

: An already old but nevertheless interesting paper advocating for the use of dependent types in mainstream programming languages. My first exposure to the [Epigram](https://github.com/mietek/epigram2) programming language on which Edwin Brady worked before developing [Idris](https://www.idris-lang.org/).

[confluentinc/ducktape: System integration and performance tests](https://github.com/confluentinc/ducktape)

: A framework for testing microservices and distributed systems
