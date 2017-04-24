------------
title: Weekly Review - Week 16
author: Arnaud Bailly 
date: 2017-04-24
------------

This post is a summary of links related to coding and software I followed and read (sometimes, skimmed over) in the past week. Its purpose is both to serve as a high-level personal log and as a potential source of interesting (or not so interesting) links. Entries are provided in no particular order with minimal comments…

## 2017-04-17 Monday

[The Teeny Tiny Mansion](http://svn.clifford.at/handicraft/2017/tttm/README)

: A formally proved adventure game: There is no dead end in the game, every states has a path that leads to a winning state.

[The CBMC Homepage](http://www.cprover.org/cbmc/)

: The model checker that is used by `tttm`. What's interesting is that it works directly on C/C++ source files. Could probably be adapted to work on Java files, if this is not already done...

[Is there library support for FOL or propositional logic? - Google Groups](https://groups.google.com/forum/#!topic/idris-lang/DHYUrWZEa8Q)

: Some discussion on the `idris-lang` mailing about adding native First-Order Logic support in the language

## 2017-04-18 Tuesday

[Enquête sur les modes d’existence](http://sociologie.revues.org/1573)

: (in French) Criticism of Latour's book and more generally Latour's philosophy. Interesting counterpoint while I am trying to use Latour's concepts to better understand software and our relationship with it.

[Le réalisme kitsch | Zilsel](http://zilsel.hypotheses.org/2103#comments)

: (in French) Another criticism, this time of [Object-Oriented Ontology](/posts/weekly-review-15.html) and Speculative Realism

[CSS TDD - Review](http://www.codecraftuk.org/events/2015/02/css-tdd-review)

: Is it possible, and useful, to write CSS in TDD mode, with automated tests? Not sure this post provides a positive answer...

## 2017-04-19 Wednesday

[Types + Properties = Software: designing with types](http://blog.ploeh.dk/2016/02/10/types-properties-software-designing-with-types/)

: Relinking to a classical series of post on Type-Driven Development in F#. 

[LeanDevBalleBalle.pdf](http://www.lean.enst.fr/wiki/pub/Lean/LesPublications/LeanDevBalleBalle.pdf)

: Short description of *Lean Development* as a complement to *Lean Manufacturing*, by one of the foremost French expert on Lean

[Org layer](http://spacemacs.org/layers/%2Bemacs/org/README.html#tree)

: How to use org-mode in Spacemacs. As I do a lot of pairing in Spacemacs those days, I need to beef up my skills with this environment and retarget my muscles memory to new keystrokes sequences...

[abailly/yak-o-matic: visualize your yaks](https://github.com/abailly/yak-o-matic)

: Thinking of reviving this old project, written with Willem van den Ende at Agile Open France 2013, whose purpose was visualising a dependency graph of tasks given an org-mode file.

## 2017-04-20 Thursday

[Lost in Technopolis](http://newartisans.com/2017/04/haskell-and-z3/)

: Some fascinating note on the use of [category theory](https://github.com/conal/concat) as an abstract *assembly language* to generate equations that are fed to [Z3](https://github.com/Z3Prover/z3) theorem prover.

[encryption - How do I compute the approximate entropy of a bit string? - Stack Overflow](http://stackoverflow.com/questions/2979174/how-do-i-compute-the-approximate-entropy-of-a-bit-string)

: I have been working at the end of the week on a small coding challenge: Write a script that emulates the behavior of `cat /dev/random`, e.g. that generates an infinite stream of as-random-an-possible bytes. My first thought was : How do I test the randomness of a stream of bytes, and this started some research on the web on how to compute entropy of a string

[Salsa20](https://en.wikipedia.org/wiki/Salsa20)

: I chose to use this *stream cipher* algorithm to generate my stream of random bytes, encryting a stream of bytes coming from a network card put in promiscuous mode. It is reasonably straightforward to understand and implement and generates good "randomness"

[RS14.pdf](https://people.csail.mit.edu/rivest/pubs/RS14.pdf)

: An article from Ron Rivest describing Spritz, an alternative to RC4.

## 2017-04-21 Friday

[Entropy and Random Number Generators @ Calomel.org](https://calomel.org/entropy_random_number_generators.html)

: Helpful post on how to compute "entropy" for random generators. Link to the [Ent](http://www.fourmilab.ch/random/random.zip) tool

[Haskell for all: Use Haskell for shell scripting](http://www.haskellforall.com/2015/01/use-haskell-for-shell-scripting.html)

: I of course chose to write my random generation script in Haskell, trying to use as few dependencies as possible: I managed to get it working using only what's provided by GHC 8.0.2 distribution. Here is an alternative and more expressive way of writing scripts in Haskell.

## 2017-04-22 Saturday

[Salsa Reference Implemention](https://cr.yp.to/snuffle/salsa20/ref/salsa20.c)

: Reference implementation of Salsa algorithm in plain old C. [salsafamily-20071225.pdf](https://cr.yp.to/snuffle/salsafamily-20071225.pdf) provides more details and rationale on this family of algorithms. This lead me to more stuff on PRNGs and how to gather entropy from the system:

* [PCG, A Family of Better Random Number Generators | PCG, A Better Random Number Generator](http://www.pcg-random.org/)
* [Brendan's blog » Top 10 DTrace scripts for Mac OS X](http://dtrace.org/blogs/brendan/2011/10/10/top-10-dtrace-scripts-for-mac-os-x/)
* [How to speed up OpenSSL/GnuPG Entropy For Random Number Generation On Linux – nixCraft](https://www.cyberciti.biz/open-source/debian-ubuntu-centos-linux-setup-additional-entropy-for-server-using-aveged-rng-tools-utils/)

[Announcing the AWS Chatbot Challenge – Create Conversational, Intelligent Chatbots using Amazon Lex and AWS Lambda | AWS Blog](https://aws.amazon.com/fr/blogs/aws/announcing-the-aws-chatbot-challenge-create-conversational-intelligent-chatbots-using-amazon-lex-and-aws-lambda/)

: Something definitely fun I would love to work on!

## 2017-04-23 Sunday

[A haskell implementation of a pseudo-random number generator using Salsa20 algorithm](https://gist.github.com/abailly/9c2b23cca63e66a71093b05462a1e17d)

: Final implementation of my pseudo-random bytes generator in Haskell

[Shannon Entropy](http://www.bearcave.com/misl/misl_tech/wavelets/compression/shannon.html)

: I started to use compression ratio as a proxy for entropy of a finite stream of bytes but replaced it with direct measurement of Shannon's entropy, using the standard formula to compute the number of bits needed to represent all elements of a finite stream $X$ with probability of occurence for each letter of $p_i$.

[ARM Releases Machine Readable Architecture Specification – Alastair Reid – Researcher at ARM Ltd](https://alastairreid.github.io/alastairreid.github.io/ARM-v8a-xml-release/)

: Another fascinating post on the use of formal specifications, this time in the realm of hardware architecture.

[Forget about project planning; it just doesn't work | Alex Katsanos | Pulse | LinkedIn](https://www.linkedin.com/pulse/forget-project-planning-just-doesnt-work-alex-katsanos?trk%3Dv-feed&trk%3Dv-feed&lipi%3Durn%253Ali%253Apage%253Ad_flagship3_feed%253Bh8fPT2gGIAweLWHuUJcjxQ%253D%253D)

: Looks like waterfall does not very well in "traditional" engineering either...

[Testing Theories of American Politics](https://scholar.princeton.edu/sites/default/files/mgilens/files/gilens_and_page_2014_-testing_theories_of_american_politics.doc.pdf)

: Can we empirically demonstrate how policies are formed and driven? This article shows the answer is yes by testing actual decisions against theories of whose interest these decisions serve. Would definitely be interested in a similar study for Europe...
