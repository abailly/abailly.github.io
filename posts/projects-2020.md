---
title: My Projects for 2020
author: Arnaud Bailly
date: 2020-01-01
---

This year 2019 has been exciting in many respects, but it's coming to an end and the time is ripe to look forward: 2020, here I come!
For my own personal records, I have written up a list of things I would like to work on, investigate, learn, discover and more generally spend some time tinkering with. I look forward to the 31st of December, 2020, to check by which margin I overestimated the time I could reasonably put into side projects!

# Idris

I fell in love with Idris (1) while reviewing Edwin Brady's [awesome book](https://www.manning.com/books/type-driven-development-with-idris). Having concrete examples of code that leveraged [Dependent Types](https://en.wikipedia.org/wiki/Dependent_type) was an eye opener to me. Since then, I [have](http://abailly.github.io/posts/dependently-typed-accounting.html) [tried](http://abailly.github.io/posts/dependently-typed-date.html) to get a [better understanding](http://abailly.github.io/posts/dependent-types-ddd.html) of how dependent types could be put to use in my "Real World", ie. to improve the design of mundane software.

In 2020, I plan to continue investigating that topic through various angles.

## Type Systems and Domain-Driven Design

* How can we use Dependent types and Dependently Typed languages to better model the _domains_ our code works on?
* What are other interesting type systems that could be used for that purpose?
* [Homotopy Type Theory](https://en.wikipedia.org/wiki/Homotopy_type_theory) looks both extremely fun, extremely abstract and extremely complex for someone who, like me, does not have a PhD in Algebra ; something like a Category Theory but even more hype! [Cubical Type Theory](https://github.com/mortberg/cubicaltt) is an implementation of HoTT, could it useful to model the kind of problems I encounter in my day job like Double-entry Ledger, payroll software, distributed systems...?
* Two years ago I built an [implementation of MiniTT](https://github.com/abailly/xxi-century-typed/tree/master/minilang) in order to deepen my understanding of such type systems, but this language was too minimalist to be really usable. I have tried again following David Christiansen's [Normalization by Evaluation](http://davidchristiansen.dk/tutorials/nbe/) tutorial, this time in Idris.

## IDE and Developer's Experience

* I have started contributed to the best of my abilities to [Idris2](https://github.com/edwinb/Idris2) and one area I am particularly interested in is how to improve the developer's experience, how to leverage the compiler's machinery to let the developer _dialog with the typechecker_ while building the software?
* [Smalltalk](https://en.wikipedia.org/wiki/Smalltalk) was one of the first languages to provide superior tools for development, with an integrated development environment where one could inspect every aspect of the system and dynamically change it, getting instant feedback. Could we do the same with Idris2, letting the developers interact with the compiler _and_ the live system's state while building it? Idris, like [Coq](https://coq.inria.fr/), provides sophisticated tooling for _theorem proving_ which is just the activity of deriving code (proof) from types (propositions).
* [Hazel](https://github.com/hazelgrove/hazel) is an example of a system that provides tooling around _holes_, allowing the developer to build the system partially, providing placeholders (stubs) in areas which need further development while still allowing the system to execute the implemented parts. Surely, there's something to learn here and implement for Idris!
* Being a practitioner and strong proponent of [Pair](https://www.jamesshore.com/Agile-Book/pair_programming.html) and [Mob Programming](https://en.wikipedia.org/wiki/Mob_programming), I wish our tools did not suck that much when it comes to sharing with fellow developers! I would like to spend some time building a _Distributed Development Environment_ based on something like Idris to let 2 or more developers collaborate in real time on the development of a system.

## Online Games

* I have implemented most of the rules of a [wargame](https://github.com/abailly/hsgames/tree/master/bautzen1945) in Idris (1 and 2), but have been stuck with turning this kernel into a usable software. I would like to build a UI in Elm that would allow multiple users and multiple simultaneous games, but the support for multi-threading is somewhat lacking in both versions of Idris.
* I had hopes I could leverage Idris2's underlying Scheme runtime to provide the heavylifting for concurrent channels but unfortunately, the game does not even run on Idris2, probably because of the lack of optimisation in various low-level numeric operations on `Nat`s.
* This lead me to the conclusion that having an optimised low-level backend for Idris2 would be useful. [LLVM](https://llvm.org/) is the way to go for such an endeavour!

# Testing

## Property- and Model-Based Testing

* [QuickCheck](https://hackage.haskell.org/package/QuickCheck) (along with similar packages for languages other than Haskell) is a fantastic tool when our types come short of providing us the necessary confidence in our implementation. My former colleague [Stevan Andjelkovic](https://bobkonf.de/2018/andjelkovic.html) has created [quickcheck-state-machines](https://github.com/advancedtelematic/quickcheck-state-machine/) which provides a great implementation of state-machine based testing, allowing one to generate tests from a more abstract automata-like _model_ of a software.
* An idea that is dear to my heart and which I have awkwardly promoted as [Homomorphic Event Sourcing](https://github.com/aleryo/homomorphic-event-sourcing) is that a single Input/Output State Machine based model could be used both as a _tester_ and as a _mock_, providing an executable specification to securely develop clients and servers in parallel.
* But once we have FSM models, why not bite the bullet and turn those into proper types, like [Session types](http://simonjf.com/2016/05/28/session-type-implementations.html)?
* I definitely want to keep investigating that field, possibly building on QSM package to provide better mocking

## FitNesse & ATDD/BDD

* [FitNesse](http://fitnesse.org) has been my favorite BDD/ATDD tool for years, but I must admit it is definitely showing signs of its age: Running a shared instance is somewhat painful to maintain, Wiki syntax is "non-standard" (not there is such a thing as standard Wiki syntax but still...), user experience is terrible when compared to things like http://notion.so ... Maybe the time is ripe for starting afresh and create a more modern version of it?
* What if we could combine models, examples, documentation and tests execution within a single tool? This idea is related to the _developers experience_ I have already mentioned, and hints at tools like [Jupyter](https://jupyter.org/) notebooks and Bret Victor's [Ladder of Abstraction](http://worrydream.com/LadderOfAbstraction/)
* Maybe the Wiki idea is not so much a useful tool than a barrier to entry for newcomers, esp. non-developers who might be reluctant to code the formatting of a page when they are used to rich UIs? FitNesse is an evolution of [FIT](http://fit.c2.com/) which initially interpreted _spreadsheets_. Given the ubiquity of online spreadsheet tools, I think getting back to a simple tool for interpreting and executing spreadsheets would be worth a try.

# Various Software-related stuff

* With [Frédéric Merizen](https://www.linkedin.com/in/fredericmerizen/?originalSubdomain=fr) I have given in November a trial session of a [Functional Architecture]() course inspired by [Michael Sperber](https://www.deinprogramm.de/sperber/) and [Nicole Rauch](https://nicolerauch.de/) own [course in German](https://www.isaqb.org/wp-content/uploads/2018/09/isaqb-Lehrplan-advanced-FUNAR_1.0.pdf). We plan to build on that experience to provide a commercial version of that training, possibly in several cities in France.
* Having worked for almost 3 years at [Symbiont](https://symbiont.io) on building a so-called _Smart Contracts_ platform, and more precisely a distributed and decentralised application to manage loans on top of this platform, gave me a new perspective on the potential of _Blockchain_ (aka. _Distributed Ledger Technology_) for developing distributed systems. I believe a generalised _Event Sourcing_ approach as exemplified in the [One Log](https://github.com/aleryo/one-log) talk we gave with Yann Schwarz at [Codemesh](https://www.youtube.com/watch?v=zKtCsODXjbw) and [NCrafts Bordeaux](https://bordeaux.ncrafts.io/2018/) in 2018, is a great way to design and build software system on such platforms.
* I would like to explore how public and/or open-source "blockchain" platforms can support large and complex decentralised applications dedicated to some specific domains like finance, supply chain or even e-government.
* At the theoretical level, I definitely need to improve my knowledge of fundamental principles of decentralised systems, how to model them, how to reason about them, how to verify them... I wonder if [Asynchronous automata](http://www.numdam.org/article/ITA_1987__21_2_99_0.pdf) could not provided some interesting foundations for reasoning about such kind of systems.

# Non-software

* Since I have read Merleau-Ponty's [Phénoménologie de la perception](http://abailly.github.io/posts/pheno-perception.html), I have been fascinated by _Phenomenology_: What is it, how does it "work", what insights does it provide... I have started reading Husserl's [Idées directrices pour une phénoménologie pure et une philosophie phénoménologique](http://www.gallimard.fr/Catalogue/GALLIMARD/Bibliotheque-de-Philosophie/Idees-directrices-pour-une-phenomenologie-pure-et-une-philosophie-phenomenologique) in the hope to gather some insights on things like _epoché_ or the _phenomenological reduction_ process.
* I think such tools could be useful as a way to approach software differently, maybe complementing the [therapeutical approach](http://abailly.github.io/posts/therapeutique-du-code.html) I advocated a couple years ago: Analysing software systems as pure _phenomena_, in the way they _appear_ to the people that work on them or with them, removing the cruft of judgment, prejudices and builtin assumptions about them ; tracking the _intentionality_ behind the relations we have with software intensive systems.
* I recently read Robin K. Hill's [What an algorithm is?](https://link.springer.com/article/10.1007/s13347-014-0184-5) and this looks like the kind of analysis I am trying to develop.
* I have also started reading Piketty's [Capitalisme et idéologie](https://www.seuil.com/ouvrage/capital-et-ideologie-thomas-piketty/9782021338041) which is a "Sequel" to his extraordinary successful [Le capital au XXIème siècle](http://www.seuil.com/ouvrage/le-capital-au-xxie-siecle-thomas-piketty/9782021082289). Piketty is one of the few modern-day thinkers that are both optimistic and realistic, providing concrete and actionable - even if counter-intuitive or controversial - solutions to make a better world.
* In the same vein, I recently discovered the existence of [Modern Monetary Theory](https://en.wikipedia.org/wiki/Modern_Monetary_Theory) which seems to be a form of Neo-Keynesianism, an heterodox macroeconomic theory that strives to counter neo-liberalist economic theory and restrictive monetary policies that are the heart of our world's growing inequalities. I look forward to read the book [Macroeconomics](https://www.amazon.fr/gp/product/1137610662/ref=ppx_yo_dt_b_asin_title_o00_s00?ie=UTF8&psc=1) and form a better understanding of what _MMT_ is and how to rebuild a credible Left alternative.

# Conclusion

That's obviously a ridiculously too high number of complex topics for a single person, in a single year. Especially taking into account I also plan to earn some money with a day job that might not fit perfectly with any of those projects, spend more time with my family, read books just for the pleasure of reading books, run and workout... Let's see what 2020 has to offer, I will be happy if I can make some decent progress on one or two of those topics!
