------------
title: Programming is not Reading
author: Arnaud Bailly
date: 2017-05-19
------------

* Laura Salvino's talk at ncrafts drawing on the analogy between learning a new (human) language and learning a programming language
* emphasizing reading software -> simplicity, patterns, focusing on readers
* do we really "want readable code" ? Reading code and making your code readable is good, and probably better than writing confusing, idiosyncratic, symbols-ladden code, but emphasizing readibility misses the point of software
* Analogies are useful tools to think but we should be wary about carrying them too far: Programming languages are not human languages and their inner logic is totally different. A programming language is not a passive thing, it is active
    * I remember a tech leader in a team I worked with that each day printed the code written by the team and carried it home for review
    * What's a the usefulness of online code review tools? Does anybody do serious reviews using Github diffs? It is much more useful to have the code locally, making sure it compiles (on *my* machine) and I can play with it
    * We spend a lot of times trying to find *the* right name for a function, a class, a method, a concept...
    * We refactor, rename, use complex patterns in names to ensure people will understand us
* Programs is mostly useful for execution: I don't care a program is perfectly readable if it is incorrect. I would rather have a program which is awkwardly written, with terse variable names but is working correctly and "as expected". To really understand a program means executing it which can be done in your head but is much more efficiently done with a REPL/Compiler/IDE
* Words and sentences are not checkable by a computer which makes their usefulness as parts of a program limited. Comments are a major PITA...
* [Living Documentation]() is hard
* See [this post](http://akkartik.name/post/literate-programming) about Literate Programming: LP is a great idea that did not came to maturity because it has been stuck into primitive state and with a focus on *reading* instead of *running*
* Related to a lot of work going on in the [notebooks space](http://nbviewer.jupyter.org/), innovative and/or extensible IDEs and [Languages](https://awelonblue.wordpress.com/2014/09/29/introducing-wikilon/), [interactive](http://worrydream.com/#!2/LadderOfAbstraction) web [environments](http://tomasp.net/coeffects/)
* Also noteworthy is the work on [Type-Driven Development]() and programming s a _refinement process_ or _dialog_ between humans and computers, like in the paper [Live functional programming with typed holes](https://arxiv.org/pdf/1805.00155) and the [Hazel](https://hazel.org/) programming environment
* [Richard Gabriel](https://www.dreamsongs.com/Files/Incommensurability.pdf) we moved from a paradigm of _programming systems_ as in LISP, in which "programming" is a verb, an action ; to a paradigm of _programming languages_ in which "programming" is an adjective, a description of the kind of languages we use. In other words, we have substituted a static _specification_ of what to do to an active _operation_ of doing something.
