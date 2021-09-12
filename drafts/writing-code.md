------------
title: How I (Try to) Develop Software
author: Arnaud Bailly
date: 2017-10-26
------------

This article is a hopefully short and to the point overview of my current state of practices when writing code, which those days is mostly Haskell.

# Tools

## Hardware

I write 100% of my code on the following machine:

> Model Name:	MacBook Pro
> Model Identifier:	MacBookPro13,2
> Processor Name:	Intel Core i7
> Processor Speed:	3,3 GHz
> Number of Processors:	1
> Total Number of Cores:	2
> L2 Cache (per Core):	256 KB
> L3 Cache:	4 MB
> Memory:	16 GB

Because I spend most of my time writing in globish, whether in code or articles like this one, I have a QWERTY keyboard with a US International layout with dead keys that allows to type accented characters.

My previous machine was also a MacBook Air and I have worked on 13'' laptop for the past 15 years. The main rationale behind this choice is that I am used to move between different places to work whether it be several offices, home or during long commutes by train. Having a small and easily transportable machine was a strong requirement. Now that I work most of the time from home, the habit of working at different places sticked and I keep moving with my laptop from the sofa, to a desk, to the kitchen's bar.

I don't own another monitor and when I worked at locations where I had one, I never managed to get used to it. Because I have a small screen (and because as I get older, I cannot decrease the size of the fonts too much) I need to make sure the code I am working on fits in a small number of lines, e.g. something like 30 lines given my current editor's configuration which implies I tend to write compact code which might sometimes come in tension with expressiveness. Note that while I write mostly "backend" code right now I used to write front-end code in some of my past jobs and the layout I used was identical.

I have been pondering for a few years now the opportunity to move most of my development to the cloud, spinning VMs with all the need tools on demand and working through SSH. That's the setting I use when pairing remotely but I did not made the step yet...

## Software

* I am mostly happy with MacOS so far: It does the job to manage GUIs and is very stable: I can't remember having had an OS crash,
* I of course use [homebrew](https://brew.sh/) as a package manager for everything which is not packaged as a proper MacOS app, e.g. most Unix-like tools,
* I use [docker for Mac](https://docs.docker.com/docker-for-mac/) a lot: Most of the software I developed since 2014 has been packaged as docker images and it is extremely useful for testing (more on this later),
* My shell is plain [bash](https://www.gnu.org/software/bash/) in [iTerm2](https://www.iterm2.com/) and I have a small set of [dotfiles](https://github.com/abailly/dotfiles) which are updated infrequently. I don't spend much time customizing my environment: I agree with the general idea that _a good craftsperson chooses the best tools_  and those tools need to be _honed_ but time is at a premium and I prefer to spend it reading articles and books, thinking about design problems, discussing with fellow developers and coding rather than fiddling with tools,

My editor of choice is [Emacs](https://www.gnu.org/software/emacs/download.html#macos): I chose it when I got back to university at the turn of the millenium and never looked back. I can edit files with vim and I even tried to use [spacemacs](http://spacemacs.org/) for a couple of months but it did not stick. Just like my shell's my emacs config is pretty basic and I probably use 5% of its power, but I am happy with it.

I write Haskell code on Emacs using [intero](http://commercialhaskell.github.io/intero/). I used to have a more or less complex a `haskell-mode` configuration adapted from various sources on the internet but intero works just fine, if a bit slowly. It provides syntax coloring, some form of autocompletion, `TAGS` file handling, type information, REPL... Intero leverages stack and allows one to select the targets to load within a given project's context which is handy when working on a large-ish codebase to select only a subset of the packages. The major shortcomings with intero is that:

* Inline errors feedback is slow,
* It stops working as soon as the file cannot be parsed.

# Practices

When writing code, I try to adhere to the *Test-Driven Development* discipline:

* Write a failing test: This is usually a _unit test_ but from time to time can be a higher level test, whether an _acceptance test_ or a _system test_ when working on a larger feature,
* Make the test fail: This implies the test should compile, which usually leads to writing some code like basic types definitions and functions skeletons,
* Write needed code to make the test pass: Depending on the code and the situation, this step can be more or less large,
* Commit code to version control,
* Refactor if needed: This usually happens after a few tests have been written,
* Rinse and repeat,
* Integrate into _trunk_.

This basic loop can be repeated several times a day depending on the features I am working on. Here are a few more details on each of those steps.

## Test Driven Development

I have become accustomed to write tests as part of my development loop and usually that's the first thing I am tempted to do. When working in Haskell I can run my TDD feedback loop in 2 different ways:

1. Use `stack test my-projet --file-watch` from the command-line in order to have a continuous feedback loop running in the background,
2. Use the REPL:
    3. (Re)Load the test file with `C-c C-l` or `:r` from the REPL,
    4. Run the tests using `hspec spec`

Writing tests brings me the following benefits:

* It allows me to design the code: Because I need to write code that must compile and execute properly, the design must be explicit and more importantly _testable_ by construction,
* It makes refactoring easy: When you have tests that fail if behaviour of the code departs from expected outcome, refactoring is never an issue,
* It makes sharing the code easy: Having a test harness in place, anyone in the team can start poking at my code, suggest improvements and refactorings, without having to spend a lot of time studying it and fearing to break something,
* It provides a _cadence_: I find it very easy to get jump into a rabbit's hole and get side-tracked to solve technical problems remotely relevant to the task at hand, writing a test gives a clear and unambiguous goal whose completion provides a warm feeling of achievement,

In some circumstances, e.g. exploring a new idea, writing a spike, I write code without writing tests. If the code is not thrown away is always ends up being a bad idea.

### Haskell TDD Specificities

Types

: Because of its very expressive type system and its purity, I need to write fewer tests in Haskell than I would normally have to in another language, say Java, without significant boilerplate. As a very basic example, if I have a function `foo :: Natural -> Natural -> Bool`  then I obviously don't need to have tests for negative numbers. It is very easy in Haskell to create domain-specific types so I try to use and abuse this,

Compiler

: Haskell's compiler (GHC) checks a lot of potential sources of errors that I would normally have to test:

    * Function's totality: When pattern matching on input GHC checks all _constructors_ are matched by at least one clause. While this is not a full-blown totality proof and is rather coarse grained it still provides assurance no case is missed hence removes the need to have explicit tests for that,
    * Typechecking: Combining precice domain-specific types with strong typechecking (and no cast!) gives a lot of guarantees about the correctness of the code. We don't have to guard and test against potential invalid inputs,

Property-Based Testing

: QuickCheck-like tools which allows one to express properties about the code and generate random inputs to check those properties are much more commmonplace and usable in Haskell than in other languages. It is however not always easy to find relevant properties which are not as complex to express as the code they check. Some common usage patterns I have found are:

    * Serialization: It's trivial to write a property expressing encoding and decoding are isomorphic:

        ~~~~~ {.haskell}
        decode . encode x == x
        ~~~~~~

        although writing this code can be complex when interacting with external software
    * Oracles: Checking a fast but complex implementation behaves identically to a slower but simpler one

Mocking

: Because we have a first-class functions and polymorphic data types, we don't need any specialized framework or tool to use [mocks]() in Haskell. This makes it easy to identify _seams_ in the design and write tests and functions accordingly. For example, if I have a function that needs to run a query against a database, I can easily pass the query execution _action_ as a function resulting in some monadic action and provide a mock implementation for unit testing purpose. When the codebase grows, those functions can be grouped into a proper typeclass.

### Unit Tests vs. Integration Tests vs. Acceptance Tests vs. ...


## Debugging

I view debugging as a failure: When I have to debug my code, this means there is something I don't understand about it which is a thought I find disturbing. Of course, the code I have written can fail in the field for a lot of reasons:

* I misunderstood how to use the result of a thirdparty library's function call,
* I am talking to the system and did not properly check for failures or exceptional conditions,
* I implemented flawed logic w.r.t requirements and actual usage,
* ...

All these reasons are amenable to unit testing so my first reflex when I encounter a bug in the field is to write a proper test exposing the bug. Writing this test _might_ require some thorough analysis which _might_ require debugging but this should be exceptional and properly analysed to prevent it. When I really need to _debug_ code, I seldom ever use GHCi's debugger and usually resort to good old `printf`: Insert `Debug.Trace.trace` statements until I find the source of the problem.

## Team Work

Being a fallible human I know that when left alone my natural laziness quickly takes over and I become tempted to code recklessly, foregoing all those nice principles and practices. Moreover there's obviously more brain power in two minds than in one and `

## Version Control

* Small commits, try to write meaningful log messages
* Use issue tracking number in commits for linking
* Work on main branch (see Continuous integration) to minimize time to market: Software should be put in the hand of end users as fast as possible
* Favor a single line of history rather than complex branch/merge graphs: Makes it easier to identify which commit introduced some issues, make bisect useful
* Merging breeds bugs, even with proper unit test coverage -> remove the need to merge

## Refactoring & Continuous Integration

* Fix defects as soon as they are spotted rather than having to wait better times and suffer from long and painful refactorings
* Haskell's type system coupled with proper TDD makes refactoring a breeze: Change what you need to then fix compilation and testing errors and you're done
* [Refactor Mercilessly](http://www.extremeprogramming.org/rules/refactor.html) and _continously_
* Requires proper _continuous integration_: Integrates small batches of changes to the system, check continously everything keeps working
    * _Trunk Based Development_ (See Version Control)
* Continuous Integration = software should be production ready 99% of the time => test harness is here to validate integration is fine

## Delivering & Validating Software

* Manual testing is important => make it easy to do so, provide staging environment(s) with latest available version(s) and relevant data set, e.g. production-like environment
* Packaging, delivery, deployment are part of the software: Having tools for testing at the system-level is a consequence and a cause of having easy to use deployment tools, both should be developed jointly

## Production Readiness

* Pay attention to logs: Logs are part of the software's interface and they should be primarily usable for machine consumption, e.g. output in some structured format like JSON (not ideal but still better than free-form text...)
* Version numbers
* Status API/CLI tool: Pre-flight checks, current status, degrade gracefully, provide a `stop` command => make the system testable
