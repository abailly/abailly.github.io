---
title: Tester un système réparti avec Docker et Haskell
author: Arnaud Bailly - @dr_c0d3
email: arnaud.bailly@symbiont.io
date: 2018-07-05
theme: serif-black-symbiont
---

# Agenda

--------

* Introduction
* System Overview & Motivation
* Docker & Testing
* Demo
* Conclusion

# Introduction

## Who am I?

![](/images/jack-of-all-trades.jpg)

<div class="notes">
* Developing software since 1994
* Now busy coding in Haskell at [Symbiont](https://symbiont.io)
* Test Obsessed
* Using docker in development and production since 2014
</div>

## Symbiont

![](/images/symbiont-web.png)

<div class="notes">
* New-York based fintech startup building a *Smart Contracts* platform
* Provide solutions to build more efficient decentralised markets
* Tech stack based on  *private blockchain* technology
* **We are hiring**
</div>

## Goals

* **Goal**: Share experience return, ideas, tips on how to use docker to test a (complex) distributed system
* Hope to raise tough questions and improve state of practice to develop such kind of systems
* **Feedback** most welcomed
* This is a _Work-in-Progress_...

# System Overview

## Symbiont's System

![](/images/symbiont-node-arch.png)

## Core Components

* A *Byzantine Fault Tolerant Replicated Log* stores cryptographically signed and encrypted transactions
* A *Contract Execution Engine* stores contract's code and allow contract's functions execution
* A *Transaction Engine* manages identities and privacy of transactions across time

## BFT Distributed Log

* Ensures that all nodes share a consistent view of the system by linearizing transactions history
* Implements [BFT-SMaRt](http://www.di.fc.ul.pt/~bessani/publications/dsn14-bftsmart.pdf) protocol which is an extension of [Paxos](https://www.zurich.ibm.com/~cca/papers/pax.pdf) to support _Byzantine failures_
* *Failures mode*:
  * Non-byzantine: Node failures, network outages, network slowness...
  * Byzantine: Arbitrary messages mangling, Node identities "stealing"...

<div class="notes">
* see Distributed Algorithms, N.Lynch, pp.99-102
* Stop failures means processes can stop working but otherwise abide by their specifications
* Byzantine failures model means processes can change state and output messages arbitrarily
</div>

## Transaction Execution Engine

* Controls execution of _Smart Contracts_ by dispatching incoming _Transactions_ from distributed log
* Basically a _State Machine_ executor with provision for respecting confidentiality of transactions
* Stores state in a local database
* *Failures mode*: Connected services failures, network outages, slowness, flickering, DB corruption...

## Deployment

* All components of the system are packed as _docker containers_
* Deployment through Kubernetes over various cloud platforms (AWS, GCP, Azure)
* A *Node* contains complete stack of services deployed together, several nodes form a *private network*

## Constraints

* **Safety**: System should tolerate arbitrary failures
* **Consistency**: Transactions log should be identical to all client in order to ensure deterministic state
* **Security**: System should enforce privacy and secrecy of transactions
* Lies in the *CP* corner of CAP triangle

# Testing

## Goals

* Provide an _easy_ way to write system-level tests for use in development and testing
* Abstract away _details_ of deployment
* Build complex models of system's behavior to generate _interesting_ test cases for asserting _correctness_ of the system in front of _faults_ => allow injecting arbitrary faults
* Fine control over system's communication to generate _Byzantine failures_

## Inspiration: [Jepsen](https://aphyr.com/tags/jepsen) ##

![](/images/call-me-maybe.jpg)

<div class="notes">
* Black-box testing tool
* Developed by Kyle Kingsbury aka. [aphyr](https://twitter.com/aphyr)
* A tool for checking consistency properties of distributed databases, lead to a series of post called Call Me Maybe
* verifies *linearizability* of execution traces
* Found a good number of flaws in various high-profile open-source and closed sources DBs, including ES, ZK, Mongo, Riak, Redis...
</div>

## Testing Symbiont System

* Deploy all or part of the system as containers in (local) docker
* Write test scenarios exerting API of _System-Under-Test_
* (_Optional_) Inject networking and system level faults with _Blockade_
* Check consistency of state of the system w/ simple properties

## Injecting Faults w/ Blockade

![](/images/blockade.jpg)

------


* A python-based tool for injecting faults in a _docker network_
* Works as a command-line utility that manipulates iptables of containers and reacts on docker engine events to control system
* Provide commands to set a given network in some defined state:
    * `slow`: Add random delay to deliver packets to some node(s)
    * `flaky`: Drop randomly packets to/from some node
    * `partition`: Break connections between nodes

## Domain Specific Language

![](/images/rosetta-stone.jpg)

<div class="notes">
* Tests are described with a custom Domain-specific language in Haskell
* Exposes high-level ops available to setup/run/teardown the system
* DSL is abstract e.g. not tied to a particular infrastructure => can be interpreted in different ways
    * local, with processes from the host
    * docker
    * docker-in-docker -> requires special handling of network
    * fully remote: Running tests against a production-like infrastructure
    * simulation
</div>

## Writing Tests

~~~~ {.haskell}
test
  :: Int -> Test e ()

test numNodes = do
  when (numNodes < 5) $
    exit "Split brain test should be run with at least 5 nodes"
  (adminKey, procs) <- setup "assembly" numNodes
  let mid = numNodes `div` 2
  run1 <- mconcat <$>
         feedTransactions 1  (10 ///) (batchOf 10) (seconds 10)
  nemesis $ Partition [[ 1 .. mid - 1 ], [ mid .. numNodes ]]
~~~~

-----

* Tests are written in Haskell and compiled to an executable => Provides strong typesafety
* Test fragments can also be executed from GHCi for rapid prototyping and easy interaction with the system
* contains operations for SUT *and* Nemesis actions (named inspired by Jepsen)
* Tests are run *sequentially*

## Running Tests

* Test executable is packaged as a container and pushed to a repository
* Allows executing tests within  _Continuous Integration_ environment and tagging specific versions

-----

![](/images/symbiont-circle-ci-assembly-sym-test.png)

## Beyond Testing

* Provides a **fast** and **easy** way to _deploy_ a whole platform with an arbitrary number of nodes and topology
* Useful for local development:
    * Link a locally built and running component to a network of docker containers
    * Use network to code and test smart contracts and high-level services
* Slowly building the language of _Systems Operations_ for our platform

# Demo

# Conclusion

## Takeaways

* Docker is tremendously useful as an environment for system-level testing
* Bridges the gap between _Dev_ and _Ops_ and fosters the advent of true _DevOps_
* There are lots of tools and utilities out there to help you doing that
* There is great value in building a test DSL that reflects the way the system works and Haskell rocks as a tool to define such kind of DSLs
* _This is just the beginning of the journey_

## What's next?

![](/images/chartres-cathedral.jpg)

<div class="notes">
* Lot of work on covering more aspects of the system
* Provide an external parser to make it easier to write and modify tests
* Complete support of remote infrastructure
* Parallelization of tests execution
* ...
* => make it easier and faster to use it
</div>

## What's next? Model Based Testing!

![](/images/magritte-pipe.jpg)

<div class="notes">
* Based on formal/executable models of the SUT
* Requires instrumentation of some sort of the processes run
* Similar to concurrent programs testing: [dejafu](https://github.com/barrucadu/dejafu)
* Still an area of active research...
</div>

## Questions?

![](/images/puzzled.jpg)
