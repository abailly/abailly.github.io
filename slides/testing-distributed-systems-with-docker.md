------------
title: Distributed Systems Testing
subtitle: How I Learned to Stop Worrying and Love Docker
author: Arnaud Bailly - @dr_c0d3
email: arnaud.bailly@symbiont.io
date: 2017-09-19
theme: serif-black
------------ 

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

* Critical piece of the system, ensures that all nodes share a consistent view of the system by linearizing transactions history
* Implements [BFT-SMaRt](http://www.di.fc.ul.pt/~bessani/publications/dsn14-bftsmart.pdf) consensus protocol which is a variation over [Paxos](https://www.zurich.ibm.com/~cca/papers/pax.pdf)
* Non-byzantine failures: Node failures, network outages, network slowness...
* Byzantine Failures: Arbitrary messages mangling, Node identities "stealing"...

<div class="notes">
* see Distributed Algorithms, N.Lynch, pp.99-102 
* Stop failures means processes can stop working but otherwise abide by their specifications
* Byzantine failures model means processes can change state and output messages arbitrarily
</div>

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

## [Jepsen](https://aphyr.com/tags/jepsen) ##

![](/images/call-me-maybe.jpg) 

<div class="notes">
* Black-box testing tool 
* Developed by Kyle Kingsbury aka. [aphyr](https://twitter.com/aphyr) 
* A tool for checking consistency properties of distributed databases, lead to a series of post called Call Me Maybe
* verifies *linearizability* of execution traces 
* Found a good number of flaws in various high-profile open-source and closed sources DBs, including ES, ZK, Mongo, Riak, Redis...
</div>

## Jepsen - How it works?

* Define a (simple) model of shared object (mutex, CAS...)
* Deploy system over an actual cluster of machines
* Generate (semi-)random sequence of operations representing concurrent clients interactions with the system
* Generate *partitions* to trigger corner-cases and observe behaviour of system when network fails
* Analyze resulting execution trace to detect violations of *model's properties* 

<div class="notes">
* Various *consistency* properties
* Knossos checks *linearizability* of operations: all operations appear to occur _instantaneously_ at some point in time while respecting program order
</div>

## Jepsen is complex

![](/images/gasworks.jpg) 

<div class="notes">
* We are not that much interested in testing linearizability
* Requires developing driver in clojure to interact with system, deploy it, run it
* Designed to run against actual cluster/VMs, not containers
* Requires expertise to design, run and analyze tests
* But lots of cool ideas and great piece of work! => We are thinking of hiring Kyle Kingsbury in the future to apply his expertise on our system!
</div>

## Testing Symbiont System

![](/images/jules.jpg)

<div class="notes">
* Deploy all or part of the system as containers in a local docker
* Write test scenarios exerting API of _System-Under-Test_
* Inject networking and system level faults with _Blockade_
* Check consistency of state of the system w/ simple properties
</div>

## Blockade

![](/images/blockade.jpg)

<div class="notes">
* A python-based tool for injecting faults in a _docker network_
* Works as a command-line utility that manipulates iptables of containers and reacts on docker engine events to control system
</div>

## Blockade

![](/images/blockade-network.png)

<div class="notes">
* Uses `iptable`/`tc` tools to modify behaviour of containers' network interfaces
* Works `exec`uting commands inside containers => requires availability of software packages
* Provide commands to set a given network in some defined state: 
    * `slow`: Add random delay to deliver packets to some node(s)
    * `flaky`: Drop randomly packets to/from some node
    * `partition`: Break connections between nodes
</div>

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

<div class="notes">
* tests are written in Haskell => compiled to an executable
* test fragments can also be executed from the REPL => rapid prototyping and easy interaction with the system
* contains operations for SUT *and* Nemesis actions (named inspired by Jepsen) 
* Tests are run *sequentially*
</div>

## Running Tests w/ Docker

* Two docker modes provided:
    * `docker` runs the test executable from the host environment
    * `dind` runs the test executable from within docker
* Test executable is packaged as a container and pushed to repository
* Allows executing tests within  _Continuous Integration_ environment and tagging specific versions

## Tests in Continuous Integration ##

![](/images/symbiont-circle-ci-assembly-sym-test.png)

<div class="notes">
* Tests are run in CI as part of distributed log's build 
* Deployed on a custom GCP instance 
* Run in dind mode from a known image tag
</div>

# Demo 

# Conclusion

## Takeaways

* Docker is tremendously useful as an environment for system-level testing
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
