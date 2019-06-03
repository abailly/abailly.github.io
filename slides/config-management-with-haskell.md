------------
title: Infrastructure-As-Haskell-Code
subtitle: Experiments and musing with Propellor and DepTrack
author: Arnaud Bailly - @dr_c0d3
date: 2017-04-27
theme: serif-compact
------------


# Agenda

--------

* Introduction
* Propellor
* DepTrack
* Conclusion

# Introduction

## Who am I?

* Software developer since quite a while, not a *SysAdmin* expert
* Started working on this topic while developing [Capital Match](https://capital-match.com) platform
* Believes in the importance of *Infrastructure-as-Code*
* Currently working as [Dr.Code](http://drcode.io)

## Why Haskell?

* I don't know Chef nor Puppet, so I needed to learn something anyway
* I feel more comfortable when I code with something typed
* I wanted to have as much as possible Haskell to minimize zoo-effect

# Propellor

## Demo - Setting up a multi-host docker network

## Background

* Developed by [Joey Hess](http://propellor.branchable.com), an historical Debian contributor
* Development is very active, current version is 4.1 and releases are very frequent
* Large monolithic package (>100 source files), contributions friendly but not on Github :(
* Targeted mostly towards personal or SME-scale infrastructure with long-running machines

## Basic Execution Flow

1. Clone the code or run wrapper to get code in `~/.propellor`
1. Hack! Hack! Hack!
2. Run `propellor <target host>`
3. Propellor takes care of the next steps:
    4. Build executable
    5. Commit changes to repository
    6. Upload code to target host
    7. *Build executable on target host*
    7. Run executable on remote host, reporting results

## Security Model

* **Key Question**: How not to disseminate confidential data (passwords, API tokens, keys, names...) while still allowing its use?
* Propellor uses GPG to manage a set of public keys and a `privdata` file which is encrypted using *public keys* of all participants
* `privdata` file is *committed* to the code repository
* When a user needs to deploy, she needs to provide credentials to decrypt private data that is then sent to remote host

## Advanced Features

* Optimises (removes) compilation of executable on remote host when source and target are binary compatible
* Provides proxy-based remote configuration allowing a single entry point to configure multiple targets
* Type-level checking of properties's OS scope
* Revertable properties

## Core Types

----

```haskell
data Property metatypes
  = Property metatypes
             Desc
             (Propellor Propellor.Base.Result)
             Info
             [Propellor.Types.Core.ChildProperty]
```

----

```haskell
newtype Propellor p
  = Propellor {runWithHost :: Control.Monad.Trans.RWS.Strict.RWST
                                Host [Propellor.Types.Core.EndAction] () IO p}
instance Monad Propellor
instance Functor Propellor
instance Applicative Propellor
instance Monoid (Propellor Propellor.Base.Result)
```

----

```haskell
-- | There can be three results of satisfying a Property.
data Result = NoChange | MadeChange | FailedChange
	deriving (Read, Show, Eq)
```

# DepTrack

## Demo - Configuring a Spark cluster from scratch

## Background

* Developed by [Lucas di Cioccio](https://github.com/lucasdicioccio/deptrack-project) and recently open-sourced
* Small codebase split in multiple "independent" packages to minimise footprint: Pay for what you need
* Targets larger-scale infrastructure and *supervision* rather than only configuration
* New and shiny!

## Basic Principles

* A (monadic) language to describe *directed acyclic graph* of *operations* linked through *dependencies*
* Generated graph provides *optimisations* opportunities (e.g. group package installation, remove redundancies) and inherent *parallelism*
* Traverse graph in dependency order to concurrently run *functions* in each node, passing along results from each node's execution as needed
* Each node provides basic supervision functions start/stop/check

## Types

-----

```haskell
type DepTrackT a m b = WriterT (DList (DepCrumb a)) m b

data DepCrumb a = Push | Pop a | SpadeIn | SpadeMiddle | SpadeOut
```

----

```haskell
type DevOpT m a = DepTrackT PreOp m a

data PreOp = forall a. Typeable a => PreOp !a !(a -> Op)

data Op = Op { opDescription :: !OpDescription
             , opFunctions   :: !OpFunctions
             , opUniqueId    :: !OpUniqueId
             }
```

## Advanced Features

* Uses *static pointers* to provide remote invocation capability with safety guarantees from GHC
* Concurrent asynchronous turnup/upkeep/turndown that automatically optimises graph's execution
* Extensible optimisations and flexible declarative model
* Simple cross-platform execution tooling for heterogeneous infrastructure management

# Conclusion

## Propellor - Pros

* Large set of functions covering whole spectrum of configuration tasks
* Track record, user base and contributors base
* Built-in interaction with Git to provide automatic configuration changes execution
* Security model

## Propellor - Cons

* Requires haskell toolchain on target hosts
* Rigid workflow with automatic builds and commits
* Big monolithic codebase which puts a heavy toll on build/run cycle
* "Static" model

## DepTrack - Pros

* Clean model gracefully handling *dependencies* between tasks in an expressive and composable way
* No requirements about specific properties of targeted infrastructure
* Enforces separation of concerns between *declaring* configuration and *running* management tasks
* Provides supervision of *nodes*
* Type-safe remote execution model

## DepTrack - Cons

* New and not much battle tested: limited user and contributors base, documentation, large-scale testing...
* Requires more work from user as most system-level tasks are not included in the codebase

## Conclusion

* **Infrastructure-As-Haskell-Code** is for real!
* Go to [Lucas di Cioccio](https://github.com/lucasdicioccio/deptrack-project): Feedback most needed, PRs welcomed!
