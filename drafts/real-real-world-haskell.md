------------
title: Real Real World Haskell
author: Arnaud Bailly 
date: 2016-03-18
------------

# Goal

* Share some experience developing a complete system in Haskell
* Motivates people to start using Haskell at work, provide some pointers to speed-up onboarding
* I will be happy if you end up thinking: *I could try this cool stuff in my next microservice*
* I will be even happier if you can put to actual use some of the stuff I present here

# The Language

## tl;dr

* Haskell syntax promotes terseness but is easy to pickup
* The type system is a great designing tool
* 

## Types ##

* Make types as precise as possible, no more string-based programming.
* `newtype`s provide cheap encapsulation of other types (they are unpacked by compiler)

    ```haskell
    newtype AccountNr = AccountNr { accountNr :: T.Text }
                        deriving (Eq,Ord,Show,Read)


    instance IsString AccountNr where
        fromString = AccountNr . T.pack
    ```
                       
* phantom types: provide annotation for common underlying representations

    ```haskell

    data Base64
    data Hex

    newtype Encoded code = Encoded { encodedText :: Text } deriving (Eq, Ord)

    toBase64Text :: ByteString -> Encoded Base64

    toHex :: ByteString -> Encoded Hex
    ```

* type families or "type-level functions": abstract over some type. 
  Comes in 2 flavors, one that allow defining a type as part of some data structure or typeclass. Here we define some types `Event a` and `Command a` which depend on the `a` the actual instance of `a` used.[^2]

    ```haskell
    class (ToJSON (Command a)) => BusinessModel a where
      data Event a   :: *
      data Command a :: *
    ```

    One for top-level functions:

    ```haskell
    type family Id a :: *
    type instance Id Account       = AccountId
    type instance Id Transaction   = TransactionId
    ```

* type classes: Defines interfaces (aka. modules, signatures) to some set of operations and data types
    * encapsulate implementation details, e.g. data storage, execution model
    * allows transparent replacement of 
* See [Type Driven Development](http://cfp.devoxx.fr/2016/talk/USZ-6984/TDD,_comme_dans_Type-Directed_Development) by Clément Delafargue

* missing first-class modules as provided by Caml

## More Types ##

* [Free](http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html)-[monad](http://underscore.io/blog/posts/2015/04/14/free-monads-are-simple.html) based Domain Specific Languages for exposing services
* Provides a strict yet composable abstraction for expressing *effects*. For example, one can define a language for working with DigitalOcean's droplets:

    ```haskell
    data DropletCommands a = ListDroplets ([Droplet] -> a)
                           | CreateDroplet BoxConfiguration (Result Droplet -> a)
                           | DestroyDroplet Id (Maybe String -> a)
                           deriving (Functor)

    type DropletCommandsT = FreeT DropletCommands
    ```

* [Cofree](https://github.com/dalaing/cofun) interpreters provide a way to decouple the language from the context it is interpreted in. Once you have a `Free` DSL to express some family of effects, you can *pair* it with a `Cofree` DSL expressing some lower-level operations for given DSL, e.g. expressing droplets operations in terms of network requests.

## Concurrency

* Mostly based on [Software Transactional Memory](https://www.schoolofhaskell.com/school/advanced-haskell/beautiful-concurrency) that provides tools for composing potentially concurrent access to shared variables
* STM do not mix with I/O operations hence you cannot use it to build critical sections with side-effects, e.g. to ensure persistence of modified data
* Enters [async](https://hackage.haskell.org/package/async) a high-level library to package asynchronous computations. Threads are cheap in Haskell.
* We use actor-like *queues* to protect access to I/O resources, e.g. database. Haskell provide a lot of useful abstractions on top of STM and core concurrency features to build your own tools without too much hassle

## The pain points

* Numeric types can be confusing: There is a whole hierarchy of typeclasses for various numeric values, from `Integral` to `Fractional` to `Real`...
* String types can be confusing: There is a whole zoo of types one can use as "strings", `String`, `Text`, `[Char]`, even `ByteString` or `Char8.ByteString`...
* Type system sometimes look like a straightjacket and might require boilerplate to achieve desired effect

# Development Process

## Compilation & Build

* **Caveat**> Compilation can take a long time!
* Standard package management tool is [Cabal](https://www.haskell.org/cabal/) which has become infamous due to [cabal hell](https://wiki.haskell.org/Cabal/Survival)
* [stack](http://docs.haskellstack.org/en/stable/README/) greatly improves thing in order to provide fully reproducible builds and sandboxed environments, down to compiler versions
* Always be sure to set ` -Wall -Werror` on compilation: Make all warnings errors, you can always selectively disable some warnings
* GHC provides options to get full stack traces in case of errors

## Development Cycle

* Use the REPL, Luke! Provides much faster feedback loop than full-blown compilation and building, can be used for Type/Test-DD too
* GHC has a cool feature to handle **holes**: variables which are not in scope but typechecked so that one can use that to deduce needed type and implementation
* My development cycle in Haskell usually boils down to:
    * Write a skeletal test file using [HSpec](http://hspec.github.io/), e.g. `test/FooTest.hs`
    * Start REPL in Emacs by loading file `C-c C-l`
    * See it fail to compile
    * `:r`eload until it compiles
    * Run the test and see it fail: `hspec myTest`
    * Fill in code until test passes
    * Do a full compile and test run before pushing to CI

## Development Environment

* One of Haskell's pain points: It's a moving target and there is no single solution
* Most advanced support is provided by integrating various extensions to Emacs to provide auto-completion, formatting, integration with REPL, auto-compilation and errors highlighting...
* There is a nice [tutorial](https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md) on how to setup emacs and Chris Done has provided some [standard configuration](https://github.com/chrisdone/emacs-haskell-config)
* [Haskell for Mac](http://haskellformac.com/) is a recent initiative to provide an interactive and easy to use programming environment in Haskell
* [IHaskell](https://github.com/gibiansky/IHaskell) provides a Haskell *kernel* for IPython notebooks for interactive programming. Provides a nice alternative to text-only development in Haskell esp. for number crunching
* [Spacemacs](http://spacemacs.org/) provides a viable alternative with "everything" preconfigured and packaged
* GHCi comes with a debugger but I have never used it. [Debugging sucks, testing rocks](http://googletesting.blogspot.fr/search/label/TotT)

## System Build

* [shake](http://shakebuild.com/) is a Make-like build system in Haskell
* Allows you to define *rules* and *targets*, manages dependencies to rebuild only needed part
* Build file is written in Haskell but can handle any kind of software
* We use it as our root build script to produce all our containers

```haskell
    "images/uibuild.uuid" *> \ uuidFile -> do
      recursiveNeed "ui" ["lib//*.cljs", "lib//*.clj", "main//*.cljs", "main//*.clj"]
      buildUiContainer uuidFile (cwd </> "ui")
```

# Web

* [WAI](https://github.com/yesodweb/wai/) provides all the needed components to build high performance REST-based services in Haskell
* Initial development was done with [scotty](https://github.com/scotty-web/scotty/) which is very lightweight and does not really take advantage of Haskell features
* Newer services are now developed with [Servant](https://github.com/haskell-servant/servant/): provides a way to express APIs **at the type level** thus ensuring correctness at compile time

    ```haskell
    type CreateJob = ReqBody '[JSON] Job :> Post '[JSON] JobId
    type ListJobs = Get '[JSON] [Job]
    type PreviewSchedule = QueryParam "count" Int :> ReqBody '[JSON] Schedule :> Post '[JSON] Calendar
    type RemoveJob = Capture "jobid" JobId :> Delete '[JSON] JobId

    type SchedulerApi = "api" :> "scheduler" :> "jobs" :> CreateJob
                   :<|> "api" :> "scheduler" :> "jobs" :> ListJobs
                   :<|> "api" :> "scheduler" :> "jobs" :> RemoveJob
                   :<|> "api" :> "scheduler" :> "schedule_preview" :> PreviewSchedule
                   -- recorder is just used for testing purpose
                   :<|> "api" :> "scheduler" :> "recorder" :> Capture "job" String :> ReqBody '[OctetStream] String :> Post '[JSON] ()
                   :<|> "api" :> "scheduler" :> "recorder" :> Capture "job" String :> Get '[JSON] String
    ```
                    
* Might be cool to have a look at [Spock](https://github.com/agrafix/Spock)

# Testing

## Testing tools

* [QuickCheck](https://hackage.haskell.org/package/QuickCheck-2.8.2) is your friend!
* No really, QuickCheck is your friend...
* Use it for defining formal properties of your code beyond what type system provides
* Also very useful for generating samples data as part of more standard tests
* Can even be used for effectful code: Generate scenarios, using [Monadic QuickCheck](https://hackage.haskell.org/package/QuickCheck-2.8.2/docs/Test-QuickCheck-Monadic.html)

* Test in depth, e.g. correct test pyramid, with a few ETE tests, more integration, lots of unit and property tests
* [hs-webdriver](https://github.com/kallisti-dev/hs-webdriver) allows you to write [Selenium](http://docs.seleniumhq.org/) tests in Haskell

## Beyond testing

* [hpc](https://wiki.haskell.org/Haskell_program_coverage) is built in GHC to generates test coverage. Using it is as simple as `stack test --with-coverage`... 
* Profiling gives you information on *cost centres* for some executions of the program. Its main inconvenient is that it requires building *all* dependencies with profiling support which effectively means you have to maintain two sets of dependencies. I only have to use it only once...
* [criterion](http://www.serpentine.com/criterion/) is the micro-benchmark library of choice for Haskell

# Deployment

## Packaging

* GHC produces native binaries with few dependencies on dynamically linked libraries
* It can produce 100% statically linked binaries if needed, but that's probably not a good idea anyway... 
    * https://www.reddit.com/r/haskell/comments/37m7q7/ghc_musl_easier_static_linking/
    * https://ro-che.info/articles/2015-10-26-static-linking-ghc
    * http://stackoverflow.com/questions/3430400/linux-static-linking-is-dead
* We package our services in d***er containers

## Configuration Management

* [propellor](http://propellor.branchable.com/) is a configuration management tool in Haskell developed by [Joey Hess](https://joeyh.name/) of Debian fame
* Configuration is expressed as a Haskell program that is compiled and run on the target host
* It provides a nice gpg key based model to encrypt private data (e.g. ssh keys, authentication tokens, passwords...) that need to be deployed in the source directory
* Allows expressing configuration items in a type-safe way -> leads to lots of tiny DSLs for various subsystems

## Infrastructure Management (WIP)

* [hdo](https://github.com/capital-match/hdo) is a client in Haskell for interacting with [Digital Ocean](https://www.digitalocean.com/)

# References

* [Worst practices should be hard](http://www.haskellforall.com/2016/04/worst-practices-should-be-hard.html) and everything from Gonzales' blog
* [Haskell in Production](http://www.shimweasel.com/hs_gbu/)
* Stephen Diehl's [What I wish I knew while learning Haskell](http://dev.stephendiehl.com/hask)[^1]

[^2]: This is how one can encapsulate concrete type definitions without first-class modules *à la* ML
