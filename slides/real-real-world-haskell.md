------------
title: The Real Real World of Haskell
subtitle: Relations on the Land of Pure Functional Programming by a Mortal Being
author: Arnaud Bailly
date: 2016-04-22
theme: old-english
------------ 

## Table of Contents

* How I Came to Reside in the Land of Haskell and Why It Matters
* On Various Aspects of Haskelland Civilisation and The Mores of Its Inhabitants
* Some Ways this Wonderful Journey Could Inspire the Reader

# How I Came to Visit Haskelland

-----------

## Where the author dares to present himself

![](/images/ebeniste.jpg)

## Where the author dares to present himself

* 20+ years of experience developing software...
* Has been coding in Haskell for side-projects since 2001
* Had the opportunity to join Capital Match as Co-founder and CTO in 2014
* Haskell was part of the plan since the beginning and the reason why I got involved
* I like theory but I prefere working software

## On the Company which I travelled to Haskelland for

![](/images/cm-website.png)

## On the Company which I travelled to Haskelland for

* Capital Match is the leading plaform in Singapore for peer-to-peer lending to SMEs
* Backend system developed in Haskell, frontend in Clojurescript/Om since 2014
* Core Development team of 4 persons

## Where it is demonstrated why the interest of the honorable reader might be aroused

* There are many Haskell tutorial resources out there but not many sharing experience developing a complete system
* Let's stop thinking about Haskell as "good for your brain but not for practical use"
* I will be happy if you end up thinking: *I could try this cool stuff in my next microservice*
* I will be even happier if you can put to **actual use** some of the stuff I present here

# On Various Aspects of Haskelland

-----

![](/images/frontispice.jpg)

----

Where the author tries to cover the most important aspects of Haskeller's civilisation in an extremely short span of time, thus
probably missing lot of important points and generating frustration in the informed reader, but nevertheless hoping to convey enough
tips for the audience to be willing to adopt some of the customs of those strange people.

# Language

## On some minor differences between Haskell Language and more Common ones

* Haskell's syntax favours terseness and strives to be close to mathematical language, compare:

    ```haskell
    [ (x, x * 2) | x <- [ 0 .. 100 ], x `mod` 3 == 0 ] 
    ```
  
* with:

    $\{ (x, 2x) | x \in [1,100] \land x\ mod\ 3 = 0 \}$

* Haskellers don't use lot of *punctuation* signs but favor using *indentation* to express nesting

## On the importance of composition in Haskell

* Haskell language is geared towards making *composition* of expressions straightforward
* *Referential transparency* allows one to factorize all kind of common sub-expressions to *remove redundancy* 
* *Partial application* and composition of functions makes it easy to write *function pipelines* and build new functions out of old ones

    ```haskell
    transactionsFromCashflows :: AccountId
                              -> (CashFlow -> [Transaction])
                              -> [CashFlow] -> [Transaction]
    transactionsFromCashflows pivot generator =
        concatMap $ map (normalize . balance pivot) . generator
    ```

## On the virtue of being lazy

* Haskell's evaluation semantics is *lazy*, *non strict* or *call-by-need*: Arguments to functions are evaluated only when needed
* This makes it practical to express *infinite computations* in the language
* This applies also to I/O operations which may not be evaluated hence have no side-effects if their result is not needed

## On some of the less desirable aspects of the language

* No support for first-class module which makes large scale development more cumbersome
* A natural tendency to abuse operators and be *too terse* and *dense* too the point of becoming cryptic
* Beginners should expect to be confused at first by how the system supports *strings* and *numbers*
* Lazy I/O can have annoying side-effects
* There are too many ways to handler exceptions

# Philosophy

## On the importance of Types for Haskellers

* Haskellers seems to be extreme *platonicists*: Everything must have a well-defined type!
* Although types can be *inferred* by the compiler, they are an extremely important tool to design programs
* Haskell's type system can be daunting at first but is conceptually elegant and *compact*

## Where one discovers types do not impose extra burden at runtime

* `newtype`s provide cheap encapsulation of other types as they are unpacked by compiler:

    ```haskell
    newtype AccountNr = AccountNr { accountNr :: T.Text }
                        deriving (Eq,Ord,Show,Read)


    instance IsString AccountNr where
        fromString = AccountNr . T.pack
    ```

* One should never refrain from creating such new types, they provide more safety than *type aliases*

## Where it is shown types can even conjure the undead

* *Phantom types* provide *type-level annotation* to add more information to other types
* Allow distinguishing between different types with identical representations

    ```haskell
    data Base64
    data Hex
    newtype Encoded code = Encoded { encodedText :: Text }
    toBase64Text :: ByteString -> Encoded Base64
    toHex        :: ByteString -> Encoded Hex
    ```
    
* Provide *thread-safety* by ensuring some type variables do not [escape local scope](https://wiki.haskell.org/Monad/ST)

## Where one discovers subtle differences cause more trouble than obvious ones

* [Type classes](http://typeclassopedia.bitbucket.org/) can be thought as way to define *interfaces*, asbtracting away implementation details

    ```haskell
    class (Eq a) => Differ a where
      uniqueId    :: a -> Id a
      constructor :: a -> Entity b
      equals      :: a -> a -> Bool
    ```

* Type classes can define not only functions, but values and types
* Type classes actually define *constraints* on types that can be accumulated in expressions and checked by the compiler

## Where one starts to wonder if there is an end to recursion

* *Type families* provide *type-level functions*, e.g. ways to compute types from types at compilation time
* Type families can be nested within `data` and `class` definitions to provide contextual types

    ```haskell
    class (ToJSON (Command a)) => BusinessModel a where
      data Event a   :: *
      data Command a :: *
    ```

* They can also be used at toplevel:

    ```haskell
    type family Id a :: *
    type instance Id Account       = AccountId
    type instance Id Transaction   = TransactionId
    ```

# Mores & Daily Life

## Where the amazed foreigner discovers how much can be done with crude tools

* There is no one-stop *Integrated Development Environment* for Haskell: Best support is provided by integrating various extensions
into **Emacs**
* There is a nice [tutorial](https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md) on how to setup emacs and
Chris Done has provided some [standard configuration](https://github.com/chrisdone/emacs-haskell-config)
* Makes it easy to setup *remote pairing*
* [Haskell for Mac](http://haskellformac.com/) is a recent initiative to provide an interactive and easy to use programming environment in Haskell
* [Spacemacs](http://spacemacs.org/) provides a viable alternative with "everything" preconfigured and packaged

## On the usefulness and efficiency of interacting with the machine

* Use the *REPL*, Luke! Provides much faster feedback loop than full-blown compilation and building, can be used for Type/Test-DD too
* GHC has a cool feature to handle **holes**: variables which are not in scope but typechecked so that one can use that to [deduce needed type and implementation](http://cfp.devoxx.fr/2016/talk/USZ-6984/TDD,_comme_dans_Type-Directed_Development)
* GHCi comes with a debugger but I have never used it: [Debugging sucks, testing rocks](http://googletesting.blogspot.fr/search/label/TotT)
* [IHaskell](https://github.com/gibiansky/IHaskell) provides a Haskell *kernel* for IPython notebooks for interactive programming. Provides a nice alternative to text-only development in Haskell esp. for number crunching

## On the typical day of a commoner in Haskell land

1. Write a skeletal test file using [hspec](http://hspec.github.io/), e.g. `test/FooTest.hs`
1. Start REPL in Emacs by loading file `C-c C-l`
1. See it fail to compile
1. `:r`eload until it compiles
1. Run the test and see it fail: `hspec myTest`
1. Fill in code until test passes
1. Do a full compile and test run before pushing to CI

# Government

## Where we discover Haskellers also value experimentation

* There are quite a few *testing* frameworks available to write unit or integration tests
* I favour [hspec](http://hspec.github.io/) which is one of the many rspec-inspired tools
* [hs-webdriver](https://github.com/kallisti-dev/hs-webdriver) provides bindings to [Selenium](http://docs.seleniumhq.org/) in Haskell
* Running tests is automated as part of build tools, e.g. cabal

## Where we see empiricism going hand in hand with formalism

* [QuickCheck](https://hackage.haskell.org/package/QuickCheck-2.8.2) is the *property-based testing* tool for Haskell
* Use it for defining formal properties of your code beyond what type system provides

    ```haskell
    instance Arbitrary ScaleRatio where  arbitrary = ...
    instance Arbitrary Transaction where  arbitrary = ...

    prop_scaled_transaction_is_normalized :: Transaction -> ScaleRatio
                                           -> Bool
    prop_scaled_transaction_is_normalized tx (ScaleRatio ratio) =
      isNormalized tx' && isBalanced tx'
      where
        tx' = scale ratio tx
    ```

## Where one discovers QuickCheck can be useful beside testing properties

* Generate sample data sets to be used as part of other kind of tests: We used it to generate test data for *schema migration* code
* Generate complex scenarios to test sequence of I/O operations

    ```haskell
    genAdminActions =
      frequency [ (1,  return [])
                , (1,  do
                       f <- NewListing <$> arbitrary <*> newFacility <*> rate <*> risk
                       (f:) <$> genAdminActions)
                , (10, (AcceptSomeFacility:) <$> genAdminActions)
                , (5,  (LookupFacilities:) <$> genAdminActions)
                , (6,  (AdvanceDay:) . (ConfirmRepayments:) <$> genAdminActions)
                ]
    ```

* [Monadic QuickCheck](https://hackage.haskell.org/package/QuickCheck-2.8.2/docs/Test-QuickCheck-Monadic.html) provides needed tooling to embed data generation within effectful code

## On the use of different level of testing

![](/images/pyramide.jpg)

# Architecture

## On the importance of laying out strong foundations

* GHC has become the standard compiler for Haskell, current version is 7.10.3
* It includes a *lot* of optimisations and features from 20 years of research on programming languages and compilation
* Always be sure to turn on  ` -Wall -Werror` when compiling: Warnings are often signs of potential troubles
* **Caveat**: Compilation can take a long time but still does not yet beat `scalac`'s slowness

## How Haskell craftsmen build complex software

* Standard package management tool is [Cabal](https://www.haskell.org/cabal/) which has become infamous due to [cabal hell](https://wiki.haskell.org/Cabal/Survival)
* Cabal is not very good at managing complex package structures out-of-the-box
* [stack](http://docs.haskellstack.org/en/stable/README/) greatly improves thing in order to provide fully reproducible builds and sandboxed environments, down to compiler versions
* [nix](http://nixos.org/nixpkgs/manual/#users-guide-to-the-haskell-infrastructure) provides a solution to package management in Haskell

# Diplomacy

## Where we separate that which is pure from that which is impure

* Type system provides a clean way to distinguish *pure* values from *impure* effects
* Staying *pure* has predictable semantics and opens the door to *optimisations*
* *Effectful computations* are one way portal to the *real world*

## Where we learn we can have our cake and eat it too

* Concurrency in Haskell is mostly based on composable [Software Transactional Memory](https://www.schoolofhaskell.com/school/advanced-haskell/beautiful-concurrency) operations
* STM do not mix with I/O operations hence cannot have observable *side-effects* but the upside is that runtime can detect *deadlocks*
* [async](https://hackage.haskell.org/package/async) a high-level library to package asynchronous computations within cheap Haskell threading model
* We used actor-like *queues* to protect access to I/O resources, e.g. database. Haskell provide a lot of useful abstractions on top of STM and core concurrency features to build your own tools

## How Haskellers do communicate with the REST of the world

* [WAI](https://github.com/yesodweb/wai/) provides all the needed components to build high performance REST-based services in Haskell
* Initial development was done with [Scotty](https://github.com/scotty-web/scotty/) which is a lightweight framework inspired by Sinatra
* Definitely need to have a look at [Spock](https://github.com/agrafix/Spock) which is a kind of "successor" to Scotty

## Where types make mortal beings doubt about their sanity

* Newer services are now developed with [Servant](https://github.com/haskell-servant/servant/) which provides a way to express APIs **at the type level** 

    ```haskell
    type CreateJob = ReqBody '[JSON] Job :> Post '[JSON] JobId
    type ListJobs  = Get '[JSON] [Job]
    type RemoveJob = Capture "jobid" JobId :> Delete '[JSON] JobId

    type SchedulerApi = "api" :> "scheduler" :> "jobs" :> CreateJob
                   :<|> "api" :> "scheduler" :> "jobs" :> ListJobs
                   :<|> "api" :> "scheduler" :> "jobs" :> RemoveJob
    ```

* Makes it possible to *derive* server, client or swagger specs from a single declaration

## On some ways data is persisted to stable storage

* Haskell has bindings to most open-source or standard (R)DBMS but *we did not used them*
* Seems like perfect environment to use [event sourcing](http://martinfowler.com/eaaDev/EventSourcing.html)
* We developed in-house solution to store events generated by the application as a *sequential log of events* stored in a file on disk

# War

## Where peaceful Haskellers prepare themselves to wage war

* [Shake](http://shakebuild.com/) is a Haskell tool to replace `make` and orchestrate complex builds
* [bake](https://github.com/ndmitchell/bake) is our *Continuous Integration* tool
* We used both to build and deploy all components of the system as [docker](http://docker.io) containers
* Both provides way to express complex build processes as *type-safe declarative* rules

## On the war-time organisation of Haskell's legions

* [propellor](http://propellor.branchable.com/) is a configuration management tool in Haskell developed by [Joey Hess](https://joeyh.name/) of Debian fame
* Configuration is expressed as a Haskell program that is compiled and run on the target host
* It provides a nice gpg key based model to encrypt private data (e.g. ssh keys, authentication tokens, passwords...) that need to be deployed in the source directory
* Once again, it allows expressing configuration items in a *type-safe* way

## On the capabilities to wage war outside of Haskelland

* [hdo](https://github.com/capital-match/hdo) is Haskell client for interacting with [Digital Ocean](https://www.digitalocean.com/)'s API where we deploy all our containers
* There is thorough Haskell support for AWS API through [Amazonka](https://github.com/brendanhay/amazonka) and Google's APIs through [gogol](https://github.com/brendanhay/gogol)
* Monitoring is done through a [riemann](http://riemann.io) [Haskell client](http://github.com/capital-match/riemann-hs)

# Conclusion

## What benefits one can expect from visiting Haskelland

* Strong type discipline and powerful compiler removes a whole class of programming errors
* Types are also great to design complex software systems and yield great opportunities for verification and optimizations
* All the *best practices* you learnt are still valid in Haskelland, only a bit differently practiced
* You can stay away from "hairy" stuff while still being productive and have fun

## What the cautious traveller should be aware of when ashoring Haskelland

* Development environment can be tricky to get - and stay - "right"
* A lot of the material one can find is *research focused* which can be daunting
* Avoid *partial functions* at all cost
* It can be hard to find Haskell developers *locally* 

# Acknowledgements

* [frontispice de l'Encyclopédie de Diderot et d'Alembert](https://upload.wikimedia.org/wikipedia/commons/9/93/Encyclopedie_frontispice_section_256px.jpg)
* [Pyramide d'Austerlitz](https://upload.wikimedia.org/wikipedia/commons/3/3b/Pyramide_Austerlitz_1805.jpg)
