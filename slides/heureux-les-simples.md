---
title: "Blessed are the poor in spirit"
subtitle: Confession of a repentant Effects programmer
author: arnaud.bailly@iohk.io - @dr_c0d3
date: 2022-09-20
theme: serif-black
---

# Intro

## Agenda

* Quick recap about _Free monads_
* My experience using such _Effects systems_
* My two cents on how to deal with effects

## Background

![](/images/jack-of-all-trades.jpg)

## Background

* 25+ years of professional development
* 20+ years hacking Haskell
* Currently working at [Input Output Global](https://iohk.io), the makers of [Cardano](https://cardanofoundation.org/)

# Side-Effects in a Pure Language

## Motivating Example

Let's assume one has a nice little component providing an interface to manage a nuclear arsenal. This component allows to:

* _Launch missiles_ immediately
* _Set DefCon_ level to some value
* _Get_ current _DefCon_ level value

## IO-based Interface

In the beginning, there came the `IO` monad $\rightarrow$ [Tackling the Awkward Squad]()

```haskell
launchMissiles :: IO ()

setDefCon :: Int -> IO ()

getDefCon :: IO ()
```

## The Problem

* `IO` is too general, anything can happen in a function returning `IO a`
* We want consumers of an interface to not have to bother about its _implementation_'s side-effects
* We want the _types_ to help us provide _guarantees_ about side-effects a function is having

# Free-monad Based Effects handling

## The Free Monad Approach

* Let's restrict the _effects_ we can make $\rightarrow$ Define a _Domain-Specific Language_
* Define DSL through an _Algebraic Data Type_ representing all the possible _actions_
* Apply _Free monad_ construction to be able to embed this language as _monadic_ code

## Canonical Encoding

Define an _Effect language_

```haskell
data Command a =
     LaunchMissiles Timeout (() -> a)
   | GetDefCon (Int -> a)
```

--------

Turn our language into a _Monad_ and add smart constructors:

~~~~ {.haskell}
type CommandM = Free Command

launchMissiles :: Timeout -> CommandM ()
launchMissiles t = liftF $ LaunchMissiles t identity
~~~~

-----

Provide an interpreter for our language:

~~~~ {.haskell}
interpreter :: CommandM a -> IO a
interpreter = iterM runCommand
  where
    runCommand (LaunchMissiles f) = do
       actuallyLaunchMissilesInIO
       f ()
    ...
~~~~

## Composing Effects

* Having a _monolithic_ language is not desirable
* We need a way to _compose_ smaller building blocks
* _Vertically_, by interpreting one language into another, lower-level language
* _Horizontally_, by mixing different languages to solve a complex problem

## Free-Monad Zoo

* Different (more efficient) encodings have been proposed
* Lots of effects libraries in Haskell: [free](https://hackage.haskell.org/package/free), [freer-simple](https://hackage.haskell.org/package/freer-simple), [extensible-effects](https://hackage.haskell.org/package/extensible-effects), [polysemy](https://hackage.haskell.org/package/polysemy), [fused-effects](https://hackage.haskell.org/package/fused-effects)...
* Lot of vocal advocates: [Alexander Granin's book](https://graninas.com/functional-design-and-architecture-book/), [Sandy Maguire's blog](https://reasonablypolymorphic.com/blog/freer-monads/), [Arnaud Bailly's _Eff in Anger_](https://www.youtube.com/watch?v=uIRHYMDsDQ0)...

# In practice

## Take 1: Free & Co-free

* [hdo](https://github.com/abailly/hdo), a tool for controlling [DigitalOcean](https://www.digitalocean.com/) resources
* Early attempt at designing and building a useful tool using this technique
* Made more complicated than needed because of use of Free/Co-Free duality

## Outcome

* Lot of boilerplate: Each sub-language came in 2 flavors:
  * As a _Sum_ to express individual actions and compose them
  * As a _Product_ to express interpreter for each action
* Unclear what the value of the Co-free stuff is...
* Yet I had _1_ external contribution!

## Take 2: Extensible Effects

* _Gorilla Space_: an attempt at building an "exchange" for commercial space
* Used _extensible-effects_ as a foundation for _System level_ effects (`State`, `Persist`, `WebServer`) embedded inside [servant's](https://hackage.haskell.org/package/servant) type-level combinators for web API
* Added ideas from _event sourcing_ and _miniservices_

-----

```haskell
type EventStore a =
    Eff (State Listing :> Log LogListing :>
         Exc ServantErr :>
         Lift IO :> Void) a

addListing :: Listings -> EventStore (Answer Listings)
addListing l = applyCommand (Add l) >>= handleResult
```

## Outcome

![](/images/one-ring.jpg)

## Outcome

* It was kind of OK as most of the complexity was hidden inside low-level layers
* I was the sole developer so hard to tell really
* Project never took off even though it went live in "demo" mode for a while

## Take 3: Freer

* _Symbiont_: Build a transaction manager for a permissioned blockchain system
* Used _freer-simple_ package which is an evolution of _extensible-effects_
* Modelled various _functional layers_ of the system as _GADTs_

## Outcome

![](/images/gasworks.jpg)

-----

* Large stack comprising 8-10 effects, hard to understand type errors, complex wiring due to dependencies between effects
* Initial strict layering went bust after a while leading to _Big Bull of Mud_ and IO-creep
* Application has been rewritten, see [here](https://medium.com/barely-functional/freer-doesnt-come-for-free-c9fade793501) and also [here](https://medium.com/barely-functional/do-we-need-effects-to-get-abstraction-7d5dc0edfbef)

## Take 4: Operational

* _Symbiont_: Built a flexible system-level testing framework to run tests locally, with docker...
* Used [operational](https://hackage.haskell.org/package/operational) package which is a "simpler" _Free monad_
* Replaced existing script-based ad hoc tests in favour of an "easy" to use scenario-based testing DSL
* More details in those [slides](testing-distributed-systems-with-docker.html)

## Outcome

![](/images/joy.png)

-----

* Worked out pretty well initially, scaling to cloud execution, spinning up VMs and the like
* **Problem**: Maintenance! As I moved to other projects, it became harder and harder for others to maintain and extend the tool until it got deprecated.

## Take 4: Moar Freer

* _IOG_: [Plutus](https://developers.cardano.org/docs/smart-contracts/plutus) is the Smart Contracts language for Cardano
* [Plutus Application Backend](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/ARCHITECTURE.adoc) is the official framework to build applications with and it's built with _freer_
* I was involved as a _user_ of the framework not an _implementor_ to build a Layer 2 solution on top of Cardano based on Smart Contracts

----

```haskell
game :: AsContractError e => Contract () GameSchema e ()
game = do
    logInfo @Haskell.String "Waiting for guess or lock endpoint..."
    selectList [lock, guess] >> game
```

## Outcome

* Better engineered than my previous attempts
* Still suffering from same issues: Complex type machinery, rigid framework, complex wiring, hard to test
* Steep learning curve for newcomers

## Key Takeaways

* Effect systems introduce an extra layer of indirection which _increases_ cognitive burden instead of _decreasing_ it
* Haskell is an _big old language_ so there's no way to build a truly seamless eDSL $\rightarrow$ We need to _encode_ effects
* DevX is somewhat terrible past the initial amazement
* Learning curve is very steep
* Compilation time increases significantly
* Performance overhead at runtime

-----

Effect systems are great for solo projects and research papers

![](/images/pipe-organ-builder.jpg)

-----

They are not so great for large scale development efforts

![](/images/refinery.jpg)

# In Praise of "Boring" Haskell

-----

![](/images/biface.jpg)

-----

* Haskell and other FP languages have this great _design patterns_ we can leverage
* Functions
* Functions that take functions as arguments
* Functions that return functions as result

-----

![](/images/function-universal-patterns.png)

------

[Handle](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html) pattern aka. _Record of functions_

```haskell
data Command m =
   Command { launchMissiles :: Timeout -> m ()
           , getDefCon :: m Int
           }
```

## Use Alternative Languages

![](/images/architecture-junning.jpg)

----

* Haskell has

# Questions?

![](/images/magritte-pipe.jpg)
