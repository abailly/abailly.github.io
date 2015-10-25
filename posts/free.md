------------
title: On Free DSLs and Cofree interpreters
author: Arnaud Bailly 
date: 2015-06-04
------------

This post has been triggered by a [tweet](https://twitter.com/etorreborre/status/605562458279944192) from Eric Torreborre on a talk
by David Laing presenting the interaction of Free DSLs and Cofree interpreters at the Brisbane Functional Programming Group. I am
currently engaged in the development of a Haskell-based system for [Capital Match](http://www.capital-match.com) which is basically
an API for managing peer-to-peer lending, and I am trying to formalise the API of the system as the result of a composition of
several domain-specific languages.

The ultimate goal is to be able to use these DSLs to define complex actions that could be interpreted in various ways: a
command-line client sending RESTful queries to a server, a Webdriver-based test executor or a simple test recorder and comparator,
or even by a core engine interpreting complex actions in terms of simpler sequencing of service calls.

The rest of the post is a simple literate Haskell style explanation of what I came up with today exploring the specific topic of the
composition of DSLs and interpreters: Given we can compose DSLs using *Free* monads and *Coproduct*, how can we *Pair* a composite
DSL to the composition of several interpreters? The answer, as often, lies in the category theoretic principle for duality: *Reverse
the arrows!* One composes interpreters into a *Product* type which is then lifted to a *Cofree* comonad paired to a *Free Coproduct* monad. 

This post has no original idea and is just rephrasing and reshaping of work done by more brilliant people than I am:

* Dan Piponi's [Cofree meets free](http://blog.sigfpe.com/2014/05/cofree-meets-free.html) blog post,
* This [thread on Stack overflow](http://programmers.stackexchange.com/questions/242795/what-is-the-free-monad-interpreter-pattern)
  about free monads,
* Runar Bjarnason talk on [Reasonably Priced Monads](https://dl.dropboxusercontent.com/u/4588997/ReasonablyPriced.pdf),
* An [Haskell implementation](https://gist.github.com/aaronlevin/87465696ba6c554bc72b#file-reasonable-hs) of the above by Aaron
  Levin,
* [Comonads are objects](http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html) by Gabriel Gonzalez, 
* [Data types à la carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf) by Wouter Swiestra,
* Edward Kmett's [All about comonads](http://comonad.com/haskell/Comonads_1.pdf) slide deck,
* And of course David Laing's [github](https://github.com/dalaing/cofun) repository.

I would not dare to say I really *understand* all of this, but at least I got some code to compile and I have some ideas on how to
turn this into a useful "pattern" in our codebase. 

# Free Coproduct DSLs

So let's start with some usual declaration and imports...

~~~~~~~~~ {.haskell .numberLines}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Capital.Client.Free  where

import           Control.Applicative
import           Control.Comonad.Cofree
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Identity
import           Control.Monad.Trans    (MonadIO, liftIO)
~~~~~~~~~~

This relies on the [free](https://hackage.haskell.org/package/free) package which defines standard *free* Constructions for
`Applicative` and `Monad`, and *cofree*  for `Comonads`.

We define our basic business-domain specific functors, one for logging some messages and another for persisting some string
value. The actual functors defined are not important, what interests us here is the fact we define those "actions" independently but
we want in the end to be able to "Assemble" them yielding more complex actions which can at the same time log messages and persist
things. 

~~~~~~~~~ {.haskell .numberLines}
data Logging a = Logging String a  deriving (Functor)

data Persist a = Store String a deriving Functor
~~~~~~~~~~

Our composite DSL should be able to interpret actions which are either logging actions, or persist actions, so we need a way to
express this alternative at the type-level, introducing the notion of *Coproduct* or *Sum*. This work has already been packaged by
Ed Kmett in the [comonads-transformers](https://hackage.haskell.org/package/comonad-transformers-2.0.3) package but let's rewrite it
here for completeness' sake.

~~~~~~~~~ {.ha1skell .numberLines}
newtype Coproduct f g a = Coproduct { getCoproduct :: Either (f a) (g a) }
~~~~~~~~~

A `Coproduct` of two functors is then simply the type-level equivalent of the familiar `Either` type, for which we provide smart
constructors to inject values from left or right and a suitable `Functor` instance.

~~~~~~~~~ {.haskell .numberLines}
left :: f a -> Coproduct f g a
left = Coproduct . Left

right :: g a -> Coproduct f g a
right = Coproduct . Right

coproduct :: (f a -> b) -> (g a -> b) -> Coproduct f g a -> b
coproduct f g = either f g . getCoproduct

instance (Functor f, Functor g) => Functor (Coproduct f g) where
  fmap f = Coproduct . coproduct (Left . fmap f) (Right . fmap f)
~~~~~~~~~

We want to be able to implicitly "lift" values from a component into its composite without resorting to explicit packing of the
various parts of the alternative formed by a `Coproduct` type, something which would be extremely cumbersome to express, hence the
introduction of a *natural transformation* `Inject` expressed in Haskell as a typeclass.

~~~~~~~~~ {.haskell .numberLines}
class (Functor f, Functor g) => f :<: g where
  inject :: f a -> g a
~~~~~~~~~

To be useful we provide several interesting instances of this typeclass that defines how to inject functors into a `Coproduct`. Note
that this requires the `OverlappingInstances` extension otherwise the compiler[^1] will refuse to compile our programs. I think this
stuff could be expressed as *type families* but did not manage to get it right, so I gave up and resorted to original formulation by
Wouter Swiestra.

~~~~~~~~~ {.haskell .numberLines}
instance (Functor f, Functor g) => f :<: Coproduct f g where
  inject = left

instance (Functor f, Functor g, Functor h, g :<: h) => g :<: Coproduct f h where
  inject = right . inject

instance (Functor f) => f :<: f where
  inject = id
~~~~~~~~~

Finally, we provide "smart constructors" that generates `Free` monadic expressions out of the individual instructions of our two
tiny DSLs. We use a `inFree` function combining lifting into `Free` monad and possible transformation between functors so that each
expressed action is a `Free` instance whose functor is polymorphic. This is important as this is what will allow us to combine
arbitrarily our DSL fragments into a bigger DSL.

~~~~~~~~~ {.haskell .numberLines}
inFree :: (Functor f, f :<: g) => f a -> Free g a
inFree = hoistFree inject . liftF

log :: (Logging :<: f) => String -> Free f ()
log msg = inFree (Logging msg ())

store :: (Persist :<: f) => String -> Free f ()
store s = inFree (Store s ())
~~~~~~~~~

Equipped with all this machinery we are ready to write our first simple program in a combined DSL:

~~~~~~~~~ {.haskell .numberLines}
type Effect = Coproduct Logging Persist

prg :: Free Effect ()
prg = store "bar" >> log "foo"
~~~~~~~~~

# Cofree Product Interpreters

We are now done with the DSL part, let's turn to the interpreter part. First we need some atomic interpreters which should be able
to interpret commands from each of our DSL. We will prefix these functors with `Co` to demote the relationship they have with the
DSL functors. Something which is not obvious here (because our DSL functors only have a single constructor) is that these
interpreters should have a dual structure to the DSL functors: Given a DSL expressed as a sum of constructors, we need an
interpreter with a product of intepretation functions. The DSL presented in David's post are more expressive...

~~~~~~~~~ {.haskell .numberLines}
data CoLogging a = CoLogging { cLog :: String -> a }  deriving Functor

data CoPersist a = CoPersist { cStore :: String -> a }  deriving Functor
~~~~~~~~~

Of course we need concrete interpretation functions, here some simple actions that print stuff to stdout, running in `IO`.

~~~~~~~~~ {.haskell .numberLines}
coLog :: (MonadIO m) => m () -> String -> m ()
coLog a s = a >> (liftIO $ print s)

coStore :: (MonadIO m) => m () -> String -> m ()
coStore a s = a >> (liftIO . print . ("storing " ++)) s
~~~~~~~~~

To be able to compose these interpreters we need a `Product` type whose definition is straightforward: This is simply the type-level
equivalent of the `(,)` tupling operator.

~~~~~~~~~ {.haskell .numberLines}
newtype Product f g a = Product { p1 :: f a, p2 :: g a }

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap f (Product (a,b)) = Product (fmap f a, fmap f b)
~~~~~~~~~

Then we can define our complex interpreter and what interpretation means in the context of this composite. `coiter` is a function
from the [`Cofree`](https://hackage.haskell.org/package/free-4.12.1/docs/Control-Comonad-Cofree.html) module that "lifts"
computation in a Functor into a `Cofree` monad, starting from a seed value.

~~~~~~~~~ {.haskell .numberLines}
type Interp = Product CoLogging CoPersist

interpretEffect :: Cofree Interp (IO ())
interpretEffect = coiter f (return ())
  where
     f a = Product (CoLogging $ coLog a, CoPersist $ coStore a)
~~~~~~~~~

# Tying Free to Cofree

This is where the "magic" occurs! We need a way to *tie* our DSLs to our interpreters so that we can apply the latter to the former
in a consistent way, even when they are composed. Enters the `Pairing` class which express this relationship using a function tying
together each functor (DSL and interpreter) to produce a result. 

~~~~~~~~~ {.haskell .numberLines}
class (Functor f, Functor g) => Pairing f g where
  pair :: (a -> b -> r) -> f a -> g b -> r
~~~~~~~~~

For the `Identity` functors, `pair`ing is simply two-arguments function application.

~~~~~~~~~ {.haskell .numberLines}
instance Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b
~~~~~~~~~

We can also define a pair relating function types and tuple types, both ways:

~~~~~~~~~ {.haskell .numberLines}
instance Pairing ((->) a) ((,) a) where
  pair p f = uncurry (p . f)

instance Pairing ((,) a) ((->) a) where
  pair p f g = pair (flip p) g f
~~~~~~~~~

And finally we can pair `Cofree` and `Free` as well as  `Product` and `Coproduct`, thus providing all the necessary tools for tying
the knots. Note that in this case no intepretation takes place before pairing hit a `Pure`  value, which actually means that
interpretation first need to build all the "spine" for program to be interpreted then unwind it and applying interpretation step to
each instruction. This precludes evaluating infinite "scripts".[^2] 

~~~~~~~~~ {.haskell .numberLines}
instance Pairing f g => Pairing (Cofree f) (Free g) where
  pair p (a :< _ ) (Pure x)  = p a x
  pair p (_ :< fs) (Free gs) = pair (pair p) fs gs

instance (Pairing g f, Pairing k h) => Pairing (Product g k) (Coproduct f h) where
  pair p (Product (g,_))  (Coproduct (Left f)) = pair p g f
  pair p (Product (_,k)) (Coproduct (Right h)) = pair p k h
~~~~~~~~~

We finally tie the appropriate "leaf" functors together in a straightforward way.

~~~~~~~~~ {.haskell .numberLines}
instance Pairing CoLogging Logging where
  pair f (CoLogging l) (Logging m k) = f (l m) k

instance Pairing CoPersist Persist where
  pair f (CoPersist s) (Store v k) = f (s v) k

type Effect = Coproduct Logging Persist
~~~~~~~~~

We are now ready to define and interpret programs mixing logging and persistence:

> let prog = store "bar" >> logI "foo" >> store "quux" >> logI "baz" :: Free Effect ()
> λ> pair const interpretEffect ((return <$> prog) :: Free Effect (IO ()) )
> "storing bar"
> "foo"
> "storing quux"
> "baz"
> λ> 

# Conclusion

As is often the case when dealing with "complex" or rather unfamiliar category theoretic constructions, I am fascinated by the
elegance of the solution but I can't help asking "What's the point?" There is always a simpler solution which does not require all
this machinery and solves the problem at hand. But in this case I am really excited about the possibilities it opens in terms of
engineering and architecting our system, because it gives us a clear and rather easy way to:

* Define in isolation fragments of DSL matching our APIs and business logic,
* Define one or more interpreter for each of these fragments,
* Combine them in arbitrary (but consistent for pairing) ways.

This code is in [gist](https://gist.github.com/abailly/84a54ace82a67c3c8aab).

[^1]: GHC 7.8.3 in our case

[^2]: In private conversation by email David Laing told me follow-up talks will deal with free/cofree duality with effects thus
taking care of evaluating monadic scripts and interpreters.
