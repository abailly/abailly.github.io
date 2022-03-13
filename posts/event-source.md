---
title: Life Beyond Relational Database in Haskell - The case for Event Sourcing
author: Arnaud Bailly 
date: 2016-05-12
---

This post contains the code I demonstrated as part of my talk at [nCrafts](http://ncrafts.io). This work is based on the following
references (stepping on the shoulders of giants, as always...):

* [Extensible effects paper](http://okmij.org/ftp/Haskell/extensible/exteff.pdf): Extensible effects theory and practice in Haskell
* [eff-cats](https://github.com/atnos-org/eff-cats): Same in Scala
* [Testing monadic code with QuickCheck](http://www.cse.chalmers.se/~rjmh/Papers/QuickCheckST.ps)
* [Blog post](http://abailly.github.io/posts/cm-arch-design.html) about the architecture implemented at Capital Match
* Work-in-progress [Haskell library](https://github.com/abailly/hevents) to simplify developing event sourced systems
* Original [Out of the Tar Pit](http://shaffner.us/cs/papers/tarpit.pdf) paper

# Imports, stuff to make the compiler happy #

We first add the usual `LANGUAGE` extension incantations... Note they should probably go in to the `.cabal` file.

```haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Hevents.Eff.Demo where
```

Then a whole bunch of imports... I never managed to choose a definite course of action on whether to import full module, import
qualified, import only selected symbols. Seems to me this is pretty much a team-level convention. 

```haskell
import           Control.Category
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Control.Eff                as E
import           Control.Eff.Exception
import           Control.Eff.Lift           as E hiding (lift)
import           Control.Exception          (finally)
import           Control.Monad.Except
import qualified Control.Monad.State        as ST
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Builder    as BS
import           Data.Either                (rights)
import           Data.Proxy
import           Data.Serialize             (Serialize, get, put)
import           Data.Typeable
import           Data.Void
import           Hevents.Eff                as W
import           Prelude                    hiding (init, (.))
import           Servant
import           Servant.Client
import           System.Environment
import           Test.Hspec
import           Test.QuickCheck            as Q
import           Test.QuickCheck.Monadic    as Q
```

# The "Business Domain"

We want to implement an event-sourced service that will allow us to manipulate a simple integer *counter*:

* Our counter changes when some value is *added* to it,
* A value is added when a command is issued that *increments* the counter, 
* We also want to modify the counter when a *decrement* command is issued, which means a *negative* value will be *added* to the
  counter, 
* We want our counter to be *bounded*: It shall never go below 0 or beyond 100,
* And its initial value will be 0.

# Let's start writing a test...

We'll first write some property describing the behaviour of our model for a single command. We expect that an `Increment` command
shall set an `init`ialized counter to the same value than the command, and that a `Decrement` command will decrease the value of
a counter. Note here we anticipate a bit on bounds requirement checking by setting the counter to some value which is greater than
any `Decrement` command we are supposed to issue.

```haskell
prop_shouldApplySingleCommandRespectingBounds :: Command Counter*> Bool
prop_shouldApplySingleCommandRespectingBounds c@(Increment n) =
    let OK result = init `act` c
    in  init `apply` result == Counter n
prop_shouldApplySingleCommandRespectingBounds c@(Decrement n) =
    let bounderCounter = Counter singleCommandUpperBound
        OK result = bounderCounter `act` c
    in  bounderCounter `apply` result == Counter (singleCommandUpperBound - n)
```

This property requires some way to generate `Arbitrary` instances of our commands, which is straightforward:

```haskell
instance Arbitrary (Command Counter) where
  arbitrary = oneof [ Increment <$> singleCommandValue
                    , Decrement <$> singleCommandValue
                    ]
    where
        singleCommandValue = choose (0,singleCommandUpperBound)

singleCommandUpperBound :: Int
singleCommandUpperBound = 20
```

Another useful property we want to define that our counter respects its bounds, no matter which sequence of events we
send to it:

```haskell
prop_shouldNotApplyCommandsOverBounds :: [ Command Counter ] -> Bool
prop_shouldNotApplyCommandsOverBounds commands =
  let finalCounter = ST.execState (mapM updateModel commands) init
  in  isWithinBounds finalCounter

isWithinBounds :: Counter -> Bool
isWithinBounds (Counter value) = value >= 0 && value <= 100
```

And of course we need some implementation of our `Command`s and `Counter`. The latter is pretty much a simple wrapping of `Int` but
to define the former we need our `Counter` type to be an instance of
[Model](https://github.com/abailly/hevents/blob/master/src/Hevents/Eff/Model.lhs) typeclass, which defines the basic structure
of an event-sourced component (or aggregate).

```haskell
newtype Counter = Counter { counter :: Int } deriving (Eq,Show)

instance Model Counter where
```

We define the "component" types of our model: Commands, events and errors which here are very simple.

```haskell
  data Command Counter = Increment Int
                       | Decrement Int
                       deriving (Eq, Show)
  data Event Counter = Added Int deriving (Eq,Show)
  data Error Counter = OutOfBounds deriving (Eq,Show)
```

Then comes our initial value...

```haskell
  init = Counter 0

```

`act` computes the effect of applying a command to current state of our counter...

```haskell

  Counter k `act` Increment n = if k + n <= 100
                                then OK $ Added n
                                else KO OutOfBounds

  Counter k `act` Decrement n = if k - n >= 0
                                then OK $ Added (-n)
                                else KO OutOfBounds
```

Then `apply` actually "updates" (or more precisely, create a new updated instance of) the counter by applying the value to add.

```haskell
  Counter k `apply` Added n = Counter $ k + n
```

When we check the behaviour of applying a sequence of `Command`s to our counter, we make use of a library function which runs in the
`State` monad and allow us to "fold" the application of a sequence of commands to a `Counter`:

```haskell
updateModel :: (Model a) => Command a -> State a (Result a)
```
  
# Exposing services built on our model

As always, we start with the testing part but this time we expect our tests to have side effects and model interactions of the
outside world with our system's fragment. We shall start with very simple modelling of client's behaviour: 

```haskell
data CounterAction = GetCounter
                   | IncCounter Int
                   | DecCounter Int
                   deriving (Show)

```

In order to generate samples for our actions we assume some frequency distribution, giving more weight to actions that get state
than to actions that update it. 

```haskell

instance Arbitrary CounterAction where
  arbitrary = frequency [ (3, return GetCounter)
                        , (2, IncCounter <$> choose (0,10))
                        , (1, DecCounter <$> choose (0,10))
                        ]
```

Then we use monadic QuickCheck to run an `arbitrary` sequence of user actions on an "effectful" model which is initialised with some
state holder and a storage backend.

```haskell
prop_servicesRespectCounterBounds :: [ CounterAction ] -> Property
prop_servicesRespectCounterBounds actions = Q.monadicIO $ do
  results <- Q.run $ do
    (model, storage) <- prepareContext
    mapM (effect storage model . interpret) actions

  assert $ all isWithinBounds (rights results)
```

This test might be considered to be a little weak, and we could probably enhance it with testing error conditions. That's something
definitely worth doing for production code, however for the sake of simplicity we will not add more tests here.

Prparation step is simple but deserve some explanations:

* We create a `Counter` with some initial value and wrap it in a *transactional variable* because our underlying `State` effects
  works within the `STM` monad. This is so in order to ensure proper atomicity of commands on the model in face of concurrent
  access, 
* We create a simple in-memory store, which is a STM-based bounded queue.


```haskell
prepareContext = (,)           <$>
  newTVarIO (W.init :: Counter) <*>
  atomically W.makeMemoryStore
```

`effect` is actually a natural transformation that composes all the small little effects we need in our sample and "lift" them in
the `IO` monad. Note the `Eff` type which exposes explicitly all the effects our code is allowed to make thus constraining its
behaviour to a limited subset of possible interactions with outside world. The `ServantErr` type is the type of *exceptions* we can
"throw" using `Exc` effect: This anticipates on the needs of the REST API we shall expose later on. Actually it could have been any
kind of `Exception` instance but once again, this makes things simpler and removes a layer of transformation from custom exceptions
to Servant errors, something we would probably want to do in production code.

```haskell
type EventSourced s e a =
  E.Eff (State s E.:> Store E.:> Exc e E.:> Lift STM E.:> Void) a

effect :: (Typeable m, Typeable e, Storage STM s, Registrar STM m reg)
         => s -> reg
         -> E.Eff (EventSourced Counter ServantErr) a
         -> IO (Either e a)
effect s m = atomically . runSync . runExc . W.runStore s .  W.runState m
```

The definition of our services is pretty straightforward:

* `getCounter` simply return the state of the counter,
* `increment` and `decrement` both send the corresponding commands and store the resulting event, returning the content of the
  counter.

```haskell
type CounterService a = EventSourced Counter ServantErr a

getCounter :: CounterService Int
getCounter = counter <$> getState

increment :: Int -> CounterService Int
increment n = applyCommand (Increment n) >>= storeEvent

decrement :: Int -> CounterService Int
decrement n = applyCommand (Decrement n) >>= storeEvent
```

The `storeEvent` function is where most of the grunt work happens and notably where we do error handling:

* We first try to `store` the produced `Event Counter`, and if this succeeds we return the state of the counter. If this fails, we
  convert the error in a `500` error, passing some hopefully useful message,
* If the input is an `Error Counter` we simply rethrow the converted error as a `400` error, all errors produced by commands
  represent precondition violations of the model's specification.

```haskell
storeEvent :: Either (Error Counter) (Event Counter)
             -> CounterService Int
storeEvent (Left e)  = throwExc $ fromModelError e
storeEvent (Right e) = store e >>= either (throwExc . fromDBError) (const $ counter <$> getState)
  where
    fromModelError e = err400 { errBody = makeBody $ "Invalid command " ++ show e }
    fromDBError    e = err500 { errBody = makeBody $ "DB Error " ++ show e }
    makeBody         = BS.toLazyByteString . BS.stringUtf8
```

Note that because of the `Store` effect we need to be able to *serialize* our events:

```haskell
instance Serialize (Event Counter) where
  put (Added i) = put i
  get           = Added <$> get
```

The last missing piece is the `interpret` function which turns our QuickCheck generated actions into actual effectful actions to be
run against our system.


```haskell
interpret GetCounter     = getCounter
interpret (IncCounter n) = increment n
interpret (DecCounter n) = decrement n
```

# Expose our counter services through a REST API

The last step of our Counter "microservice" is to expose it as a REST interface. We will leverage the excellent work done on the
[Servant](http://servant.github.io) and firstly define out API's type which is a simple exposition of the previously defined
services: 

```haskell

type CounterApi =
 "counter" :> (Get '[JSON] Int
              :<|> "increment" :> Capture "inc" Int :> Get '[JSON] Int
              :<|> "decrement" :> Capture "dec" Int :> Get '[JSON] Int)

counterApi :: Proxy CounterApi
counterApi = Proxy
```

Writing our test is pretty straightforward and mostly repeats the previous test at the service level layer, the main difference
being the effectful services are run within an actual web server on some predefined port. Note this is makes our test brittle and
non parallelizable: It would be better to let the server select a free port and return it as part of its startup.

The only noteworthy part is that we can use our previously defined `effect` "interpreter" natural transformation and wrap it inside an `EitherT`
transformer which is the type expected by Servant.

```haskell
prop_counterServerImplementsCounterApi :: [ CounterAction ] -> Property
prop_counterServerImplementsCounterApi actions = Q.monadicIO $ do
  results <- Q.run $ do
    (model, storage) <- prepareContext
    server <- W.runWebServerErr 8082 counterApi
                  (Nat $ EitherT . effect storage model) handler
    mapM runClient actions `finally` cancel server

  assert $ all isWithinBounds (rights results)
```

The client-side services are obtained from a destructured assignment using Servant's `(:<|>)` operator which is overloaded at the
type and value level and then used to interpret user actions.

```haskell
counterState :<|> incCounter :<|> decCounter = client counterApi (BaseUrl Http "localhost" 8082)

runClient GetCounter     = runEitherT $ counterState
runClient (IncCounter n) = runEitherT $ incCounter n
runClient (DecCounter n) = runEitherT $ decCounter n
```

It is also noteworthy we can simply build our REST server composing our already defined services. Servant's type wizardy ensures the
expected type for the whole API is matched by actual functions composed with `(:<|>)`.


```haskell
handler = getCounter :<|> increment :<|> decrement
```

Writing a `main` server that is able to listen on some port and runs our services is left as an exercise for the reader.

# Conclusion

The goal of this post and the associated talk was to demonstrate how Haskell's type system and some well designed and though out
libraries made it easy to build type-safe "microservices". In particular, I would like to emphasize the following points:

* Haskell's pure functional core makes it a perfect fit for designing, implementing and experimenting business domain models using
  *Domain Driven Design* principles: Use a common language which here is embedded in the form of commands, events and errors,
  provide a pure core which implements core business rules and can be very easily tested, wrap that pure core within a *Hexagonal
  architecture* providing needed side-effects...
* QuickCheck is a great tool for doing *Test Driven Development* of such models: One can use it to expose assumptions about the
  behaviour of the model's clients in the form of *Arbitrary* instances and then test potentially complex sequences of actions
  against the model,
* Using monadic QuickCheck, it is possible to leverage that technique at any level of the system's stack, making it easy to test
  various effects of our system and how it interacts with the outside world. Something that we could introduce here is testing the
  server with multiple concurrent clients and then checking our model doesn't break its invariants in the face of concurrent
  accesses,
* The *Extensible effects* framework allows us to define and compose tiny effectful "DSLs" over the core domain DSL. More
  importantly, thanks to its extensibility, it is possible to define new effects without having to recompile any of our existing
  services (note this would require our services' types to be more lax, using typeclass constraints instead of concrete type for the
  *Eff* monad).
