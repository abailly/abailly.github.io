---
title: Model-Based Testing with QuickCheck
author: Arnaud Bailly - @dr_c0d3
institution: Input Output Global
date: 2024-03-15
theme: virgil-black-iohk
revealjs-url: /reveal.js
---

## Agenda

* Why?
* Property-Based Testing
* `quickcheck-dynamic`: Practice
* `quickcheck-dynamic`: Theory
* Conclusion

# Why?

## The Quest for Better Software

Going full circle

_Automata Testing_ $\rightarrow$ _Test-Driven Development_ $\rightarrow$ _Property-Based Testing_ $\rightarrow$ _Model-Based Testing_

::: notes

:::

## Engineering vs. Science

![Dijkstra on Testing](/images/dijkstra-testing-quote.jpeg)

::: notes

* Not sure what Dijkstra meant here, was it a way to diminish the relevance of testing for "proving" program correctness?
* Don't take Dijkstra too seriously, he seems to have been a grumpy old man for most of his life
* Testing is critically important to _design_ and _develop_ any program
* Testing is a practical engineering compromise when proving is too difficult: We would like to "prove" everything but that does not necessarily makes sense and it's not always feasible

:::

## My Goals for this Talk

* Share my experience with Model-Based testing to...
* ... spark interest in the use of this family of tools and ...
* ... trigger contributions to improve `quickcheck-dynamic`

::: notes

contributions = code + feedback

## What can you expect?

* An overview of an interesting new tool
* "From the trenches" report on the usefulness of model-based testing
* Tips & tricks on how and what to model

# Property-Based Testing

## What is Property-Based Testing?

* Example-based testing $\rightarrow$ Property-Based Testing
* Generalise examples to express _properties_ of the SUT

::: notes

:::

## Classical examples

```haskell
elements_are_sorted :: [a] -> Property
elements_are_sorted list =
  let sorted = sort list
  in undefined  --- TBD

```

::: notes

* property of a "sortedness" can be expressed independently of the algorithm to implement it

:::

## Classical examples

```haskell
roundtrip_encoding_decoding :: a -> Property
roundtrip_encoding_decoding a =
  let encoded = encode a
      decoded = decode encoded
  in decoded === Just a

```

::: notes

* very simple properties to guarantee consistency of any roundtrip

:::

## QuickCheck Core Features

* Generate some number of [arbitrary](https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck.html#g:5) values...
* ... when property fails, [shrink](https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck-Arbitrary.html#v:shrink) the failing input ...
* ... to provide smallest [counterexample](https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck.html#v:counterexample) upon failure

::: notes

* basic principles of PBT, works for any library out there

:::

## Additional Key Features

* [cover](https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck.html#v:cover) expected distribution of test cases
* Build [generators](https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck.html#g:8) combining smaller pieces
* Derive generators [generically](https://hackage.haskell.org/package/generic-random)
* ... and much more :right_pointing_hand: [QuickCheck on hackage](https://hackage.haskell.org/package/QuickCheck)

## Stateful QuickCheck

* [Monadic](https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck-Monadic.html) version of QC
* Running properties against stateful (IO)  programs

----

![Testing Monadic code with QuickCheck](/images/monadic-quickcheck-paper.png)

## Stateful QuickCheck

* Define a _Domain-Specific Language_ of inputs to the SUT
* Provide a _Model_ of the expected behaviour
* Run the _Model_ and the SUT in parallel with an _arbitrary_ sequence of actions
* Compare the results

## Example: A Basic Repository Interface

[Real-life example](https://github.com/abailly/sensei/blob/fd8aafe8e0f3e1422c6b08e1079c75d1adff395e/test/Sensei/DB/Model.hs#L44) drawn from personal tool that needs to read/write events in a database.

----

Define a GADT of "interesting" `Action`s to run

```haskell
-- | Relevant commands issued to the underlying DB
data Action a where
  WriteEvent :: Event -> Action ()
  ReadEvents :: Pagination -> Action EventsQueryResult
  ...
  NewUser :: UserProfile -> Action ()
  SwitchUser :: Text -> Action ()
```

----

Interpret actions against a `Model`

```haskell
interpret :: (Monad m, Eq a, Show a) => Action a -> StateT Model m (Maybe a)
interpret (WriteEvent f) = do
  modify $ \m@Model {events} -> m {events = events |> EventView {index = fromIntegral (Seq.length events + 1), event = f}}
  pure $ Just ()
interpret (ReadEvents (Page pageNum size)) = do
  es <- getEvents
  ...
```

----

Generate (meaningful) sequence of `Action`s:

```haskell
generateActions :: Model -> Gen Actions
generateActions model =
  Actions <$>
    (arbitrary >>=
       generateAction startTime model . getPositive)
```

----

Run interpreter and SUT in parallel and check conformance:

```haskell
canReadFlowsAndTracesWritten ::
  (DB db, HasCallStack) =>
  FilePath -> (forall x. db x -> IO x) -> Property
canReadFlowsAndTracesWritten dbFile nt =
  forAllShrink (generateActions start) shrinkActions $
    \(Actions actions) -> monadicIO $ do
      res <- run $ nt $
        initLogStorage >> evalStateT (validateActions actions) start
      forM_ res monitorErrors
      assert $ all isNothing res
```


# `quickcheck-dynamic`: Practice

## The need for a better tool

* Ad-hoc model-based testing has limitations
* Quviq people are experts in these techniques and they were building a tool for IOG
* I was involved in a complex project that would benefit from a comprehensive testing strategy

::: notes

* Have personal experience building various tools over the course of my career, it's always a significant investment

:::

## Hydra - Overview

* A Layer-2 network for UTxO blockchains based on _State channels_
* Complex on-chain protocol advancing a state-machine through transactions
* Off-chain leader-based distributed consensus
* More details at https://hydra.family

----

![High-level Hydra Protocol](/images/hydra-head-lifecycle.svg)

----

![Hydra Deployment](/images/basic-hydra-head.jpg)

## Hydra - Model

![Hydra Test Architecture](/images/hydra-test-architecture.png)

## Hydra - Model

* Simulation-based testing + MBT = Powerful _combo_ ([FoundationDB](https://apple.github.io/foundationdb/testing.html), [Quviq's PULSE](https://smallbone.se/papers/finding-race-conditions.pdf))
* Runs network of nodes using [io-sim](https://github.com/input-output-hk/io-sim), a Free-monad based framework for simulating Haskell runtime
* See Philip Kant's [presentation at BobKonf 2022](https://www.youtube.com/watch?v=uedUGeWN4ZM)

## Hydra - Properties

Original [research paper](https://eprint.iacr.org/2020/299.pdf) defines several key properties

![Conflict-free liveness](/images/hydra-conflict-free.png)

---

Properties are (manually) expressed as _Dynamic Logic_ formulas

```haskell
conflictFreeLiveness = do
  anyActions_
  getModelStateDL >>= \case
    Open{} -> do
      payment <- forAllNonVariableQ (nonConflictingTx st)
      tx <- action $ Model.NewTx payment
      eventually (ObserveConfirmedTx tx)
```

::: notes

* Some details omitted for readability

:::

---

![Hydra Property Execution](/images/hydra-property-execution.png)

## Peras

* Fast finality protocol for Cardano
* Early work integrating research, formal methods, engineering...
* Use `quickcheck-dynamic` to produce _Executable Specification_ from _Formal Specification_

::: notes

* Peras is a WIP, paper not yet published so I won't share details about the protocol itself
* We are using the project also as a way to improve our approach to formal and executable specifications, trying to link both worlds

:::

----

The slogan is:

> Agda Proofs are Quickcheck Tests

----

![Development Workflow](/images/peras-workflow.jpg)

::: notes

* Formal specification in Agda is our ground truth
* Part of the model and code used in the q-d tests is generated from Agda
* The general idea is that _Proofs in Agda become Properties in QuickCheck_

:::

----

![Testing Architecture](/images/peras-testing-architecture.png)

::: notes

* We use the same `RunMonad` which is parameterised by the underlying execution monad and the actual network interface to define `RunModel`

:::

----

Testing _Common Prefix_ property

```haskell
 describe "IOSim Network" $
   prop "Chain progress" $
     prop_common_prefix iOSimNetwork

 describe "Netsim Network" $
    prop "Chain progress" $
      withMaxSuccess 20 $
        prop_common_prefix netsimNetwork
```

----

Testing _Common Prefix_ property

```haskell
chainCommonPrefix = do
  anyActions_
  getModelStateDL >>= \Network{nodeIds} -> do
    anyAction
    chains <- forM nodeIds (action . ObserveBestChain)
    void $ action $ ChainsHaveCommonPrefix chains
```

# `quickcheck-dynamic`: Theory

## What is it?

* A library for _Property-Based_ testing of _stateful_ systems
* Developed by [Quviq](https://quviq.com) while working at [Input Output](https://iohk.io) on Cardano
* Open-sourced in 2022

## Principles

* Model programs as _labelled transition systems_
* Express properties as [Dynamic Logic](https://en.wikipedia.org/wiki/Dynamic_logic_(modal_logic)) formulas
* Generate sequence of actions respecting the property
* Run sequence of actions against SUT and find bugs!

::: notes

DL is inspired by Hoare's triples itself inspired by Hoare's triple

:::

## Model - Basic steps

Specify possibles _actions_ and `initialState`

```haskell
instance StateModel KV where
  data Action KV a where
     Put :: String -> Int -> MySystem ()
     Get :: String -> MySystem (Maybe Int)

  initialState :: KV
  initialState = mempty
```

## Model - Basic steps  {transition=none}

Generate actions according to the current state

```haskell
instance StateModel KV where
  initialState = ...
  arbitraryAction env state =
    oneof [ genWrite
          , genRead
          ]
    ...
```

## Model - Basic steps  {transition=none}

Define `nextState` transition

```haskell
instance StateModel KV where
  initialState = ...
  arbitraryAction = ...
  nextState state (Put k v) variable =
     insert state k v
  nextState state (Get k) variable =
     state
```

## Implementation

Relate the specification to an actual (monadic) implementation

```haskell
instance RunModel KV StoreM where
  perform state (Put k v) env = do { putStore k v }
  perform state (Get k) env = do { getStore k }



```

## Implementation  {transition=none}

Specify `postcondition`s that should hold after each `Action`

```haskell
instance RunModel KV StoreM where
  perform state (Put k v) env =  ...
  postcondition (before, after) (Put k) env v = pure True
  postcondition (before, after) (Get k) env v =
    pure $ v == lookup env k

```

## Dynamic Logic

* A _Modal logic_ to define properties over _traces_ of a system
* Exposed as both an _expression_  and _monadic_ DSL
* Provide combinators to relate results of actions to predicates over the state of the SUT

-----

![Dynamic Logic book](/images/dyn-logic-book.jpg)

## Dynamic Logic - Syntax

Modality: $[a]p$

* `After a p`

  > After action `a`, `p` holds

* `AfterAny p`

  > After _any_ action `a`, `p` holds

----

Constants: $\mathbb{0}$, $\emptyset$

* `Stop`

  > When execution `Stop`s, expression is _true_

* `Empty`

  > When no execution is possible, expression is _false_

----

Alternatives: $[a \cup b] p$, $[ a \cap b] p$

* `Alt Demonic f g`

  > After `a` or `b` `p` must hold

* `Alt Angelic f g`

  > `p` must hold `After` either `a` or `b`


::: notes

* Choice in expressions is always `Angelic` but it turns `Demonic` when exploring branches and asserting whether a sequence is stuck

:::

## Dynamic Logic - Monadic Syntax

Provide a more convenient syntax

```haskell
newtype DL s a = DL { ... }
  deriving (Functor, Applicative, Alternative, Monad)

action :: Action s a -> DL s (Var a)
anyAction :: DL s ()
anyActions :: Int -> DL s ()
getModelStateDL :: DL s s
...
```

----

Combinator to `Gen`erate arbitrary values within expression

```haskell
class  Quantifiable q  where
  type Quantifies q

  quantify :: q -> Quantification (Quantifies q)

forAllQ :: Quantifiable q => q -> DL s (Quantifies q)
```

## Examples

From [Thread Registry](https://github.com/input-output-hk/quickcheck-dynamic/blob/53e8839b6646af0d531a49b1f7ad75d80dfc06c4/quickcheck-dynamic/test/Spec/DynamicLogic/RegistryModel.hs#L34) example

Can register an unbound thread under a new `name`

```haskell
canRegister = do
  anyActions_
  name <- pickFreshName
  tid <- pickAlive
  unregisterNameAndTid name tid
  action $ Register name tid
```

----

Cannot register an already registered thread under a new `name`

```haskell
canRegisterNoUnregister = do
  anyActions_
  name <- pickFreshName
  tid <- pickAlive
  action $ Register name tid
  pure ()
```

## Running Properties

Tie _Dynamic Logic_ expression and _Actions_ execution into a `Property`

```haskell
forAllDL ::
     (DL.DynLogicModel s, Testable a)
  => DL s ()
  -> (Actions s -> a)
  -> Property
```

## Running Properties

Tie _StateModel_ and _RunModel_, interpreting `Action`s against the SUT

```haskell
runActions
  :: forall state m e
   . ( StateModel state
     , RunModel state m
     )
  => Actions state
  -> PropertyM m (Annotated state, Env m)
```

## Shrinking

Sequence of actions that fail are _shrank_ while respecting DL expression

* Shorten `anyActions_` traces
* Shrink `action` and `anyAction` data according to model's `shrinkAction`
* Shrink `Quantifiable` values generated
* `precondition` filters invalid sequence of actions

::: notes

* note the importance of shrinking to provide minimal counterexamples

:::

## Anatomy of a test run

![](/images/test-generation-flow-1.png)

## Anatomy of a test run {transition=none}

![](/images/test-generation-flow-2.png)

## Anatomy of a test run {transition=none}

![](/images/test-generation-flow-3.png)

## Anatomy of a test run {transition=none}

![](/images/test-generation-flow-4.png)

## Anatomy of a test run {transition=none}

![](/images/test-generation-flow-5.png)

## Anatomy of a test run {transition=none}

![](/images/test-generation-flow-6.png)

## Anatomy of a test run {transition=none}

![](/images/test-generation-flow-7.png)

## Testing for safety

* We often want to test that _nothing bad can happen_, eg. safety properties
* We can express that in the model through _Polarity_ of `Action`s:
  * A `Positive` action is valid w.r.t to the state and is expected to _succeed_ when run
  * A `Negative` action is invalid in current state and is expected to _fail_

## Testing for safety  {transition=none}

![](/images/test-fail-generation-flow-1.png)

::: notes

* Test sequence starts as before, with the initial state and some action matching a precondition

:::

## Testing for safety  {transition=none}

![](/images/test-fail-generation-flow-2.png)

::: notes

* An `Action` that _fails_ it's `precondition` but passes it's `failurePrecondition` can be included in the generated trace
* Usually, such a `Negative` is expected to not change the state, but one could want to track those failures nevertheless

:::

## Testing for safety  {transition=none}

![](/images/test-fail-generation-flow-3.png)

::: notes

Full trace is generated

:::


## Testing for safety  {transition=none}

![](/images/test-fail-generation-flow-4.png)

::: notes

* A `Negative` action must pass the `postconditionOnFailure` predicate for the test to succeed
* The result of the action, whether considered and `Error` or not is passed to the predicate

:::

## Testing for safety  {transition=none}

![](/images/test-fail-generation-flow-5.png)

::: notes

The rest of the test proceeds as before
:::

# Conclusion

## Reflecting on practical use

* Tests execution does not catch much bugs beyond some regressions
* _But_ test failures in development often pinpoints blind spots and misunderstandings
* Main benefit is to help us _clarify_ and _formalise_ our thoughts on the protocols
* It requires non-negligible investment to build and maintain

::: notes

* we have extensive coverage elsewhere so regressions are often caught in other parts of the system
* also requires specific skills and interests

:::

## Tips & Tricks

* Model does not have to be _unique_
  * $\Rightarrow$ _Tailor models to specific problems_
* Model is code hence can be buggy
  * $\Rightarrow$ _Test-drive your generators, shrinkers, doubles..._
* It's easy to delude yourself
  * $\Rightarrow$ _See it fail!_
* `StateModel` works with symbolic values
  * $\Rightarrow$ _Model expectations in postcondition_

## Tips & Tricks (contd.)

* Detailed modeling leads to complex code
  * $\Rightarrow$ _Abstract the uninteresting bits away_
* Acting on and observing internal state is tricky
  * $\Rightarrow$ _Make the SUT testable and observable_
* `IO` Dependencies make tests slow and brittle
  * $\Rightarrow$ _Abstract dependencies behind simulatable interfaces_
* Triggering environment failures
  * $\Rightarrow$ _Simulate runtime environment for fault injections_

## Tips & Tricks (end)

> Model-Based Testing work hand-in-hand with Test-Driven Development

::: notes

Also works for plain QC

:::

## What's next?

Development on quickcheck-dynamic is fairly active, with plans for:

* Parallel testing of properties for "race conditions" detection (à la [quickcheck-state-machine](https://hackage.haskell.org/package/quickcheck-state-machine))
* Improved shrinking
* More documentation, examples, use cases...

## Takeaway  {transition=none}

> A plan is useless, planning is everything
>
> Gal. von Moltke

## Takeaway   {transition=none}

> A model is useless, modeling is everything
>
> quickcheck-dynamic

## Questions?

![](/images/puzzled.jpg)
