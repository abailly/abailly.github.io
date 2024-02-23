---
title: Model-Based Testing with QuickCheck
author: Arnaud Bailly - @dr_c0d3
institution: Input Output Global
date: 2022-10-24
theme: virgil-black-iohk
revealjs-url: /reveal.js
---

## Agenda

* Why?
* Property-Based Testing
* `quickcheck-dynamic`
* Examples
* Conclusion

# Why?

## Obligatory Quote

![Dijkstra on Testing](/images/dijkstra-testing-quote.jpeg)

::: notes

* Not sure what Dijkstra meant here, was it a way to diminish the relevance of testing for "proving" program correctness?
* Don't take Dijkstra too seriously, he seems to have been a grumpy old man for most of his life
* Testing is critically important to _design_ and _develop_ any program

:::

## The Quest for Better Software

* _Test-Driven Development_ $\rightarrow$ _Property-Based Testing_
* How to better test stateful programs and systems?
* Formal specification and "programs proving" is very hard
* PhD on state-machine driven test case generation 20 years ago

::: notes

:::

## Goal(s) for this Talk

* Share experience using Model-Based testing
* Spark interest in the use of this family of tools
* Gather feedback on quickcheck-dynamic

::: notes

* blah

:::

# Property-Based Testing

## What is Property-Based Testing?

* Example-based testing $\rightarrow$ Property-Based Testing
* Generalise examples to express _properties_ of the SUT

::: notes

* Some minor differences with Haskell
* Types are declared with single colon character
* Type of parameters can be named, which is useful for both documentation and referring to those names as variables in in other parts of the signature

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

## PBT Core Feature

* Generate some number of [arbitrary](https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck.html#g:5) values
* When property fails, [shrink](https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck-Arbitrary.html#v:shrink) the failing input
* Provide smallest [counterexample](https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck.html#v:counterexample) upon failure

::: notes

* basic principles of PBT, works for any library out there

:::

## Additional Key Features

* [cover](https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck.html#v:cover) expected distribution of test cases
* Build [generators](https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck.html#g:8) from combinators and [generic](https://hackage.haskell.org/package/generic-random)
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


# quickcheck-dynamic


# Examples

## Hydra

* Modeling Layer-2 protocol for Cardano

# Conclusion

## Takeaways

## Things Not Covered

## Links

## Questions?

![](/images/puzzled.jpg)
