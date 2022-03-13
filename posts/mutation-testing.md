---
title: Mutation-Based TDD
subtitle: Developing Plutus contracts with QuickCheck
author: Arnaud Bailly
date: 2022-01-31
---

# Smart Contracts & Cardano

I discovered the concept of _Smart Contracts_ in 2016 in Singapore when I met the nice folks at [Legalese](https://legalese.com/), Meng Wong and Alexis Chun. This is what attracted me to blockchain much more than the cryptocurrency and speculation space: The fascinating idea of designing software that would run in a fully decentralised and distributed manner, representing evolving contractual obligations and rights over time, and reacting to consensual events added to the chain.

Since then I have worked on a private blockchain based platform at [Symbiont](https://www.symbiont.io/), designing and building a [mortgage servicing system](https://www.symbiont.io/mortgages) using Symbiont's [SymPL](https://www.symbiont.io/post/safety-and-ease-of-use-in-sympl-a-dsl-for-enterprise-smart-contracts) ; and recently joined [IOG](https://iohk.io/) which is the company developing the core technology of the [Cardano](https://cardano.org/) blockchain and cryptocurrency.

In 2021 [Plutus](https://plutus.readthedocs.io/en/latest/), Cardano's native smart contract language, was made available on mainchain. Plutus is basically a lambda-calculus, and thus a Turing-complete language, with which developers write _scripts_ that can lock [eUTxO](https://files.zotero.net/eyJleHBpcmVzIjoxNjQzNDY2MzE0LCJoYXNoIjoiYTVhYmY4NjdiY2E2YzdkNTNjODkwNWNmZDZhYmM5MjAiLCJjb250ZW50VHlwZSI6ImFwcGxpY2F0aW9uXC9wZGYiLCJjaGFyc2V0IjoiIiwiZmlsZW5hbWUiOiJDaGFrcmF2YXJ0eSBldCBhbC4gLSAyMDIwIC0gVGhlIEV4dGVuZGVkIFVUWE8gTW9kZWwucGRmIn0%3D/156852d95f236fc19bf9615579d71dd7857ba06556a4b867adef6bfe7e5c4e1a/Chakravarty%20et%20al.%20-%202020%20-%20The%20Extended%20UTXO%20Model.pdf). The scripts are evaluated when a transaction _consumes_ such a eUTxO and the transaction is considered valid iff each validator evaluates to  _True_ in the context of the given transaction.

Of course, the correctness of the validators' code aka.  _Smart Contracts_ is of the utmost importance as they can control large amount of funds and coordinate complex processes involving a large number of parties. The Smart contracts space is infamous for quite a few exploits, some of them [famous](https://en.wikipedia.org/wiki/The_DAO_(organization)) and having resulted in significant losses, hence the ability to test, verify, validate, audit Plutus code is critical. While we wait for proper formal verification tools to mature, we have to resort to standard practices like auomated testing and manual auditing, hence as _Smart Contracts Developers_ we need to be extra-careful with this part of the code and put ourself in the shoes of potential "Attackers" that could try to harm users in various ways: Steal currencies, Denial-of-Service, lock funds...

# Hydra Smart Contracts

With my fellow colleagues working on the [Hydra](https://github.com/input-output-hk/hydra-poc) Layer-2 protocol for Cardano, [Mathias Benkort](https://www.linkedin.com/in/matthias-benkort-47186a57) and [Sebastian Nagel](https://www.linkedin.com/in/sebastian-nagel-2bb43a1a/), we decided in October 2021 to move away from PAB and Plutus Application Framework in the development of the Hydra on-chain validators and experiment with so-called _Direct Chain_ interaction: Use the standard cardano-api protocols and data structures to interact with the blockchain, posting transactions and following the chain as new blocks get created and new transactions added.

After having spent some time setting up the needed infrastructure to build, post and observe Hydra-relevant transactions to and from a cardano-node, we went back to revisit our earlier work on Contracts and implement the full "Happy path" Hydra lifecycle, from initialising a Head to _Fan-out_ and redistribution of UTxO created within the Head.

## Test-Driving Hydra's Validators

Being Test-Driven Development addicts the first question raised was then: How do you test-drive Plutus smart contracts? There's a growing set of tools developers have at their disposal to test and test-drive Plutus contracts:

1. The "official" [Model-based testing](https://plutus-apps.readthedocs.io/en/latest/plutus/tutorials/contract-testing.html) framework which is part of the plutus-apps repository
    * Scope is complete Plutus apps, which are tested at the level of the `Contract` monad, eg. including both on-chain and off-chain code,
    * Tests are generated based on a _state machine model_ of the system,
    * It uses QuickCheck and [quickcheck-dynamic](https://github.com/input-output-hk/plutus-apps/tree/main/quickcheck-dynamic) framework to explore state machine, generate traces and check correctness of implementation,
    * Tests are run within an `Emulator` that's supposed to reproduce the behaviour of the blockchain.
2. [plutus-libs](https://github.com/tweag/plutus-libs/) is another model-based testing approach also based on QuickCheck from [Tweag](https://www.tweag.io/blog/2022-01-26-property-based-testing-of-monadic-code/), called `cooked-validators`:
    * It provides own `MonadBlockChain` abstraction to represent off-chain interaction with the blockchain, which ultimately is based on Plutus' representation of the ledger's types,
    * Tests are written as properties over trace expressions written in a `GenT` monad allowing interleaving generators and chain interactions like posting transactions,
    * _modalities_ `somewhere` and `everywhere` provide a way to modify generated traces to produce more traces representing some arbitrary change over the set of traces. This is a powerful concept akin to _Temporal logic_ modal operators, see [this example](https://github.com/tweag/plutus-libs/blob/main/examples/tests/PMultiSigStatefulSpec.hs#L272) for a use of `somewhere`,
3. [tasty-plutus](https://github.com/Liqwid-Labs/plutus-extra/tree/master/tasty-plutus) provides a unit and property testing framework integrated with [Tasty](https://hackage.haskell.org/package/tasty)
    * It's based on a DSL to build a `ScriptContext` that can then be used to run the validators directly,
    * Uses Plutus' `Scripts.runScript`  function to run the script,
    * The scripts are run in compiled form and passed to the CEK interpreter,

## Mutation-based Property Driven Development

We decided to explore another avenue, which we have called _Mutation-based Property Driven Development_ and which is as one can guess, a combination of _Property-based Testing_ with QuickCheck, _Test-Driven Development_, and _Mutation testing_. Traditional [Mutation testing](https://en.wikipedia.org/wiki/Mutation_testing) is a testing technique that introduces small modifications like changing a comparison operator, or modifying constants, into a program and checks whether or not the existing tests "kill" the produced mutants, eg. fail. Mutation testing requires somewhat complex tooling because it needs to modify the source code, in limited and semantically meaningful ways in order to generate code that won't be rejected by the compiler. A quick search lead me to [MuCheck](https://hackage.haskell.org/package/MuCheck) which seems to be the only available tool in Haskell-land and is quite old already, and beside we did not want to rely on esoteric tooling.

Plutus eUTxO validators are boolean expressions of the form:

~~~~ {.haskell}
validator : Datum -> Redeemer -> ScriptContext -> Bool
~~~~

All things being equal, "mutating" a _validator_ so that it returns `False` instead of `True` can be done:

* Either by _mutating_ the code of the `validator` implementation,
* Or by _mutating_ its arguments.

This simple idea lead us to the following strategy to test-drive each of our validator scripts, `Head`, `Commit` and `Initial`:

1. Start with a validator that always return `True`,
2. Write a _positive_ property test checking _valid_ transactions are accepted by the validator(s),
3. Write a _negative_ property test checking _invalid_ transactions are rejected. This is where _mutations_ are introduced, each different mutation type representing some possible "attack",
4. Watch one or the other properties fail and enhance the validators code to make them pass,
5. Rinse and repeat.

As this is really the most "novel" part here are some details about the _Mutations_ and the _Adversarial_ property we check.

## Generic Property and Mutations

The definition of the property is simple and completely generic way: Given a transaction with some UTxO context, and a function that generates `SomeMutation` from a valid transaction and context pair, this property checks applying any generated mutation makes the mutated (hence expectedly invalid) transaction fail the validation stage.

~~~~ {.haskell}
propMutation :: (CardanoTx, Utxo) -> ((CardanoTx, Utxo) -> Gen SomeMutation) -> Property
propMutation (tx, utxo) genMutation =
  forAll @_ @Property (genMutation (tx, utxo)) $ \SomeMutation{label, mutation} ->
    (tx, utxo)
      & applyMutation mutation
      & propTransactionDoesNotValidate
      & genericCoverTable [label]
      & checkCoverage
~~~~

To this basic property definition we add a `checkCoverage` that ensures the set of generated mutations covers a statistically significant share of each of the various possible mutations classified by their `label`.

The `SomeMutation` type is simply a wrapper that attaches a `label` to a proper `Mutation` which is the interesting bit here.

The `Mutation` type enumerates various possible "atomic" mutations which preserve the structural correctness of the transaction but should make a validator fail.

~~~~ {.haskell}
data Mutation
  = ChangeHeadRedeemer Head.Input
  | ChangeHeadDatum Head.State
  | PrependOutput (TxOut CtxTx Era)
  | RemoveOutput Word
  | ChangeInput TxIn (TxOut CtxUTxO Era)
  | ChangeOutput Word (TxOut CtxTx Era)
  | Changes [Mutation]
~~~~

The constructors should hopefully be self-explaining but for the last one. Some interesting mutations we want to make require more than one "atomic" change to represent a possible validator failure. For example, we wanted to check that the `Commit` validator, in the context of a `CollectCom` transaction, verifies the state (`Head.Input`) of the `Head` validator is correct. But to be interesting, this mutation needs to ensure the _transition_ verified by the `Head` state machine is valid, which requires changing _both_ the datum and the redeemer of the consumed head output.

## Transaction-specific Mutations

To be run the `propMutation` requires a starting "healthy" (valid) transaction and a specialised generating function. It is instantiated in the test runner by providing these two elements:

~~~~ {.haskell}
describe "CollectCom" $ do
  prop "does not survive random adversarial mutations" $
    propMutation healthyCollectComTx genCollectComMutation
~~~~

The interesting part is the `genCollectComMutation` (details of the `Mutation` generators are omitted):

~~~~ {.haskell}
genCollectComMutation :: (CardanoTx, Utxo) -> Gen SomeMutation
genCollectComMutation (tx, utxo) =
  oneof
    [ SomeMutation MutateOpenOutputValue . ChangeOutput ...
    , SomeMutation MutateOpenUtxoHash . ChangeOutput ...
    , SomeMutation MutateHeadScriptInput . ChangeInput ...
    , SomeMutation MutateHeadTransition <$> do
        changeRedeemer <- ChangeHeadRedeemer <$> ...
        changeDatum <- ChangeHeadDatum <$> ...
        pure $ Changes [changeRedeemer, changeDatum]
    ]
~~~~

Here we have defined four different type of mutations that are interesting for the `CollectCom` transaction and represent possible "attack vectors":

  * Changing the `Head` output's value, which would imply some of the committed funds could be "stolen" by the party posting the transaction,
  * Tampering with the content of the UTxO committed to the Head,
  * Trying to collect commits without running the `Head` validator,
  * Trying to collect commits in another Head state machine transition.

## Running Properties

When such a property test succeeds we get the following report which shows the distribution of the various mutations that were tested.

```
Hydra.Chain.Direct.Contract
  CollectCom
    does not survive random adversarial mutations
      +++ OK, passed 200 tests.

      CollectComMutation (200 in total):
      30.5% MutateOpenUtxoHash
      27.0% MutateHeadTransition
      23.5% MutateOpenOutputValue
      19.0% MutateHeadScriptInput

Finished in 18.1146 seconds
```

In the case of a failure we get a detailed report on the context of the failure:

```
 test/Hydra/Chain/Direct/ContractSpec.hs:96:5:
  2) Hydra.Chain.Direct.Contract.CollectCom does not survive random adversarial mutations
       Falsified (after 5 tests):
```
With details about the `Mutation` that was attempted:

```
         SomeMutation {label = MutateHeadTransition, mutation = Changes [ChangeHeadRedeemer (Close {snapshotNumber = 0, utxoHash = "\EOT\ETX\STX", signature = [000003]}),ChangeHeadDatum (Open {parties = [1], utxoHash = "\SOH\SOH\ETX\EOT\SOH\STX\NUL\STX\NUL\ETX\EOT\NUL\ETX\STX\ETX\SOH\SOH\NUL\ETX\EOT\ETX\ETX\ETX\SOH\EOT\EOT\ETX\SOH\STX\NUL\EOT\EOT"})]}
```
The failure itself:
```
         Phase-2 validation should have failed
         Redeemer report: fromList [(RdmrPtr Spend 0,Right (WrapExUnits {unWrapExUnits = ExUnits' {exUnitsMem' = 831248, exUnitsSteps' = 362274551}})),(RdmrPtr Spend 1,Right (WrapExUnits {unWrapExUnits = ExUnits' {exUnitsMem' = 1030658, exUnitsSteps' = 424175713}})),(RdmrPtr Spend 2,Right (WrapExUnits {unWrapExUnits = ExUnits' {exUnitsMem' = 1030658, exUnitsSteps' = 424175713}})),(RdmrPtr Spend 3,Right (WrapExUnits {unWrapExUnits = ExUnits' {exUnitsMem' = 1030658, exUnitsSteps' = 424175713}}))]
```
The UTxO that we used (possibly mutated):

```
     Lookup utxo: {
             "31237cdb79ae1dfa7ffb87cde7ea8a80352d300ee5ac758a6cddd19d671925ec#455": {
                 "address": "addr_test1wpjstex8ajlkn8sp5lr8dsfkn9v2m2pfudmn9kzy6epyegqk5664m",
                 "datumhash": "1f4e83d60d16d6bc976fa8d1d1a7a43f2fef540e643cc3c2cb5cd2d0d5052f06",
...
```

And most importantly the details of the transaction that failed, including all the relevant pieces of data (inputs, outputs, scripts, datums, redeemers):

```
         Tx: "83ad5c518d4adacf84f5f8fb17e0f2d175a76ae88494966360cb2e96a939f260"
           Input set (4)
             - 31237cdb79ae1dfa7ffb87cde7ea8a80352d300ee5ac758a6cddd19d671925ec#455
             - 96b5f154b0afc62c6a91d756ee31dfc219d76c08ebd30341c198e7b22533745e#179
             - d9c38f56d9147ba5ce4a0b52456ef4594c46992b74051e462ab8275845345e98#996
             - fb3d635c7cb573d1b9e9bff4a64ab4f25190d29b6fd8db94c605a218a23fa9ad#140
           Outputs (1)
             total number of assets: 0
             - 34.056295 ₳
           Scripts (2)
             total size (bytes):  12917
             - ScriptHash "6505e4c7ecbf699e01a7c676c1369958ada829e37732d844d6424ca0"
             - ScriptHash "97b5cb76fd4dcccdfcff850abbe7bdc95d69f70b7eeb1a1c33135ebd"
           Datums (6)
           ...
             - SafeHash "59e610cce1fb1636a27bdc6e65c2bf373c829f6c726140dcedbffc5fc950af1c" -> DataConstr Constr 0 [Constr 0 [I (-13)],List [I 18446744073709551597,I 4,I 21]]
           Redeemers (4)
           ...
             - DataConstr Constr 0 []
             - DataConstr Constr 0 []
             - DataConstr Constr 0 []
```

Note that this report could be made more friendly by trying to decode some of the `datums` and `redeemers` as we usually know what their actual type in code is, and making the association between redeemers and inputs more immediate. But even in this somewhat crude form it provides a wealth of information that makes it straightforward to manifest the shortcomings in the validators.

# Conclusion

We have applied this strategy to drive the development of the so-called "Happy Path" of the Hydra Head On-Chain Validation state machine, writing code for three different validators and five different transaction types. The early steps were a bit painful as applying mutations requires fiddling with the internals of a transaction in potentially complicated ways. It took us some time to define a good-enough set of "atomic" `Mutation`s and some good generators and helper functions, but we already had most of the API covered thanks to previous work on implementing _Direct_ chain interaction, but as with any framework-like effort, we were able to observe increasing returns over time: Defining new mutations for new types of transactions has become easier.

Writing Plutus validators lends itself pretty well to this technique:
  * The "universe of discourse" is relatively complicated (Cardano transactions are large data structures with lot of moving parts) and can fail in subtle and/or surprising ways, especially because of indirect interactions between contracts,
  * It is somewhat self-contained and the validators' code is in the end just a _predicate_ over some more or less complex data types,
  * It benefits from adopting an "Adversarial" mindset, trying to find interesting changes that should not be disallowed.

More generally, test-driving code using both mutations and properties seems to improve the quality of the "triangulation process" which TDD rests upon: QuickCheck generated counterexamples pinpoint exactly what's missing in the code, which can be fixed straightforwardly even using "fakes", but next run will find more counterexamples until enough coverage is reached.

I think this approach, while particularly well-suited to Plutus validators, has broader applicability on every development platform where there's support for Property-Based Testing.
