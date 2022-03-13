---
title: Why mocking is a good idea
subtitle: ... when done right
author: Arnaud Bailly
date: 2021-10-29
---

Someone wrote a [recent post](https://cs-syd.eu/posts/2021-10-22-why-mocking-is-a-bad-idea) debunking what the author thinks are _Mocks_. I say "thinks" because actually, what the author describes are not what _Mock objects_ really are or how they were thought to be used, as this post demonstrates.

# What are Mock Objects really?

The history of the _Mock object pattern_ for _Test-Driven Development_ is explained by [Steve Freeman](http://www.mockobjects.com/) its inventor. He wrotes, with Nat Pryce, the book [Growing Object-Oriented Software Guided by Test](http://www.growing-object-oriented-software.com/) which is a must read even if one does only functional programming. There's also a shorter [OOPSLA 2004 paper](http://jmock.org/oopsla2004.pdf) and an [XP 2000 paper](https://web.tecgraf.puc-rio.br/~ismael/Cursos/Cidade_MA/aulas/downloads/mockobjects.pdf). The very same idea was also exposed in the context of refactoring legacy code by Michael Feathers in his seminal [Working Effectively with Legacy Code book](https://understandlegacycode.com/blog/key-points-of-working-effectively-with-legacy-code/) which is another must read, even if one does functional programming.

The key insight about Mock objects dawned on me when I realised these were really _Design tools_, much like one harvests most of the benefits of TDD when she conceives tests as a force to drive software design and not merely as a safety net against regression. In other words, one _does not_ use Mock objects, or mock _interfaces_, only to replace a cumbersome dependency, but rather mocks roles that emerge from the needs of some piece or component of the system, to express the expectations of this component regarding some dependency without having to depend on the implementation details. Mocks are used to design _interfaces_ between moving parts of the system, or to let _seams_ appear as M.Feathers names those.

# The proof is in the pudding

Here is a concrete example drawn from my current project, Hydra, implemented in Haskell. The Hydra node we are building needs to interact with a [Chain](https://github.com/input-output-hk/hydra-poc/blob/2056b8c9ba441aebc396a4fe0f50a419d6ee7be3/hydra-node/src/Hydra/Chain.hs) component, both to submit transactions and to receive them. Instead of having to depend on the [somewhat complicated implementation details](https://github.com/input-output-hk/cardano-node) of a real Cardano node, we instead defined an _interface_ which expresses in a concise and implementation independent way the information we send and receive.

Because we are using a functional language, this interface is expressed as a function which itself takes other functions (callbacks), a recurring pattern we documented in an [Architectural Decision Record](https://github.com/input-output-hk/hydra-poc/blob/2056b8c9ba441aebc396a4fe0f50a419d6ee7be3/docs/adr/0007-with-pattern-component-interfaces.md):

~~~~~ {.haskell}
type ChainComponent tx m a = ChainCallback tx m -> (Chain tx m -> m a) -> m a
~~~~~

where

~~~~~ {.haskell}
type ChainCallback tx m = OnChainTx tx -> m ()

newtype Chain tx m = Chain  { postTx :: MonadThrow m => PostChainTx tx -> m () }
~~~~~

The details of the messages we are sending and receiving are defined as two separate data types, one representing outbound transactions, eg. transactions we'll post to the chain:

~~~~~ {.haskell}
data PostChainTx tx
  = InitTx {headParameters :: HeadParameters}
  | CommitTx {party :: Party, committed :: Utxo tx}
  | AbortTx {utxo :: Utxo tx}
  | CollectComTx {utxo :: Utxo tx}
  | CloseTx {snapshot :: Snapshot tx}
  | ContestTx {snapshot :: Snapshot tx}
  | FanoutTx {utxo :: Utxo tx}
~~~~~

and the other representing inbound messages, eg. transactions and errors observed on the chain:

~~~~~ {.haskell}
data OnChainTx tx
  = OnInitTx {contestationPeriod :: ContestationPeriod, parties :: [Party]}
  | OnCommitTx {party :: Party, committed :: Utxo tx}
  | OnAbortTx
  | OnCollectComTx
  | OnCloseTx {contestationDeadline :: UTCTime, snapshotNumber :: SnapshotNumber}
  | OnContestTx
  | OnFanoutTx
  | PostTxFailed
~~~~~

Had we coded in an Object-Oriented language, or used a [final tagless encoding](https://peddie.github.io/encodings/encodings-text.html), these would have been expressed as two separate interfaces with one method for each type. The important point here is that _we_ are in control of this interface, _we_ define the patterns of interactions with the external system we depend on and abstract away all the nitty-gritty details a dependency on a concrete implementation would entail. Our system is now _loosely coupled_ to the other system as we have [separated concerns](https://en.wikipedia.org/wiki/Separation_of_concerns).

This has allowed us to focus on the core functionality of our software, to build a complete implementation of the [Hydra off-chain protocol](https://iohk.io/en/research/library/papers/hydrafast-isomorphic-state-channels/), [demonstrate a network of Hydra nodes](https://www.youtube.com/watch?v=3D_SAC4nyVM) and have automated [end-to-end tests](https://github.com/input-output-hk/hydra-poc/blob/master/local-cluster/test/Test/EndToEndSpec.hs#L99) all without the hassle of dealing with a full Cardano node implementation. Instead, we have several _Mock_ implementations of the `Chain` interface described above suitable for various use cases:
- A very fast [in-process](https://github.com/input-output-hk/hydra-poc/blob/2056b8c9ba441aebc396a4fe0f50a419d6ee7be3/hydra-node/test/Hydra/BehaviorSpec.hs#L380) mock for Behaviour-driven testing of Hydra cluster,
- A [0MQ-based](https://github.com/input-output-hk/hydra-poc/blob/2056b8c9ba441aebc396a4fe0f50a419d6ee7be3/hydra-node/src/Hydra/Chain/ZeroMQ.hs) implementation that allows us to spin-up a cluster of Hydra nodes, completely mocking an [external chain](https://github.com/input-output-hk/hydra-poc/blob/2056b8c9ba441aebc396a4fe0f50a419d6ee7be3/hydra-node/exe/mock-chain/Main.hs).

Note that:
- We are not side-stepping the integration problem as demonstrated by the fact we are also testing [interaction with Cardano node](https://github.com/input-output-hk/hydra-poc/blob/2056b8c9ba441aebc396a4fe0f50a419d6ee7be3/local-cluster/test/Test/LocalClusterSpec.hs#L51), and we definitely will enhance our End-to-End tests to "Close the loop" once the [Chain tests](https://github.com/input-output-hk/hydra-poc/blob/2056b8c9ba441aebc396a4fe0f50a419d6ee7be3/local-cluster/test/Test/DirectChainSpec.hs#L30) demonstrate the _concrete implementation_ of our abstract transactions work correctly on a real Cardano network,
- We have been careful to [Mock types we own](https://testing.googleblog.com/2020/07/testing-on-toilet-dont-mock-types-you.html) in order to not fall into the trap of relying on a dumbed down and probably wrong implementation of some system we depend on.

# There is more to it

This whole idea has been applied in a couple other areas of the system, most notably the [Network](https://github.com/input-output-hk/hydra-poc/blob/2056b8c9ba441aebc396a4fe0f50a419d6ee7be3/hydra-node/src/Hydra/Network.hs) interface: Here again we express some requirements from the point of view of the Hydra node, letting those emerge from tests we write.

While it does not respect the _Mock types you own_ mantra, we have also used this technique to great profit leveraging the [io-sim](https://github.com/input-output-hk/ouroboros-network/tree/3f16f617f8ada5e0e8f560b5b2d9635ec0d803f3/io-sim)  and [io-classes](https://github.com/input-output-hk/ouroboros-network/tree/3f16f617f8ada5e0e8f560b5b2d9635ec0d803f3/io-classes), as exposed in [another ADR](https://github.com/input-output-hk/hydra-poc/blob/2056b8c9ba441aebc396a4fe0f50a419d6ee7be3/docs/adr/0005-use-io-sim-classes.md). This has allowed us to test-drive the development of the protocol in an outside-in way, expressing expected [observable behaviour](https://github.com/input-output-hk/hydra-poc/blob/2056b8c9ba441aebc396a4fe0f50a419d6ee7be3/hydra-node/test/Hydra/BehaviorSpec.hs#L123) in a cluster of nodes, in a safe and fast way, as pure functions.

Of course, this is a dangerous path to tread and we need to also run tests with the real _Multi-threaded Runtime System_ to ensure proper coverage, like the End-to-end tests I already talked about and [load testing](https://github.com/input-output-hk/hydra-poc/tree/2056b8c9ba441aebc396a4fe0f50a419d6ee7be3/local-cluster/bench).

# Conclusion

I hope this post managed to convince the reader that using _Mock objects_ is actually a good idea as soon as one embraces it the way it's been intended to be used, namely as a _Test-driving technique_ also called _London School TDD_, and not as a mere technical artifact to ease testing _after the fact_. As I already advocated a while ago TDD has a [fractal dimension](https://abailly.github.io/posts/tdd.html): It can, and in my opinion must, be applied at all stages of a system's development and at all level of abstractions.

"Mock objects" is just a name for the technique that lets one work outside-in, precisely and formally (in code) expressing and testing each component first in isolation, but within a broader context: As soon as a concrete, production-ready implementation of an interface is ready, it should be integrated in the higher level tests. This is also something we wrote down in an [ADR on Testing strategy](https://github.com/input-output-hk/hydra-poc/blob/2056b8c9ba441aebc396a4fe0f50a419d6ee7be3/docs/adr/0012-testing-strategy.md) as it deeply impacts the architecture of the system.
