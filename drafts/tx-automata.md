------------
title: UTxO as Asynchronous Automata
author: Arnaud Bailly
date: 2021-11-03
------------

There's been somewhat of a stir recently in the Cardano and crypto community as some doubts were raised about the concurrency level one could achieve with the UTxO model for Smart contracts applications. Being myself involved in the development of [Plutus-based](https://github.com/input-output-hk/plutus) application, I have had to go wrap my head around the [eUTxO](https://iohk.io/en/research/library/papers/the-extended-utxo-model/) programming model, which lead me to explore ways of designing such applications in an easier way, taking into account the inherent concurrency the eUTxO model entails.

Concurrency is hard to reason about, and there's been quite a few models invented since _Computer Science_ became a thing. Among those many models, the theory of [Mazurkiewicz traces](https://en.wikipedia.org/wiki/Trace_theory) presents the advantage of being "simple", for some definition of "simple", resting on the already well-known theory of regular languages and automata.

This short post investigates the relationship between a UTxO ledger and _Asynchronous automata_, a class of automata that can recognise trace languages.

## Working through an Example

Let's start with a simple example, some graph of (abstract) transactions consuming and producing UTxOs:

![](/images/txs.png)

Among the 4 depicted transactions it's pretty obvious that:
  * `tx4` only depends on `tx1`,
  * `tx3` depends on both `tx1` and `tx2`.

One way to reason about the ledger is to think of transactions as _transitions_ in an automata (or state-machine) where the state is a set of UTxO. If we try to give such an automata theoretic representation of the above transactions, we get the following picture:

![](/images/automata.png)

Here, what's obvious is not the _dependency_ between transactions but rather which ones are (pairwise) indeependent, a fact that's represented by the characteristic _diamond-shape_ of parts involving independent transactions. The concurrency inherent to this _independence relation_ is modelled as the _possible interleaving_ of all the transactions. This is interesting in itself because, in practice in a ledger, transactions are applied sequentially anyway.
