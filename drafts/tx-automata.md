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

What's also obvious is the combinatorial blow-up this explicit representation entails: Every pair of independent transactions can happen in any order, which means that if all transactions are independent then the number of states and transitions of the automata is exponential in the number of transactions.

However, transactions being _independent_ implies they can be processed by concurrent processes, with dependent transactions being  synchronizing points between those concurrent processes. What we can do then, is _decompose_ the global automata and, taking into account the dependencies between transactions, find _local automata_ whose "composition" provides the same behaviour. By composition, we mean more precisely composition through _synchronisation product_, a particular operator which we'll precisely define later.

From the previous dependencies statement, we can define one automata to just be `tx1` followed by `tx4`
![](/images/automata2.png)

and another one to have possible interleaving of `tx1` and `tx2`, followed by `tx3`
![](/images/automata1.png)

Note that this decomposition is not necessarily unique, nor minimal. In that particular case we could have introduced another decomposition in three automata:

* One automata with `tx1 -> tx4`,
* One with `tx1 -> tx3`,
* One with `tx2 -> tx3`.

The finest grain decomposition would be one where each individual automaton would have either one transition, if the transaction it represents is independent, or two transitions if one depends on the  other. Then the _synchronisation product_ will take care of producing a global trace from those simple, atomic atutomata.

What this gives us is a drastic reduction in complexity as we can now reason locally, on each atomic automata, to understand the sequential part of the behaviour of the system, and only take care of the composition and global state to reason on the synchronised part, for example to detect deadlocks or loopholes.

## Formal Model

Note: These definitions are drawn from [Partial Commutation and Traces](https://www.researchgate.net/publication/280851316_Partial_Commutation_and_Traces).

Formally, given an alphabet $\Sigma$ and an independence relation $I \subseteq \Sigma \times \Sigma$ a _trace language_ $T$ is a subset of the _free partially commutative monoid_ denoted $\mathbb{M}(\Sigma, I)$. A _trace_ denoted $[w]$ is the equivalence class of words in $\Sigma^*$ under the congruence relation $\cong_I$ induced by independence set $I$, where

$$ab \cong_I ba, (a,b) \in I.$$
