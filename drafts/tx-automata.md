---
title: UTxO as Asynchronous Automata
author: Arnaud Bailly
date: 2021-11-03
---

There's been somewhat of a stir recently in the [Cardano](https://cardano.org/) and crypto community as some doubts were raised about the concurrency level one could achieve with the UTxO model for Smart contracts applications. Being myself involved in the development of [Plutus-based](https://github.com/input-output-hk/plutus) application, I have had to go wrap my head around the [eUTxO](https://iohk.io/en/research/library/papers/the-extended-utxo-model/) programming model, which lead me to explore ways of designing such applications in an easier way, taking into account the inherent concurrency the eUTxO model entails.

Concurrency is hard to reason about, and there's been quite a few models invented since _Computer Science_ became a thing. Among those many models, the theory of [Mazurkiewicz traces](https://en.wikipedia.org/wiki/Trace_theory) presents the advantage of being "simple", for some definition of "simple", resting on the already well-known theory of regular languages and automata.

This short post investigates the relationship between a UTxO ledger and _Asynchronous automata_, a class of automata that can recognise trace languages.

## Working through an Example

Let's start with a simple example, some graph of (abstract) transactions consuming and producing UTxOs:

![Ledger transactions](/images/txs.png)

Among the 4 depicted transactions it's pretty obvious that:

  * `tx4` only depends on `tx1`,
  * `tx3` depends on both `tx1` and `tx2`.

One way to reason about the ledger is to think of transactions as _transitions_ in an automata (or state-machine) where the state is a set of UTxO. If we try to give such an automata theoretic representation of the above transactions, we get the following picture:

![Global automaton ($\cal{A}$)](/images/automata.png)

Here, what's obvious is not the _dependency_ between transactions but rather which ones are (pairwise) indeependent, a fact that's represented by the characteristic _diamond-shape_ of parts involving independent transactions. The concurrency inherent to this _independence relation_ is modelled as the _possible interleaving_ of all the transactions. This is interesting in itself because, in practice in a ledger, transactions are applied sequentially anyway.

What's also obvious is the combinatorial blow-up this explicit representation entails: Every pair of independent transactions can happen in any order, which means that if all transactions are independent then the number of states and transitions of the automata is exponential in the number of transactions.

However, transactions being _independent_ implies they can be processed by concurrent processes, with dependent transactions being  synchronizing points between those concurrent processes. What we can do then, is _decompose_ the global automata and, taking into account the dependencies between transactions, find _local automata_ whose "composition" provides the same behaviour. By composition, we mean more precisely composition through _synchronisation product_, a particular operator which we'll precisely define later.

From the previous dependencies statement, we can define one automata to just be `tx1` followed by `tx4`

![Local automaton 1 ($\cal{A}_1$)](/images/automata2.png)

and another one to have possible interleaving of `tx1` and `tx2`, followed by `tx3`

![Local automaton 2 ($\cal{A}_2$)](/images/automata1.png)

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

From our previous example we can define the trace language $\cal{L}$ over alphabet $\Sigma=\{\texttt{tx}_1,\texttt{tx}_3,\texttt{tx}_3,\texttt{tx}_4\}$ and independence relation $I = \{(\texttt{tx}_1,\texttt{tx}_2),(\texttt{tx}_2,\texttt{tx}_4), (\texttt{tx}_3,\texttt{tx}_4)\}$, as

$$\cal{L} = \{\lbrack \texttt{tx}_1, \texttt{tx}_2,\texttt{tx}_3,\texttt{tx}_4\rbrack\}.$$

_Zielonka's Theorem_ is a deep result from trace theory that states a _trace language_ is _recognizable_ if and only if it is recognized by some _asynchronous automaton_. An asynchronous automaton over a set of processes $J$ is an automaton $\cal{A} = (Q,\Sigma,\delta,q_0,F)$ where

* $Q = \prod_{i \in J} Q_i$, where $Q_i$ is a the _local state_ of process $i$,
* $\Sigma$ is the alphabet, with a function $\textrm{dom} : \Sigma \rightarrow J$ defining a _domain_ function as the subset of $\Sigma$ local to each process,
* $q_0 \in Q$ is the initial state,
* $F \subseteq Q$ are the final states,
* $\delta \in Q \times \Sigma \times Q$ is the transition relation over global state resulting from the "conjunction" of each transition relation $\delta_i$ for each process $i$. In other words,
  $$\delta((q_1, .., q_n), a) = (q'_1, .., q'_n) \Leftrightarrow \delta_i(q_i, a) = q'_i, \forall i\in J$$

Note this means the transition relation is defined for some state $q$ and some letter $a$ iff it is defined for _all_ $\delta_i$. It is customary to turn the (partial) $\delta_i$ relations into functions by adding a sink state $q_\bot$ and  transitions:

1. $\delta(q,a) = q$ if $i \not\in \mathrm{dom}(a),$
1. $\delta(q,a) = q_\bot$ otherwise.

The link between a rational trace language and an asynchronous automata is complex to establish formally, but intuitively the idea is that the independence relation $I$ represents letters from $\Sigma$ that can commute with each other, thus expressing concurrency through interleaving. Letters are dependent when their order is constrained by at least one process.

Then, given a set $\cal{A}_i, i \in J$ of local automata, defined over an alphabet $\Sigma$, we can construct an _asynchronous automaton_ using the _synchronization product_ $\cal{A}_{i_1} \otimes \cal{A}_{i_2} \dots \otimes \cal{A}_{i_n}$. The synchronization product operator $\cal{A}_i \otimes \cal{L}_j$ can be defined informally as the cartesian product of the two automata, _synchronized_ on the letters in $\Sigma_i \cap \Sigma_j$, where $\Sigma_i = \{ a \in \Sigma \mid a \in \text{dom}(i))\}$:

* Transitions $(q_i, q_j) \xrightarrow[(a,b)]{} (q'_i, q'_j)$ are possible iff either $a \not\in \Sigma_i$, $b  \not\in  \Sigma_j$, or $a=b$.

In other words the shared letters of each automata represent _synchronization points_ where they are required to advance in lock step, otherwise being free to run locally in isolation.

In our ongoing example, we have $\Sigma$ defined as before and

* $\Sigma_1 = \{ \texttt{tx}_1, \texttt{tx}_4 \},$
* $\Sigma_2 = \{ \texttt{tx}_1, \texttt{tx}_2, \texttt{tx}_3 \},$
* $\cal{A} = \cal{A}_1 \otimes \cal{A}_2.$

The _synchronization product_ gives us a way to define implicitly the behaviour of a global system as some composition of the behaviour of each local system, in a constructive way.

## So What?

Writing fancy mathematics in $\LaTeX$ is cool but one could ask: What's the point of all this formalisation? Does it solve any practical problem? While I don't have a definitive answer to this question, I believe that in the context of a UTxO based blockchain it could be interesting to explore how we can use the idea of _synchronized product_ and _asynchronous automaton_ to build large systems from smaller ones taking advantage of the inherent _concurrency_ introduced by the UTxO model.

When designing a smart-contracts based application, the state machine model is appealing as a way to structure the relevant sequence of transactions, so much so that it's been baked in Plutus as a specific [State Machine module](https://github.com/input-output-hk/plutus-apps/tree/40e8e93dec35cebb830af317b1ddb64d275014e2/plutus-contract/src/Plutus/Contract/StateMachine). However, such a state machine is inherently _sequential_ and does not take into account the possibility of multiple actors _synchronised_ through the blockchain. In such a case, we could still model the interaction of the application with each actor as a single sequential state machine and reason about the global behaviour of the system usign the _synchronised product_ of a finite number of such state machines. While it's prohibitively expensive to effectively build the asynchronous automaton, we could build the automaton _on-line_, eg. while _running_ each of the individual automata. This approach would give us a way to explore the resulting system, either for generating test cases and check the actual execution on chain, or to simulate this execution and ensures some properties are "ensured".
