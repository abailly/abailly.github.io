---
title: Against (Formal) Method?
subtitle: An experience report on Formal Methods from a developer point-of-view
author: Arnaud Bailly - @dr_c0d3
institute: Pankzsoft
date: 2025-03-14
theme: virgil-black
revealjs-url: /reveal.js
---

# Introduction

## Agenda

* Context & experiments
* Findings & Analysis
* Possible solutions

## Who am I?

* Dev/Tech Lead/Architect/Consultant for 30+ years
* PhD in computer science (20 years ago)
* Dedicated _eXtreme Programming_ Practitioner
* Skeptic believer in the benefits of formal methods

## Goals

How can we build better software?

* Share experience integrating Formal Methods as part of Software Development Life-Cycle
* Share analysis from this experience in the light of my understanding of software development
* Identify _issues_ and _shortcomings_
* Suggest _solutions_

::: notes

* better = build the software right + build the right software
* opinions and analysis are my own and are debatable

:::

## FUNARCH '2024

![FUNARCH'2024](/images/funarch-paper.png)

::: notes

* previously published paper at Workshop on Functional Architecture w/ James Chapman and Polina Vinogradova
* summarizes various other projects
* contains some findings and analysis shared here

:::

# Experience Report

## Cardano

A research-based proof-of-stake blockchain and cryptocurrency based
on _Ouroboros Praos_ protocol

![Ouroboros Praos](/images/praos-paper.png)

----

* Globally distributed and fully decentralized _open_ system
* 3000+ block producing nodes, 1M+ "wallets", 100s of _developers_ and _startups_
* Daily transactions amounting to 100s of millions USD
* _Security & safety are not an option_

::: notes

* not an endorsment of economic model or libertarian values

:::

## R&D

**Turn research papers into working software**

* [Hydra](https://hydra.family): Isomorphic state channels for Cardano (aka. Layer 2)
* [Mithril](https://mithril.network): Stake-based multisigned snapshots
* [Peras](https://peras.cardano-scaling.org): Faster settlement for Ouroboros
* [Leios](https://leios.cardano-scaling.org): Higher throughput for Ouroboros

::: notes

* only list projects I have been personally involved with

:::

-----

Common theme:

* More or less (more) complex algorithms and protocols w/ proven properties
* Written by cryptographic & security researchers, aka. _mathematicians_, with heavy proof apparatus
* Require collaboration of people with diverse background and skills

<!-- ## Hydra/Mithril -->

<!-- * No attempt at working with formal methods -->
<!-- * Heavy emphasis on Property-based testing & Test-driven development -->
<!-- * Used _some_ model-based testing approach -->
<!-- * After-the-fact formal specification in LaTeX -->

<!-- ---- -->

<!-- ![Hydra formalization](/images/hydra-formal-spec.png) -->

## Peras

* One project within _Innovation streams_
* Experiment and refined structured _method_ to go from research ideas to products
* Small (3.5 people) team: Researcher, FM engineer, 2 x Architects/Developers

::: notes

* Product is somewhat mising in the picture but let's not dive to deeply into this
* Method replicated for Leios projects

:::

## Process & Tools

![Peras detailed workflow](/images/peras-process.jpg)

----

* _Agda_: Formal specification language
* _Agda2HS_: Generate Haskell code from Agda specification
* _quickcheck-dynamic_: Haskell code to generate conformance tests
* _Haskell_ and _Rust_: Target languages for prototypes

## Agda formalisation

* Protocol modelled in Agda using [Small-steps semantics](https://peras.cardano-scaling.org/agda_html/Peras.SmallStep.html) specifying the impact of each node "actions" on global state
* Took inspiration from previous work on [Formalizing Nakamoto-Style Proof of Stake](https://eprint.iacr.org/2020/917.pdf) in Coq

----

![Agda Specification](/images/peras-agda-spec.png)

## Peras conformance tests

* 💡 Write a "simpler" model for testing purpose in Agda
* Generate code suitable for use with Haskell QuickCheck
* Write [Proof of soundness](https://github.com/input-output-hk/peras-design/blob/main/src/Peras/Conformance/Soundness.agda)

----

![Peras testing](/images/peras-conformance-testing.png)

<!-- ## Cardano-ledger conformance tests -->

<!-- * 💡 Write an Agda model (specification) -->
<!-- * Generate Haskell code using standard MAlonzo "opaque" code generator -->
<!-- * Use generated model to test conformance of (Haskell) implementation -->

<!-- ## Outcome -->

<!-- https://tweag.github.io/cardano-peras/peras-design.pdf -->

# Shortcomings

## Maintenance

* Most formal specifications are written once and rarely touched upon
* Software necessarily evolves over time to suit the need for changing requirements, users, environment
* How do we keep formal specifications and FM artefacts maintenable over time?

----

![Coq Proof fragment](/images/pos-nsb-proof.png)

## Skills shortage

* FM are rarely touched upon before graduate level
* There's a plethora of tools and languages to choose from, each with its own (small) ecosystem
* Upskilling general purpose programmers to use FM takes time
* Even switching from one method to an other is non trivial

## Tools

* Tooling is not on par with tooling for general purpose programming languages
* "Market size" effect -> building tools for niche languages does not attract big players
* "PhD quality software" -> software written to support research ideas or projects, not "industrial" software

::: notes

* `agda2hs` is basically incompatible with `stdlib` -> they serve 2 different purposes
* it's not maintained by a dedicated team

:::

## Silo

![Silos](/images/silo.jpeg)

----

* FM work requires specialised skills and knowledge
* FM on the critical path of software development introduces delays
* _The DBA effect_
* _Ivory Tower_ architects

## Researchers & Formal Methods

* Researchers are not FM engineers
* Writing "Pen & paper" proofs is _very different_ from writing mechanized proofs in Agda or Coq
* How are we sure the formal specification matches the paper?

## Cost & time

* Writing proofs for non-trivial properties is time consuming (took 4 months to prove soundness property for Peras)
* This time impacts total development cost

## Engineering/FM

* "Agile" software development techniques have become more and more widespread
* How do we integrate FM in an iterative and incremental process?

# Conclusion

## It's just another _Domain_

![Relating Proofs & Programs](/images/proof-program.png)

----

* Researchers are domain experts whose ideas we want to turn into software
* _Domain Driven Design_ emerged in the past decade as a great (set of) tool for software development
* Creating and evolving an _Ubiquitous Language_ is a key ingredient in DDD

----

> Can Formal Methods be useful as a Better Ubiquitous Language?

## Call to action

* Need to improve tooling
* Teaching and training for "the rest of us"
* Gradual integration of FM in software lifecycle (work on small feature or components)
* Consolidation of the FM landscape
* Integration with LLM and generative AI?

## References & Links

* Peras [website](https://peras.cardano-scaling.org) and [code repository](https://github.com/input-output-hk/peras-design) contain details about the project
