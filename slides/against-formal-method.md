---
title: Against (Formal) Method?
subtitle: An experience report on Formal Methods from a developer point-of-view
author: Arnaud Bailly - @abailly.bsky.social
institute: Pankzsoft
date: 2025-03-14
revealjs-url: /reveal.js
---

# Introduction

## Agenda

* Introduction
* Context & Experiments
* Findings & Analysis
* Takeaways & Conclusion

## Where do I speak from?

* Dev/Tech Lead/Architect/Consultant for 30+ years
* PhD in computer science (20 years ago)
* Dedicated _eXtreme Programming_ Practitioner
* _Cautious believer_ in the benefits of formal methods
* Experience limited to _specific_ types of software

## Too Long; Didn't Stay

Formal Methods (FM) are not a _Silver Bullet_ but a useful tool that
can bring value to most software development efforts

* _Proving software correctness_ is still out of reach for most teams and systems
* FMs can be introduced incrementally in the _Software Development Lifecycle_ (SDLC)
* FM can help grow and maintaing a powerful _Ubiquitous Language_

::: notes

* better = build the software right + build the right software
* opinions and analysis are my own and are debatable

:::

# Context & Experience

## Why use Formal Methods?

* Fun: Because it's so cool...
* Computer science: Study type systems, mathematics, programming languages, etc.
* Applied science: Back research with machiqne-checkable proofs of stated properties
* Software quality: Provide strong safety guarantees ⇒ _make the software right_
* Software design: **Improve design with better models**  ⇒ _make the right software_

::: notes

DDDEurope keynote about Boeing 737 Max design failures: https://2025.dddeurope.com/program/the-boeing-737-max-when-humans-and-technology-dont-mix/

:::

## Cardano

Key features:

* Globally distributed and fully decentralized _open_ system w/ 3000+
  block producing nodes and 100s of _developers_
* _Security & safety are critically important_
* Established tradition of working with Formal Methods
* Research plays a key role in the system's development

::: notes

* not an endorsment of economic model or libertarian values

:::

## R&D Projects

Projects I worked on had a common theme:

* More or less (more) complex algorithms and protocols w/ proven properties
* Written by cryptographic & security researchers, aka. _mathematicians_, with heavy proof apparatus
* Require collaboration of people with diverse background and skills
* Strong safety and/or liveness requirements

<!-- ## Hydra/Mithril -->

<!-- * No attempt at working with formal methods -->
<!-- * Heavy emphasis on Property-based testing & Test-driven development -->
<!-- * Used _some_ model-based testing approach -->
<!-- * After-the-fact formal specification in LaTeX -->

<!-- ---- -->

<!-- ![Hydra formalization](./images/hydra-formal-spec.png) -->

----

<h4>**How do we turn research papers into reliable working software?**</h4>

----

![Relating Proofs & Programs](./images/proof-program.png)

----

💡 Researchers are Domain experts

::: notes

Domain in the sense of DDD

:::

## Peras

* One project within _Innovation streams_
* Experiment and refine structured _method_ to go from research ideas to products
* Small (3.5 people) team: Researcher, FM engineer, 2 x Architects/Developers

::: notes

* Product is somewhat mising in the picture but let's not dive to deeply into this
* Method replicated for Leios projects

:::

## Process & Tools

![Peras workflow](./images/peras-process-simple.png)

----

* _ΔQ_: Network performance formalism
* _Agda_: Formal specification language
* _Agda2HS_: Generate Haskell code from Agda specification
* _quickcheck-dynamic_: Haskell code to generate conformance tests
* _Haskell_ and _Rust_: Target languages for prototypes

## Agda as specification language

* Protocol modelled in Agda using [Small-steps
  semantics](https://peras.cardano-scaling.org/agda_html/Peras.SmallStep.html)
  specifying the impact of each node "actions" on global state
* Took inspiration from previous work on [Formalizing Nakamoto-Style
  Proof of Stake](https://eprint.iacr.org/2020/917.pdf) in Coq
* Heavy emphasis on producing a _readable specification_

----

![Agda Specification](./images/peras-agda-spec.png)

## Agda driving conformance tests

![Peras testing](./images/peras-conformance-testing.png)

::: notes

* Write an executable model for testing purpose (_Agda_)
* Generate code suitable for use with _QuickCheck_
* Write [soundness proof](https://github.com/input-output-hk/peras-design/blob/main/src/Peras/Conformance/Soundness.agda) relating the two models

:::

<!-- ## Cardano-ledger conformance tests -->

<!-- * 💡 Write an Agda model (specification) -->
<!-- * Generate Haskell code using standard MAlonzo "opaque" code generator -->
<!-- * Use generated model to test conformance of (Haskell) implementation -->

<!-- ## Outcome -->

<!-- https://tweag.github.io/cardano-peras/peras-design.pdf -->

# Outcomes

## What went well

## A Better Standard

_Literate Agda_ formed the backbone of a [Cardano Improvement Proposal](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0140) standard specification.

![CIP-140](./images/peras-cip.png)

## Improved Feedback loop

* Formalisation (and prototyping) uncovered shortcomings in the protocol that lead to improvements
* Interaction of formal modeling and prototyping uncovered a few bugs in _both_
* Having a "small" formal model helped bootstrap [development beyond prototyping](https://tweag.github.io/cardano-peras/peras-design.pdf)

::: notes

* triangulate problem through FM and prototyping

:::

## Towards a "Security Research" DSL

!["Informal" pseudocode](./images/peras-pseudo-code.png)

----

!["Formal" pseudocode](./images/peras-pseudo-specification.png)

::: notes

* Researchers are not FM engineers
* Writing "Pen & paper" proofs is _very different_ from writing mechanized proofs in Agda or Coq

:::

## What could be improved

## Silos

![Silos](./images/silo.jpeg)

----

* Integrating FM engineering in the day-to-day activity of the team is not straightforward
* FM engineering is a specialty that's not (yet) widespread
* Creating silos is a slippery slope that leads to _DBA_ or _Ivory Tower architects_ situations

::: notes

* FM are rarely touched upon before graduate level
* Upskilling general purpose programmers to use FM takes time
* Even switching from one method to an other is non trivial

:::

## Coping with change

![Coq Proof fragment](./images/pos-nsb-proof.png)

----

<h4>How do we keep formal specifications and FM artefacts maintenable over time?</h4>

::: notes

* Most formal specifications are written once and rarely touched upon
* Software necessarily evolves over time to suit the need for changing requirements, users, environment

:::

## Tools & Process

* Tooling is not on par with "industrial languages"
* Research and industry needs and interests are not always aligned
* FM is a very fragmented landscape with mostly incompatible ecosystems

::: notes

* There's a plethora of tools and languages to choose from, each with its own (small) ecosystem
* `agda2hs` was mostly incompatible with `stdlib` -> they serve 2 different purposes
* it's not maintained by a dedicated team
* not always straightforward to learn

:::

# Conclusion

## Philosophical detours

![](./images/discussion-of-method.jpeg)

::: notes

* Everything is a heuristic
* State of the art is the set of heuristics known and accepted by an individual or a group at any point in time

:::

----

![](./images/against-method.jpeg)

::: notes

* "anything goes": there's not _one_ scientific method
* facts, hypothesis, and theories often start as ad hoc constructions that got strengthened or destroyed over time
* epistemic anarchism

:::

## Takeaways

----

<h4>Use formal specification to interact with domain experts as early as possible</h4>

::: notes

* In the case of Peras, we started working even before the paper was written
* This also implies publishing readable spec as early as possible and use feedback to improve communication aspects
* Counterexample: Agda ledger specification was not meant for external consumption, turns out to be hard for engineers interested in implmenting => did not pass the "ubiquitous language" test

:::

----

<h4>Model-based Testing is a great way to introduce formal languages and methods</h4>

::: notes

* Use FM as models for unit/integration/property/ETE testing
* particularly suited for state-machine based testing (with the usual caveat on how to handle concurrency)
* Can evolve into a conformance test suite usable across the organisation/ecosystem

:::

----

<h4>Start small, focusing on important/critical components of the system</h4>

::: notes

* Peras: test model for block production with limited adversarial power, ignore crypto/certificates aspects

:::

----

<h4>Ensure collective code ownership ⇒ training, pairing, mobbing, mentoring</h4>

::: notes

Not an advice peculiar to FM!!

:::

----

<h4>Select one tool and stick to it (but select wisely)</h4>

::: notes

* Peras/IOG: settled on Agda, perhaps not the best choice out there but at least it becomes consistent across the whole company
* Some parts of the company worked with Isabelle/HOL, Lustre, Lean4, Coq => isolated efforts
* Contributions are often more than welcome, communities are small and eager to share and learn (personal experience with Idris)

:::

----

<h4>Do not put proofs on the critical path of software delivery</h4>

::: notes

* Formal proofs of complex properties can be extremely tricky and lengthy to write

:::

## Santa's List to the FM Community

* Improve tooling and developer experience
* Lower the barrier of entry through more accessible and "practical" training material
* Consolidate the formal languages and methods landscape

::: notes

* integrate with standard and widely used tools, eg. Docker, VS Code, easy to use web services, CI (e.g Github actions)
* compatibility across tools through shared representations, ontologies perhaps? => express a state machine in Agda and export it to TLA+
* work on error messages !!
* avoid far-reaching breaking changes (example: Idris1 --> Idris2)

* documentation for the rest of us (tutorials, howtos) focused on the _pragmatics_ not the theoretical aspects of the FM
* concrete examples and success story people can relate with (eg. like this one!)
* A "generalist" developer should be able to _maintain_ an Agda/Idris/Coq/Whatever codebase 80-90% of the time

Probably wishful thinking but it would be great if not every university had its own language and tooling, or if those tools

:::

## Related work

![FUNARCH'2024](./images/funarch-paper.png)

::: notes

* personal take from this paper and own experience
* previously published paper at Workshop on Functional Architecture w/ James Chapman and Polina Vinogradova
* summarizes various other projects
* contains some findings and analysis shared here

:::

----

Peras [website](https://peras.cardano-scaling.org) and [code repository](https://github.com/input-output-hk/peras-design) contain details about the project

## Thanks

* My colleagues at IOG from whom I learnt a lot
* BOBKonf organisers for inviting me
* Josselin Auguste, Bertrand Bougon, Emmanuel Gaillot, Pascal Grange, Fabien Lamarque, Xavier Maso, Matthias Neubauer, and Hugo Traverson for improving it
* Christophe Thibaut for the inspiration
* **You**

----

<h2>Questions?</h2>
