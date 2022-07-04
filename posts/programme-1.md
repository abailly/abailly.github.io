---
title: PROGRAMme Final Workshop
subtitle: Day 1
author: Arnaud Bailly
date: 2022-07-03
---

I have already published some notes on the [HaPoP-5](/posts/hapop-5.html) conference, here are more notes from the accompanying [PROGRAMme workshop](https://programme.hypotheses.org/spring-workshop-iii-what-is-a-computer-program-final-conference). [Tomas Petricek](http://tomasp.net/) and [Liesbeth de Mol](https://pro.univ-lille.fr/liesbeth-de-mol/) kindly asked me to take part in the workshop as a _respondent_ for one of the presentations, of which I am most grateful to them! Note the goal of the _PROGRAMme_ program (!) is to publish a - possibly constantly evolving - book to answer the question "What is a computer program?" This book currently lives in a private wiki as it's still a work-in-progress, with lot of unfinished chapters.

# Day 1 - PROGRAMme

## What is a computer program? An anti-disciplinary view

> presented by Liesbeth De Mol

* This is the introduction to the whole book, reminding it's an ANR funded project
* _anti-disciplinary_ vs. _trans-disciplinary_: The intent is not to let each discipline (logic, CS, philosophy, linguistics...) undisturbed but rather to
* The goal is to question the object that lies at the foundation, as programs have become transparent while software is "eating the world", to approach the problem with ontological glasses on
* Problems are also opportunities:
  > Wherein lies the danger, grows also the saving power.
  > _Friedrich Hölderlin._
* Programs evolve and their use, study, and make should be on equal footing. A program is like a [_pharmakon_](https://arsindustrialis.org/pharmakon), being both a cure and a poison depending.
  * There's a risk of alienation which has been identified, among others, by Simondon, when the use of a technological artefact becomes disconnected from its make
  * Programs _are historical_ and their invisibilisation is a process that increases the alienation they induce, yet we think about them with decades or even centuries old ideas
    > We march backwards into the future
    > _Marshall McLuhan

* In his response, Paul Edwards highlights there are 2 genres of computing history:
  * One concerned with _logic_, the evolution of concepts and the intellectual history of programming or calculability
  * Another one concerned with _machines_, taking an engineering and economic perspective
* _Computer Science_ is the discipline that connects _notations_ and _machines_, seeking (or producing) semiotic closure of the questions it delves with
  * Scientific disciplines are a construct from the XIXth century (see Foucault, _Les mots et les choses_)
  * Within a discipline there's no place for some questions and that's how boundaries between disciplines are erected
  * The tension between _notations_  and _machines_ is captured by the following [Greimas square](https://en.wikipedia.org/wiki/Semiotic_square):
    ```
    notations --------- machines
       |                   |
       |                   |
       |                   |
     systems ----------- logic
    ```

> I wholeheartedly support the intention of breaking the barriers disciplines erect around them, a process leading to the over-specialisation which has dire consequences on the whole of humanity. I think it's Gunther Anders who introduced the moral imperative that one should fullly understand the consequences of his or her actions as part of a system, or refrain from undertaking them. That software is both a boon and bane is now quite obvious and rethinking our, and others', relationship to programs, programming, and software is an important task to carry out.

## Classifying programs

> presented by Baptiste Mélès

* The unicity of the concept of program contrasts with the diversity of concrete programs. The goal of classification is also to not forget "minorities".
* Bottom-up classification approach, using _comparative analysis_ and unifying existing classifications
  * Not that there aren't that many classifications, presenter highlights Brooks' (System/Product) and [Lehman's](https://www.expressionsofchange.org/lehman-spe-classification/) (S/P/E)
* Top-down (a priori) classification seeks to escape "real-life mess"

> Like most of the chapters, this is a work-in-progress about which I don't have much to say.

## Logic – a declaration of independence?

> presented by Giuseppe Primiero

* Logic representation comes before the implementation of any program
* Draw on the analogy of "Logic as Motherland" and "Languages as colonies"
* Flow diagrams from Von Neumann was a first step
  * Logic is used as a specification, using mathematical and possibly recursive definitions naturally
  * This leads to the _Correctness problem_: Is an implementation correct w.r.t. its specificaiton? This involves proving invariants by going through a flowchart and checking the state each transition leads to
  * Floyd, 1967: Properties are independent of their physical realisations in a language
  * Hoard, 1969: Programming is an _exact science_ and we can apply deductive reasoning from the program
* Then come various _policies of control_ (Curry-Howard, logic programming, operational semantics...): How does Motherland enforces rules on colonies
  * Types were initially very different from logic
  * To what extent do colonies depend on Motherland?
* Adopt a pragmatic view: The nature of programs depend on the user or agent
  * Do programs have _habitus_?
  * Is there something like a "Sociology of programs"?
* The _Order of Concepts_ opposes the _Order of Things_
* A new role for _Formal Methods_: Not imposing regulations on what can or cannot be programmed, but hlping in constructing systems
  * More verifications leads to more fine-grained ontologies
* It seems there are many Motherlands: Logic, business needs, military
  * Are we not entering a _post-colonial_ phase of programming?

> I enjoyed how far we can get applying the Motherland/colonies pattern to CS and programming history. There is still this "provincial" feeling within CS w.r.t. Mathematics and Logic. It's not uncommon for programmers with strong math background to grow some form of superiority complex regarding their inferior brethens, the "mere" programmers and this attitude is especially rife in the FP circles. Putting the bonds between logic and programming in perspective, realising that type theory or formal methodes are just _one_ aspect of programs, is an important work to carry out.

## Notations. There is no escape

> presented by Tomas Petricek

**Note**: These notes are a mix of things from the chapter presented by Tomas and my own response to it. See the [slides](/slides/notations-escape.html).

* The ever growing proliferation of notations seems like a curse, as if they have escaped some Pandora's box
* They are the result of a _negotiation_ between humans and machines
  * We need a notation to be close to the way we think and express ourselves
  * But it must also be close to how machines actually work
* Having different notations for humans  and machines is a way to enforce division of labor & maintain hierarchy of thinkers and doers
* LISP and Smalltalk provides higher _notational programmability_

## Machines – Hide and seek

> presented by Maarten Bullynck

* [Brown & Carr, 1954](http://bitfragment.net/notes/proglang-src-brown-automatic-1954/) develops the idea of "Automatic Programming" into a general translation system, whereby the computer itself is used to translate high-level language into low-level instructions, something which was novel and actively resisted by some people at the time
* Computers become digital because we _tame the waves_ from analog signal
* Cloud computing and virtual machines seems to mark the triumph of software vs. hardware
* There is a need to _open the machines_

## Systems – the system if dead! Long live the system.

* Understand programs from a systems perspective
* A _system_ is a stabilised heterogeneous situation containing members, see [McIlroy, 1969](https://dl.acm.org/doi/pdf/10.1145/1115858.1115870)
* A program:
  * contains programs
  * interact with other programs (it's never alone)
* Programs:
  * are programmed
  * programming happens within programs
* States of the system are _opening_, _exchanging_, _closing_
* Maintaining invariants:
  * enabling multiple purposes, generality vs. applicability
  * managing compatibility: convenience vs. dependence
* Example of RUNCOM evolving to `/etc/rc.d` then `systemd`
  * systemd provides unity, separation of responsibilities of the system vs. applications
