---
title: HaPoP-5
subtitle: History and Philosophy of Programming
author: Arnaud Bailly
date: 2022-06-21
---

I have a long standing interest for Philosophy, and I was thus delighted when [Tomas Petricek](https://twitter.com/tomaspetricek) proposed me to join the _Program Committee_ for the [HaPoP-5](https://www.shift-society.org/hapop5/) conference, more than two years ago. The conference was initially supposed to take place in 2020, co-located with NCrafts, but both events were cancelled because of _You Know What_. Here are some notes along with personal comments about each of the conference's sessions. Another blog post will cover my notes from the [PROGRAMme workshop](https://programme.hypotheses.org/spring-workshop-iii-what-is-a-computer-program-final-conference) which took place in the same location right after HaPoP-5.

## Warren Sack - Miniatures, Demos and Artworks: Three Kinds of Computer Program, Their Uses and Abuses

Warren Sacks is the author of [Software Arts](https://mitpress.mit.edu/books/software-arts)

* Those three kinds of programs look superficially close but are actually very different artefacts
* _Miniatures_ are like _philosophical statements in code_, the goal is to rewrite a system in a miniature form in order to clarify what's specific about it, its "essence"
  * [Inside Computer Understanding](https://www.amazon.com/Inside-Computer-Understanding-Miniatures-Intelligence-ebook/dp/B00J4BRAMM) is a great example, it's a book containing the description of 5 systems along with their miniatures
  * Another example is Minix built for the purpose of teaching and understanding operating systems
  * Notebooks, literate programming are other examples of miniatures
* _Demos_  are _rhetoric_ in nature, demonstration in the prime sense (proving) very often to the purpose of getting more funding
* _Artworks_ are _aesthetic_ endeavours, of course. Software art is inscribed in the history of art esp. the emergence of modern and then conceptual art which detaches the artwork from its concrete realisation
* Miniatures are pedagogical, demos persuasive, artworks pleasurable

> I don't know much about software arts but this talk gave me the desire to discover more of it. I liked the attempt at classifying various artifacts which share the common theme of _Programming-in-the-small_ but serve very different purposes. It seems to me however that this classification does not do justice to the "Demo scene" whose artifacts lie at the intersection of all three classes.
>
> Another very interesting category of programs which would have been worthy of analysis is what I would call "Constrained Competitive programming", things like the [IOCCC](https://www.ioccc.org/), or programs written in very alien languages. There is an _exploratory_ dimension that would deserve a category of its own.

## Shoshana Simons - Programming practice as a microcosmos of human labor and knowledge relations

* The main thesis of this work is that technical practices are not separate from the rest of the world and are historically situated which implies the programmer embeds its worldview in their work
* Starting examples are:
  * Runtime analysis of a program completely ignores the actual labor involved in executing the program
  * Buying an apple at a grocery store ignores and hides the labor involved in bringing the apple, shrinkwrapped, to the store
  * The _command-line_ metaphor reflects the division of labor between those who give command and those who execute them
* Technical practice is hegemonic, it's a training ground for capitalism
* There are 2 main approaches to ethics esp. as it relates to programming and tech:
  * Ethics as values: People have values they impose on otherwise neutral data/technologies
  * Ethics as consequences: Technology is a given, what are its impacts and how to make it work for the greater good
* These 2 views keep technology as a _black-box_ they are like the 2 sides of a same coin, the input and output of "function" whose workings are ignored
* The _How_ matters, we need to open the black-box if we really want to improve our ethical approach to tech
* Practice _Critical Journaling_ which is a daily reflection on one's own relation to tech, critiquing what is unethical in it and hopefully improving it

> Another interesting and somewhat provocative talk, even though the critique of the idea that scientific theories, and even more so practices, are a-historical and independent of the social context in which they are produced is somewhat mainstream nowadays. Although this talk takes a more political stance on the question, this reminds me of early Latour's work, esp. in [La science en action](https://www.editionsladecouverte.fr/la_science_en_action-9782707145468) in which he demonstrates how scientists are not pure minds completely detached from the mundane practicalities of this world, like money, power status, ego...
>
> I find teaching the practice of _Critical Journaling_ a great idea, and also the emphasis on "unpacking" the black-box relevant in a world where technology is unquestioned and appears somewhat "magical", with its impacts on the real world and real people's life being ignored (think how food delivery startups take advantage of loopholes in law to employ workers without paying the full benefits and taking the responsibilities an employer has, how AirBnB's pricing algorithms impact housing prices in metropolis...)

## Lucas Bang - Program Size: A Call for Inquiry

https://mitpress.mit.edu/books/making-sense

* Blum's Size theorem states that a total program equivalent to a non-total program would suffer from an exponential blow-up in size. Intuitively, non-total languages allow one to "compact" the behaviour of a program
  * Computing GCD takes 2 lines in Python but 400 in (total) Idris
* A general program could be:
  1. Most general purpose PL are too general
  2. Let's write more programs into restricted (total) PL
  3. Blum's theorem might bite you
* "If you want to optimise your code, you have to write more code"
* We need developers to pay more attention to program size

> I was completely ignorant of Blum's theorem and even though its results are somewhat obvious, the fact it talks about total programming languages is very interesting and resonates with my personal investigations on [practical use of Dependent Types](./dependently-typed-accounting.html).
>
> The "call to arms" to developers to start taking care of their program size makes of course a lot of sense, and it's well-known that program size is [good measure for complexity](https://www.researchgate.net/publication/220204439_Cyclomatic_Complexity_and_Lines_of_Code_Empirical_Evidence_of_a_Stable_Linear_Relationship) or at least as good as any other measure, and taming complexity is a major problem in software engineering.
>
> In a way, Lucas is advocating for more polyglot programming, whereby teams and projects will use the "right tool" for the job and not reach out for whatever hammer that happens to be "ready-at-hand" at one point in time for everything. Using total languages (eg. Idris), or _restricted languages_ ([Dhall](https://dhall-lang.org/) or [Grace](https://github.com/Gabriella439/grace)), for the relevant parts of a system entails significant advantages in terms of quality, testing, security (eg. reduction of attack surface). My intuition is that those advantages, in the long run, offset the drawback of having to manage several languages/toolchains.

## Andre Dickson - The disturbance of death and debt

* Major disturbances of programs are
  * Death (Peter Naur, [Programming as Theory Building](https://pages.cs.wisc.edu/~remzi/Naur.pdf), or [here](https://gist.github.com/dpritchett/fd7115b6f556e40103ef) for a markdown)
    > The death of a program happens when the programmer team possessing its theory is dissolved. A dead program may continue to be used for execution in a computer and to produce useful results. The actual state of death becomes visible when demands for modifications of the program cannot be intelligently answered. Revival of a program is the rebuilding of its theory by a new programmer team.
  * Conceptual Integrity (Brooks)
  * Debt (Ward Cunningham)
* In Naur's views, programs die over time because the programmers lose understanding of the underlying theory, or because there is no theory
  * Naur's theory is implicit whereas [Ryle](https://plato.stanford.edu/entries/ryle)'s explicit
  * The problem with Naur's view is that theories erase the program as an _equipment_ (in phenomenological sense following Heidegger), a set of tools, rules and languages to make the practice of programming explicit
* _Programs are equipments_

> This talk was one of the most philosophical of the conferences, drawing on Heidegger's phenomenology to criticize, or shed a different light on Naur's and Cunnigham's notions and how they "disturb" the "lifecycle" of a program. We have had a follow-up discussion by mail with the presenter which somehow clarified his point of view. In my words, the idea is that a program (and the whole environment in which it is taken care of by programmers) is an _equipment_ in the sense of Heidegger.
>
> This talk prompted me to think again about this _conceptual integrity_ concept, and to somehow reject Peter Naur's (and Brooks') idea that a program is somehow the "embodiment" or the "realisation" of a _theory_. This feels a lot like classical Aristotelian shape/matter duality which in my opinion is a common plague of our industry and possibly of our civilisation as it conveys the idea that what _matters_ most is the shape, the idea, the theory that one bestows on matter in a god-like gesture, like how antique gods modelled humanity out of clay and infused them with life.

## [Daniel Kuby](https://twitter.com/danielkuby) - Towards a linguistic conception of programming

* Linguistics is concerned with human languages whereas prog languages are the realm of _formal languages_ theory
* What is a PL? We can take the machine centric view or use the formal language sense, but talking about *language* for PLs is a metaphor
  * Linguistics make use of formal languages but they are not its subject of study
* Goal of the talk is to reframe PL as _natural languages_. According to studies in [cognitive linguistics](https://www.researchgate.net/publication/333481200_The_Language_of_Programming_A_Cognitive_Perspective) programming activates the same brain areas than _talking_ and not _problem solving_
  * [Nofre et al., 2014](https://www.researchgate.net/publication/263743154_When_Technology_Became_Language_The_Origins_of_the_Linguistic_Conception_of_Computer_Programming_1950-1960) provides an inquiry into the links between linguistics and programming
* The fact that the design of PL is linguistic engineerings is new only in philosophy. Famous quote from [Abelson and Sussman](https://en.wikipedia.org/wiki/Structure_and_Interpretation_of_Computer_Programs) about readability of programs dates back to the 80's
* There are 2 traditions that take interest in specific languages:
  * Wittgenstein  and his primitive language then later _language games_
  * Frege and the microscope analogy about math/science notation vs. ordinary language
  * Poverty/simplicity of a language does not mean _deficiency_ but _fitness for use_
* This leads to the notion of _Restricted Languages_ related to applied linguistics, the study of _usage_ of languages
  * _restriction_ is an operator on _natural language_ to produce sublanguages

|              | Spontaneous       | Engineered           |
|:-------------|:------------------|:---------------------|
| Restricted   | sublanguages      | Controlled languages |
|              | (medicine)        | eg. PL               |
| Unrestricted | Natural languages | Planned languages    |
|              |                   | eg. Esperanto        |

> We keep talking about _languages_ in the context of programming but outside of CS this is understood asa metaphor and linguistics usually ignores programming languages. I like the idea of applying linguistic tools and categories to PL, considering those as part of a continuum of symbolic systems. Us practitioners are always concerned with the readability and expressivity of our programs, perhaps a little too much sometimes. Perhaps this line of research will provide insights that could inform future languages development, or possibly engineering of DSLs.

## Nicolas Nalpon and Alice Martin - Why semantics are not only about expressiveness - The reactive programming case

* Concerned with the _epistemic_ difference (and cost) of different semantics: How a given formal semantics represent a system and its behaviour matters for the end user
* Having identical semantic expressiveness does not imply identical semantic representation
* Example: Reactive programming for UI. To give a semantic to sush a system requires dealing with 2 distinct parts: An algorithmic part and a reactive part. Experiment with 2 formalisms:
  * [Bigraphs theory](https://core.ac.uk/download/pdf/82274932.pdf) is a formalism from Robin Milner (of CCS and π-calculus fame)
  * λ-calculus
* Possible solution is using hybrid semantics, eg. different fwks for different parts of the system at the cost of increasing complexity in code generation
* What's the quantitative impact of various formalisms wrt to length of derivation/execution steps?

> Going beyond the obvious but useless Turing completeness yardstick to compare formal languages makes a lot of sense, and the example provided while simple makes a good case about how different representations can have different "epistemic" costs. I am too sure about the usability of bigraph formalism which I am unfamiliar with but if I think about something simpler like π-calculus, the classical reduction of λ-calculus to it (and vice-versa) already demonstrates how different notations for different aspects of system can be more or less painful to work with. It's another good argument for polyglot programming, this time when it comes to formal specifications.

## Robin Hill - Hello World? An Interrogation

* HW is about writing  complete program, end-to-end
* It's about helping students' relationship with code to evolve from [present-at-hand_ to _ready-at-hand](https://eternalisedofficial.com/2021/02/02/ready-to-hand-and-present-at-hand-heidegger/)
* Why investigate HW?
  * Wikipedia has a page that exists since 2001 and has been constantly updated
  * There's a common metric to compare PL, the _Time To Hello World_ or TTHW
  * HW is a _meme_
* HW does nothing, it has nothing to do about programming and is born of expedience. It is stripped of all the "interesting" parts of programming (eg. algorithms, problem solving...)
* It stems from the fact students need first hand experience, to learn basic things that hard to articulate and make the programming environment _ready-at-hand_
  * For example, HW shows that programming is about _ordering_ (do one thing, then one thing)
  * Code is _materiality without matter_, it presents possibilities and resistances
* There are analogs from other crafts like stitching fabrics together when learning to sew
  * Goal is to show _how_ and _that_
* Important realisation is that coding involves code to do coding -> _we use programs to write programs_ which is something specific to this activity
* programming = materiality + computational thinking ?

> The last talk of the conference proposes a phenomenological inquiry into the humble yet ubiquitous "Hellow World" program.
>
> I found it interesting this analysis lead to the somewhat obvious but yet ignored truth that _coding involves code_: Even though it's possible to write programs with pencil and paper, to actual _code_ and _program_ requires some other program to actually do something. HW is the first artefact a wannabe developer encounters that helps to acquire that insight. It reminds me of how puzzled I was before I learnt systems programming, when I reading books about Computer hardware and CPU, that were hinting at assembly or C code and I was pretty much clueless on how to actually _write_ and _execute_ those. I only have ever programmed using high-level applications like Multiplan or Access and did not know there was another reality behind those applications.
>
> Robin Hill's work, while seemingly simple, highlights this fact there is a _huge_ amount of implicit knowledge behind even the simplest task of programming.
