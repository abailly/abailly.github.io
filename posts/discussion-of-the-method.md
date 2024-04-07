---
title: On "Discussion of the Method"
author: Arnaud Bailly
date: 2024-04-05
---

> I am indebted to [Christophe
> Thibaut](https://www.linkedin.com/in/christophe-thibaut-35b4657/)
> for discovering this author and book. He has been using the
> terminology and ideas exposed in it to reframe his own perception
> and practice of software engineering, and is exposing those ideas in
> a very approachable and actionable way in his various posts on
> LinkedIn. If you read French and are interested in putting in
> practice ideas from this book, do yourself a favor and follow
> Christophe's posts.


[Billy Vaughn Koen](https://en.wikipedia.org/wiki/Billy_Koen)'s book is the latest, expanded version of a line of work and thought that spans decades. A much more compact version of the central ideas exposed in the book can be found in [this article](https://www.tandfonline.com/doi/pdf/10.1080/03043798808939429).

The first part of the book consists in proposing  a specific definition of the _engineering method_, providing several concrete and enlightening examples and counter-examples, and then comparing alternative widely accepted definitions and refine the definition along the way. The second part of the book takes a more philosophical turn and arguments for the universality of the engineering method as exposed in the first three chapters, or the _Universale Organum_ as the author calls it, in direct reference to Francis Bacon's [Novum Organum](https://en.wikipedia.org/wiki/Novum_Organum) which itself was referencing Aristotle's  [Organon](https://en.wikipedia.org/wiki/Organon).

## The Engineering Method

The book starts with this definition of the engineering method:

> The strategy for causing the _best change_ in a _poorly understood_ situation with the _available resources_.

The various parts of this definition states that engineering is an activity that needs to reach some goals while dealing with uncertainty and constraints. And the goal itself is subject to this uncertainty and constraints, as hypothesis made initially about it and how to reach it can be invalidated later on.

### Heuristics

Engineering, or designing a path to some desirable state, is therefore characterized by the use of _heuristics_. A heuristic:

* Does not guarantee a solution to some problem exists,
* May contradic other heuristics,
* Reduces search space (and time) for solving problems,
* Is contextual.

In the case of engineering, Billy Koen proposes a few generic heuristics that engineers have found useful over time:

1. At some point in a project, freeze the design, eg. rather than keep
   searching for a perfect solution, improve an existing solution
2. Allocate resources as long as the cost of not knowing exceeds the
   cost of finding out. This contradicts scientific approach where
   _knowing_ is the most important thing. In engineering a solution
   can be discarded because of _cost_ (time, money, resources) to
   elaborate
3. Allocate sufficient resources to the _weak link_
4. Solve problems by successive approximation. You want a large scale
   view of the "problem" and a sketch of a solution before delving
   into details, and then later refine the details. Interestingly, the
   author notes that:

   > This strategy does not appear to be commonly used by humans. The
   > clear tendency is to begin at the beginning of the problem, to
   > march through the project and, when you got to an end, stop.

### State-of-the-Art

But a heuristic is never used alone, it's part of a group of
heuristics for a given engineer at some given point in time, from
which the engineer can pick and choose whatever heuristic suits their
purpose (and the selection process is itself a _heuristic_ by the
way).

This group of heuristics is called a _State-of-the-Art_, abbreviated as a _sota_, and is the second most important concept from the book. A sota is situated knowledge, limited to some domain, time, and person. This allows B.Koen to refine his definition of engineering's first rule:

> The rule of engineering is in every instance to choose the best heuristic for use from what my personal sota takes to be the sota representing best engineering practice at the time I am required to choose.

Importantly, there's no single sota but a whole universe of sotas: Every engineer has their own sota, the union of all the sotas of all the engineers for some domain, plus some is a _sota_, there are various levels of collective sotas, etc.

There's no _absolute_ "best solution" or "best practice" that would be discoverable by some prescribed rational process, and a heuristic only approximates it. And this leads the author to state that engineering is best characterized as the use of heuristics, that this definition is the only one that adequately expresses all the nuances of engineering, and actually as the second part of the book tries to demonstrate, all human activity.

Over time various conceptions of engineering have been advocated for, even though engineering itself is a topic that's rather ignored by philosophers and thinkers:

* Defining engineering as _problem-solving_ and _goal-oriented_ activity, à la Polya, suffers from a kind of teleological argument: It postulates a pre-existing goal, a problem definition, whose very existence is also dependent on some _heuristic_, on someone's sota
* Defining it as following some process, in a "morphological" way as B.Koen puts it, suffers from the problem there's many competing morphological approaches who bear an air of similarity but are different,
* Suggesting that engineering is _applied science_ is also wrong, as while engineers _often_ use science they don't always do, sometimes deferring to rule of thumb, tribal knowledge, or even value judgments for taking design decisions,
* Focusing on engineering as _trial and error_ also falls short as it clearly does not account for the high success rate of engineering efforts.

This last aspect is interesting and highlights the role of the sota as both a toolbox from which engineers draw to make progress and design things, and as a limiting factor into what is considered doable. The sota is basically a _World view_ (or _Weltanschauung_ to be pedantic) and therefore it both enables and limits what its bearer can do or think of doing. These boundaries can and are being pushed, and engineers know how to anticipate and bet on time when designing solutions, but you can only go so far.

## Universale Organum

B.Koen then proceeds to enlarge the scope of heuristics and consider things that are usually assumed as stable and well defined like _time_, _language_, _science_, _mathematics_ and whole slew of concepts, as heuristic, adopting a radical skeptic stance, to the point where he can conclude that _All is heuristic_. This discussion and analysis comprises about half of the book and draws on domains like quantum mechanics, Gödel's incompleteness theorems, philosophy of the mind, linguistics, psychology, to systematically undermine the stability and "absolutness" of every single concept that usually makes up our world view.

I found this part way too long, somewhat confusing, quite pretentious, and ultimately ending with a boring conclusion: Taking about 100 pages to defend and demonstrate a somewhat naive form of relativism wasn't necessary, especially as it draws from a number of well-known and analysed paradoxes and counterintuitive ideas. In particular, I found surprising and disappointing that a nuclear engineer by trade choose quantum mechanics and its classical paradoxes to critique our sense of reality and our notions of time and space. The phenomenological approach, which B.Koen seems to advocate through his radical skepticism, can be used as demonstrated by [Michel Bitbol](https://cv.hal.science/michel-bitbol?langChosen=fr)'s works to completely dissolve those paradoxes: Schrödinger's cat, EPR, or Wigner's friend mind bending experiments are paradoxes only under a strong realist stance, when one adopts "God's viewpoint" and are dissolved when one realises that an observation only makes sense for an observer. And if one wants to avoid subjectivism, there are other available interpretations like [Carlo Rovelli](https://en.wikipedia.org/wiki/Relational_quantum_mechanics)'s relational interpretation that also address those paradoxes.

## A practical use for sota

At [SoCraTes Rennes 2024](https://socrates-rennes.github.io) Christophe Thibaut introduced the audience to the concepts exposed in this book, and the consequences on his work as a coach and consultant he has drawn from it. In particular, C.Thibaut suggests that a lot of frictions and problems which our industry is rife with like quality issues, unexpected delays, rework from miscomprehensions in "requirements", teams misalignment, come from _conflicting sotas_ between the three main actors of a software development project: The management, the product, the engineers.

To address those problems he proposes a particular kind of workshop whereby different actors will make their respective sotas explicit by enumerating various practices, techniques, tools and principles they each consider important for the success of the project. By _mapping_ the individual sotas onto a shared space on a board, conflicting, misaligned, or simply surprising differences in world-views will emerge and help objectivise what's often left subjective. It's not uncommon within teams that relatively small misunderstandings, which could be probably addressed and fixed when taken early and "objectively", grow over time leading to over or open conflicts.

This is akin to the [Varieties of human work](https://humanisticsystems.com/2016/12/05/the-varieties-of-human-work/) analysis which analyses the problems created by misalignment between _Work-as-done_, which is the actual, concrete activity carried on by people in the field, and others characterisation of work like _Work-as-prescribed_, the set of rules which the work _should_ follow, _Work-as-disclosed_ or how we talk and describe the work, or _Work-as-imagined_ which is how we think the work is done.

## Conclusion

The part of the book that focuses on engineering is very relevant to our job as software engineers, and clearly exposes the flaws that any kind of "first principle", mathematical, or scientific approach to engineering bears. Engineering is _not_ applied science as is often claimed, and by the way science is not even "science" either as the author later on advocates[^1].

By acknowledging the fact we all draw from a set of heuristics which have their limits in space, domain and time, that there's no unique or absolute principle that should guide our engineering and design work, that more often than not the very act of defining the goal of some particular endeavour is part of the engineering work, that the division between those who sets the goals and those who work to achieve it is artificial and counterproductive, the book advocates for engineers to refrain from "absolutising" what is essentially situated in time and space and therefore pay more attention to the social aspects of their work, both within their teams or organisations, and within society in general.

[^1]: The interested reader might want to read Latour's [Science in Action](https://en.wikipedia.org/wiki/Science_in_Action_(book)) or [Aramis](https://en.wikipedia.org/wiki/Aramis,_or_the_Love_of_Technology) which provide thorough analysis of how intertwined society and science are.
