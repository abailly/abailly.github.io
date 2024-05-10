---
title: On "A theory of facilitated variations"
author: Arnaud Bailly
date: 2024-05-02
---

I got referred to [The theory of facilitated variation](https://www.pnas.org/doi/full/10.1073/pnas.0701035104), by Gerhart and Kirschner, while reading [La survie des médiocres](https://www.gallimard.fr/Catalogue/GALLIMARD/Bibliotheque-des-Sciences-humaines/La-survie-des-mediocres) de Daniel S. Milo and found it interesting enough in its own right to be worth a short blog post.

With this paper I am quite far from my usual bases: I am not a specialist in biology, evolution, genetics, nor any of the topics it draws from, so if you dear reader notice a mistake, please let me know. But I think I got the gist of it and it resonated deeply with some ideas and experiences from the software world.

One of the puzzles that evolution and natural selection pose is an explanation of the wide phenotype variations observed in nature. Phenotype is the set of features exhibited by some organism and with which it interacts with its environment. The theory of evolution posits that all living organisms evolve, that this evolution is "undirected", and that natural selection kicks in to favor those variations that increase the "fitness" of the organism with respect to its environment. While this was unaccessible to Darwin at the time he formulated his theory, we now know that inheritable variations are caused by genetic mutations: Random variations in the genotype of an organism produce phenotypical variations, some which end up being _selected_ and passed down to descendants.

What's unclear however are the _rules_ governing those variations, and at what speed does it occur? Is it the case that significant phenotype variations like the one famously observed on Galapagos' finches by Darwin, are the result of many small repeated mutation/selection cycle? Or could it be possible that phenotypical traits evolve at a faster rate? The theory of _facilitated variations_ provides some evidence for the latter.

The key insight for this theory comes from the fact modern advances in genetics have proven that all living organisms share a substantial share of their genotypes, and that most of the key features of how life "works" at the molecular level were already in place in the pre-cambrian, in early prokaryote and eurkaryot organisms, before the subsequent explosion of life-forms. In other words, it seems clear that the wild array of phenotype variations observed in living and extinct species over the past billion of years is caused by a rather limited set of _core components and processes_ from DNA replication, RNA transcription and generation of proteins, to cell division and specialisation, body axis and lateralisation, etc.

The general idea is that large phenotype variations can be caused by tiny DNA mutations if those mutations entail changes in the _regulatory processes_ that link together various components of the living system, because those core components and processes are very versatile and adaptable. Or said differently, minor variations in genotypes can generate recombinations of components and processes that in turn lead to major variations in phenotype. This is made possible because those core components exhibit some key charateristics:

1. Weak regulatory linkage where some component's complex behaviour, typically a cell, can be triggered or inhibited by the presence or even the absence (through self-inhibition) of some simple molecule. The inhibitor or enabler does not need any "knowledge" or even direct relation with the other process, and

   > The ease with which simple signals can entrain complex processes reflects the capacity of core processes to engage in weak regulatory linkage.
2. State selection, the built-in capability of a component to be in several states depending on its environment and interactions. Those so-called conserved processes and components being very versatile and _encapsulating_ complex behaviour,
2. Exploratory capabilities whereby processes are able to "explore" a very large space to find suitable targets, similar to how ants look for food in their environment

Those characteristics make core components and processes _robust_ and _adaptable_ and as the authors put it:

> Robustness and adaptability are essential to the kind of evolution we have described, wherein core processes are used in different combinations, amounts, and states to produce new traits.

Moreover, these adaptative capabilities manifest themselves differently in different parts of the organisms, leading to even more differentiation, a phenomenon called _compartmentation_ by the authors.

Any software developer will recognize the characteristics cited above, albeit disguised under different names, and more generally the way variations of observable behaviour occurs through recombination of core components and processes, as key software design principles.

* Weak regulatory linkage is well-known as the _Loose coupling - Strong cohesion_ principle,
* State selection along with weak linkage is very reminiscent of what John Osterhout calls _Shallow interface - Deep module_ principle in his [Philosophy of software design book](https://www.amazon.com/Philosophy-Software-Design-John-Ousterhout/dp/1732102201),
* Exploratory processes are definitely akin to _iterative and incremental_ software development processes.

Noting the connection between how "Nature" designs life forms and how "humans" design artifacts is certainly not new, dating back to at least Aristotle and his conception of "Art as imitation of Nature", where art is to be understood as any form of human design including what we now call art, craft, architecture. However, Aristotelian conception is teleological as both Nature and Art are assumed to be driven by a _purpose_, whereas evolution and natural selection have no purpose being the result of complex interactions within and among ecosystems. Moreover, Aristotle mostly conceived Art and Nature as _static_: An artist realises some goal in some art form, a process known as morphogenesis that consists in imposing a shape on matter, but never has to cope with change. Similarly, Nature is fixed and not conceived to be evolving, the myriad of life forms having been created once and for all.

What this article - and theory of evolution in general - highlights is how "Nature" has "built" in the lowest level of life's components and processes how to more or less easily cope with _change_. And by contrast it strikes me as something we very often, in software design and development, overlook or pay lip service to. Far from having learned Darwin's and genetics' lessons, we are still very much "fixists" in how we conceive our systems.
