------------
title: On the Mode of Existence of Software
author: Arnaud Bailly - @dr_c0d3
date: 2017-05-19
theme: serif-black
------------ 

# Genesis

----

# Ontology

-----

![](/images/aristotle.jpg)

## Hylemorphism 

<div class="notes">

* A form of dualism different from Platonism (*Physics*): Objects are the product of a *shape* encountering some *matter* (and living beings are the product of a *soul* encountering a *body*)≠
* Prominent view of software development process has been shaped by hylemorphism and essentialism: 
    * Software is a tool that is *used* to solve "real-world" problems in a more efficient way
    * The problem can be *specified* be *domain experts* then *implemented* by developers using some well-defined engineering process
    * This assumes there is an essence of the problem at hand that is independent of the fact we are using software
* This is embodied in the separation of the *design* vs. the *coding* vs. the *testing* vs. the *deploying* view... 
</div>

## Poeisis & Praxis

<div class="notes">

* Poiesis is production of something and is the domain of (slave) *labor* whereas *praxis* is action and the domain of freemen
* Praxis as a way to emancipate human beings from labor, see [André Gorz](/posts/gorz.html)
* Hannah Arendt values praxis as the way to change the society, above contemplation and theoretical work, she was the one who reactivated the distinction between poiesis and praxis
* considering writing software as praxis, e.g. an action to change the world and do good?
</div>

-----

![](/images/spinoza.jpg)

## Conatus

<div class="notes">

* A very specific form of monism, *parallelism* of mind and body: Every *affect* of the body is "reflected" in the mind and vice-versa.
* There is only one substance that takes different modes
* *conatus* is the name of the force that drives *persistence of being* and is part of every *mode* (every manifestation of the unique substance, a *thing* or a *sentience*). But not every mode shares the same *kind* of way to *persist*, or the same mode of existence! 
* [Ethica, III, 6](http://www.ethicadb.org/pars.php?parid=3&lanid=3#306): 

    > Everything, in so far as it is in itself, endeavours to persist in its own being.
</div>

## Passion & Action

<div class="notes">
* [Ethica, III,22](http://www.ethicadb.org/pars.php?parid=3&lanid=3#321)

    > He who conceives, that the object of his love is affected pleasurably or painfully, will himself be affected pleasurably or painfully; and the one or the other emotion will be greater or less in the lover according as it is greater or less in the thing loved.
* [Ethica, III,54](http://www.ethicadb.org/pars.php?parid=3&lanid=3#354)

    > The mind endeavours to conceive only such things as assert its power of activity.
</div>

-----

![](/images/kant.jpg)

## Intuition & Intellection

<div class="notes">
* Kant was concerned about providing inner solid grounds to *knowledge* : What can I know, what are the conditions for shared knowledge? This is the Critic of Pure Reason and his views on the mind. 
* We cannot know the *noumenal* world, the *thing-in-itself* we can only *know* through our perceptions and intellect, e.g. the *phenomenal* world
* The mind's capabilities are divided in 3 main parts: 
    * Intuition, the domain of sensible experience
    * Intellect, the domain of rational thoughts, 
    * Reason, the domain of ideas
* Through *transcendental* analysis, we can deduce what are the *a priori* conditions for the existence of knowledge and shared experience (categories)
   * for intuition (space / time)
   * intellect (causality, totality, uniqueness...)
   * Reason: Absolute
</div>

## Pure Reason

<div class="notes">
* Pure reason is the realm of the *absolute*, the part of the mind that produces judgments and seek to generalise statements from the intellect
* We cannot *know* anything outside of experience hence reason does not produce *knowledge*: We cannot *know* God nor *soul* nor *noumenal* world
* But because pure reason is driven towards the absolute, it is also the ground for morality and freedom
</div>

## Autonomy

<div class="notes">
* the *moral imperative* is grounded in pure reason because any moral grounded on experience would be particular and not universal
* the only *end* that is universally applicable, valid for any reasonable being (not even human) is the *being* itself, hence the various declinations of the *categorical imperative*: 

  > Act in such a way as to consider others always as an end and never as a mean

* because it is universally applicable, moral is what sets us *free*. And conversely, *freedom* of reason and our capability to conceive unconditioned ideas and thoughts is what permits the existence of moral
* Kant promotes *autonomy* versus the *heteronomy* produced by external moral rules (whether produced by "nature", society, religion, habits...)
</div>

# Affects

-----

![](/images/code-affects-1.png)

-----

![](/images/code-affects-2.png)

-----

![](/images/code-affects-3.png)


# Becoming

-----

![](/images/simondon.jpg)

-----

## Concretion & Abstraction

![](/images/tube-concretion.jpg)


<div class="notes">
* concrete/abstract in the simondonian interpretation implies some form of relationship with the world: Concretion is the process by which some technical object adheres to the world and is more "efficient" and "effective". It departs from its abstract or generic form to reach a state of better integration, or more *integrity*. Its parts fits perfectly within each other to form a coherent whole that is in some sense *organic*. Each organ is pluri-functional and not only serves a purpose but reinforces the whole (example of the combustion engine)
    * How does this relate to software? There is clearly a tension between *genericity* and *specialisation* in software, between *reuse* (which produces bloat) and *reinventing* (which produces obscurity)
    * Concretion and abstractions are not dependent on usage but on behaviour of the object, its "internal logic". In software, there is also this tension between needs arising from expected use and systemic constraints, leading to potentially suboptimal solutions
    * Concretion is the establishment of a feedback loop between the technical object and its environnement => *individuation*
</div>

## Individuation

![](/images/crystal.jpg)

<div class="notes">
* Boundaries of *what* is a "piece of software" are more and more blurred
  * long-lived server parts
  * support systems 
  * thirparty services
  * short-lived cloud functions
  * custom/local elements (Excel, macros, notebooks)
* Through individuation, the software loses its purely instrumental nature and starts *reflecting* on the humans it interacts with
    * Humans are *transformed* (transduced?) by the technical object, hence the software
* *Web* of interconnected elements, individuation becomes difficult to characterise, every piece of code seems to come "alive" in interaction with more and more other pieces

</div>

## Techno-logy

---- 

![](/images/deleuze-guattari.jpg)

-----

![](/images/latour.jpg)

-----

![](/images/boyle-pump.jpg)

-----

![](/images/aime.jpg)



# Crafting

-----

![](/images/alexander.jpg)

<div class="notes">
* No talk about the philosophy of software would be complete without a reference to Christopher Alexander!
* I have always been wary until I started reading *A Pattern Language*
</div>
----- 

![](/images/pattern-language-example.png)

<div class="notes">
* initially, _A Pattern Language_ expresses patterns in the language of the *problem*, e.g. 136 Couple's Realm or 44. Local Town Hall or even 224. Low Doorway are all architectural elements of a building, not instructions to builders, electricians or plumbers on how to build towns or buildings.

    > We do not believe that these large patterns, which give so much structure to a town or of a neighborhood, can be created by centralized authority, or by laws, or by master plans.
    > A Pattern Language, p.3
* patterns are supposed to be used in communicating with inhabitants, users, citizens and their use should be validated. They are really meant as a combinatory language with which one weaves a description of some building 
</div>

----- 

![](/images/design-patterns.jpg)

<div class="notes">
* Software patterns are described in the *language* of software, as code samples and they can hardly be composed to form a coherent whole
* Design Patterns are timed, located, grounded in some specialised dialect of the *solution* space. They were interesting and valuable but they suffered from *devaluation* over time as the paradigms they were grounded in (class based OO languages) are waning
* Contrast with the continued relevance of Alexander's Patterns, even when he speaks about urbanisation, towns, neighbourhoods, houses... 
* We don't wear Jacquard pull-overs nor nylon long sleeved shirts anymore but we still live in houses
</div>

-----

![](/images/ingold.png)

-----

## Tools

![](/images/biface.jpg)

<div class="notes">
* example of the [biface](http://www.archaeologywordsmith.com/lookup.php?terms=biface): ubiquitous across archaelogical sites and time. Arch. are projecting their own scheme of *design* on it: They assume it has been designed by some intelligence, then crafted into its current shape.
    * This is taken has evidence for prehistorical hominides' intelligence and consciousness
    * What if the biface we find was actually a residue of sort? Could be that people were using the small fragments as tools, rather than the biface itself
    * Shape of the biface is symetrical to the shape of two joined hands: could be that people were holding it with one hand while hitting it
* Ingold criticizes our teleological views and the fact we project our own assumptions on other being's motives and existence
</div>

-----

## Design 

![](/images/chartres-cathedral.jpg)

<div class="notes">
* medieval buildings were not *designed*
    * modern buildings are *completed* before handover to inhabitants, strict separation between construction and living 
    * medieval buildings were built using shapes, solid patterns made of wood. Stones are carved one by one and fit in place with other stones
* *objects* --> *things*
</div>

-----

## Transduction & Agency

<div class="notes">
* notion of transduction vs. agency
    * agency (Agentivité) is a corollary to embodiment (incarnation), it closes things on themselves and embeds the principles of action inside them
    * examples of kite, potery, reindeerman, cello: thing acts as a *transducer* between the gestures of a person and a material/nature
    * duration: preservation of shape (achievement, completion) vs. continuity of a process (ex. Moore's warrior)
    * correspondance, share experience through the exchange of letters, when I read a letter it is as if I was *with* the writer
    * distinction between *interaction* and *correspondance* : 
      * interaction is confrontation of two beings, stillness : none can move forward
      * correspondance is responding to the world through the mediation of transduction, attuning gestures and thoughts to the present world until they become undistinguishable through movement, through *making*

* One operates on/with software through the basic tool of *transduction*, e.g. the translation from one formal language to another one. 
    * High-level design languages: UML, formal specifications in B, type systems...
    * Programming languages at various levels from very "abstract" (Prolog, Haskell, Coq) to the very "concrete" (C, Assembly...)
    * Its transductions and code all the way down, down to the physical circuits and silicium cells
* Interpretation of software takes *time*, it is never instantaneous, it is a *process*: A code is the description of a process that needs interpretation hence time, it is never given in whole, like an image or metaphor, it requires traversal, reading/writing, understanding, analysis...

</div>

-----

![](/images/crawford.jpg)

<div class="notes">
* Shop class as soulcraft
* The World beyond your Head (which contains ill-founded critics of Kant's theory of knowledge and reason)
* Sometimes verges on the *reactionary*
</div>

---- 

![](/images/pipe-organ-builder.jpg)

---- 

![](/images/keyboards.jpg)

<div class="notes">

* Coding is an intense crafting activity whose medium is the same as the matter it manipulates.
* One craft software with her hands, [typing on a keyboard](https://www.meetup.com/Mechanical-Keyboard/photos/27166931/452768389/?_locale=fr-FR) is so important that there are groups dedicated to it and hundreds of different types of keyboards. 
* Each developer spends a lot of time customising her environment to be "more" efficient, dozens of books have been written on the subject of software craftmanship
</div>

# Ethics

## Anarchy

* Modern workplace is the place of heteronomy
* Autonomy is tied to morality and responsibility: Autonomy $\leftrigharrow$ Freedom $\leftrigharrow$ Solidarity
* responsibility extends to *all beings*, including the code itself

## Polymorphism

## Attunement

## Credits

* [Baruch de Spinoza](https://upload.wikimedia.org/wikipedia/commons/thumb/e/ea/Spinoza.jpg/1200px-Spinoza.jpg)
* [Aristotle](https://s-media-cache-ak0.pinimg.com/originals/7d/90/f4/7d90f4ab8d34e9e1a80dd09bdca64f1b.jpg)
* [Immanuel Kant](https://upload.wikimedia.org/wikipedia/commons/4/43/Immanuel_Kant_%28painted_portrait%29.jpg)
* [Gilbert Simondon](http://iphilo.fr/wp-content/uploads/2014/09/simondon.jpg)
* [Gilles Deleuze & Felix Guattari](https://didierbazy.files.wordpress.com/2013/07/20130722-124057.jpg)
* [Tim Ingold](https://raipgconference2013.files.wordpress.com/2013/02/1.png)
* [Bruno Latour](https://static1.squarespace.com/static/53a0503be4b0a429a2614e8b/53a0ec3ce4b091cafcd7fa26/57813322ebbd1acae39dad2c/1482445699448/Latour.jpg?format=1500w)
* [Christopher Alexander](https://upload.wikimedia.org/wikipedia/commons/9/95/ChristopherAlexander2012.jpg)
* [Design Patterns Book](https://qph.ec.quoracdn.net/main-qimg-e966f479b74b12155f6a6ebdff1999cd-c)
* [An Inquiry into Modes of Existence](https://s-media-cache-ak0.pinimg.com/736x/f9/b4/03/f9b403da85df11413086db664ec44c7d.jpg)
* [Matthew Crawford](http://md1.libe.com/photo/857914-next78_03extras_intell01.jpg?modified_at=1457437565)
* [Mechanical Keyboards Meetup Singapore](https://secure.meetupstatic.com/photos/event/7/9/8/6/600_453691110.jpeg)
* [Biface](https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/Biface_Silex_Venerque_MHNT_PRE_.2009.0.194.1_Fond.jpg/1200px-Biface_Silex_Venerque_MHNT_PRE_.2009.0.194.1_Fond.jpg)
* [Cathedral of Chartres](http://img.ceskatelevize.cz/program/porady/10719213271/foto09/214387750940021_chartres.jpg)
* [Pipe Organ Builders](http://magazine.nd.edu/assets/176376/fullsize/basilica_organ_11493.jpg)
* [Boyle's Air Pump](https://www.sciencenews.org/sites/default/files/main/blogposts/to_M0014706_free.jpg)
* [Crystal](http://www.carionmineraux.com/mineraux/mineraux_septembre_2010/cristal_de_roche_chine_3.jpg)
* [Electronic Tube Concretion](http://digitalmilieu.net/wp-content/uploads/2016/04/simondon02.jpg)
