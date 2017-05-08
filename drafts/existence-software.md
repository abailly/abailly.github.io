------------
title: On the Mode of Existence of Software
author: Arnaud Bailly 
date: 2017-05-18
------------


* The concrete experience of developing software, the craft behind it... 
  * [Matthew Crawford](https://en.wikipedia.org/wiki/Matthew_Crawford)'s "Shop class as soulcraft":  Using one's hand, leaving purely intellectual work behind, meaning and struggling with matter
  * I have the same experience with coding, to me this is an intense crafting activity whose medium is the same as the matter it manipulates.
  * one craft software with her hands, [typing on a keyboard](https://www.meetup.com/Mechanical-Keyboard/photos/27166931/452768389/?_locale=fr-FR) is so important that there are groups dedicated to it and hundreds of different types of keyboards. Each developer spends a lot of time customising her environment to be "more" efficient, dozens of books have been written on the subject of software craftmanship
  * is software really a craft? or is it an art? Aristotle distinguishes between poiesis and praxis, and classifies all human arts and crafts in the former while the latter is reserved for doing the good
  * poiesis is production of something and is the domain of (slave) *labor* whereas *praxis* is action and the domain of freemen
  * Praxis as a way to emancipate human beings from labor, see [André Gorz](/posts/gorz.html)
  * Hannah Arendt values praxis as the way to change the society, above contemplation and theoretical work, she was the one who reactivated the distinction between poiesis and praxis
* considering writing software as praxis, e.g. an action to change the world and do good?
  
* Software is shape and matter, pattern and background, it defeats classical hylemorphic scheme, it is essentially a *performative language*
* Software is information
* software is the ultimate [mediator](http://www.yvescitton.net/wp-content/uploads/2014/09/LATOUR-MediasModesExistence-Juin2014-Txt.pdf), or the ultimate `[DC]` agent...

* 2 main modes of existence are *subject* and *object*
* Souriau adds the following modes of existence (from Latour's *Reflections on Etienne Souriau's Les différents modes d'existence*):
    * *thing* 
    * *phenomenon*
    * *soul*
    * *fiction*
    * *god*

* One operates on/with software through the basic tool of *transduction*, e.g. the translation from one formal language to another one. 
    * High-level design languages: UML, formal specifications in B, type systems...
    * Programming languages at various levels from very "abstract" (Prolog, Haskell, Coq) to the very "concrete" (C, Assembly...)
    * Its transductions and code all the way down, down to the physical circuits and silicium cells
* Interpretation of software takes *time*, it is never instantaneous, it is a *process*: A code is the description of a process that needs interpretation hence time, it is never given in whole, like an image or metaphor, it requires traversal, reading/writing, understanding, analysis...

* concrete/abstract in the simondonian interpretation implies some form of relationship with the world: Concretion is the process by which some technical object adheres to the world and is more "efficient" and "effective". It departs from its abstract or generic form to reach a state of better integration, or more *integrity*. Its parts fits perfectly within each other to form a coherent whole that is in some sense *organic*. Each organ is pluri-functional and not only serves a purpose but reinforces the whole (example of the combustion engine)
    * How does this relate to software? There is clearly a tension between *genericity* and *specialisation* in software, between *reuse* (which produces bloat) and *reinventing* (which produces obscurity)
    * Concretion and abstractions are not dependent on usage but on behaviour of the object, its "internal logic". In software, there is also this tension between needs arising from expected use and systemic constraints, leading to potentially suboptimal solutions
    * Concretion is the establishment of a feedback loop between the technical object and its environnement => *individuation*
* Through individuation, the software loses its purely instrumental nature and starts *reflecting* on the humans it interacts with
    * Humans are *transformed* (transduced?) by the technical object, hence the software
    
* But more *polymorphic* programs are in some sense more concrete ... see [John de Goes](http://degoes.net/articles/insufficiently-polymorphic) argument:
    * Take the function:
      
        ```haskell
        sameInteger :: Int -> Int
        sameInteger x = x
        ```
      
      It is an instance of the `id : a -> a` function but curiously, whereas the former's type entails a *lot* of possible implementations, the latter has only *one*!
    * *polymorphism* is not the same as abstraction (in Simondon's meaning), on the contrary, it is a *concretion* process because it makes the element (the function/type) fits several purposes in the same object (esp. when applying DRY)
* Individuation is a process, a *becoming* whose limit or phenomenon is the *individual* (ex. of the crystal). There is no predefined shape that is superimposed on shapeless raw matter to yield some end result, but rather the matter contains the germ of its individuation that can be triggered by some conditions (e.g. temperature, pressure, presence of impurities...): Individuality is an *emerging* property of some system that make "things" become

* One does not simply *force* shape over matter and this is even truer in software
    * Prominent view of software development process has been shaped by hylemorphism and essentialism: 
        * Software is a tool that is *used* to solve "real-world" problems in a more efficient way
        * The problem can be *specified* be *domain experts* then *implemented* by developers using some well-defined engineering process
        * This assumes there is an essence of the problem at hand that is independent of the fact we are using software
    * This is embodied in the separation of the *design* vs. the *coding* vs. the *testing* vs. the *deploying* view... 
    * Because there are different activities involved in building and running a software system does not imply those activities should be separated in time and space
    

* the Graal of declarative programming? What does it mean? 
    
* Intuitionism logic (Brouwer et al.), constructivism : we cannot conjure things out of thin air (out of excluded middle), we have to build mathematical (and technical?) beings, to prove their existence
* Software, programs, codes are proof systems

* Kant's Critic of Pure Reason and his views on the mind:
    * We cannot know the *noumenal* world, the *thing-in-itself* we can only *know* through our perceptions and intellect, e.g. the *phenomenal* world
    * categories for intuition (space / time) and intellect (causality, totality, uniqueness...)
    * Pure reason is the realm of the absolute
    
* Contrast Simondon's concretion/abstraction w. Alexander's Wholeness and Theory of Centers ? Looks like there are similarities in the underlying theory if not in the language
* Alexander as a tutelar semi-god of software development
    * initially, _A Pattern Language_ expresses patterns in the language of the *problem*, e.g. 136 Couple's Realm or 44. Local Town Hall or even 224. Low Doorway are all architectural elements of a building, not instructions to builders, electricians or plumbers on how to build towns or buildings.

        > We do not believe that these large patterns, which give so much structure to a town or of a neighborhood, can be created by centralized authority, or by laws, or by master plans.
        > A Pattern Language, p.3
    * patterns are supposed to be used in communicating with inhabitants, users, citizens and their use should be validated. They are really meant as a combinatory language with which one weaves a description of some building 
    * Software patterns are described in the *language* of software, as code samples and they can hardly be composed to form a coherent whole
* [A city is not a tree](http://en.bp.ntu.edu.tw/wp-content/uploads/2011/12/06-Alexander-A-city-is-not-a-tree.pdf): a city is actually a graph. The graph (or network) is the canonical modern shape
* Comparison between Alexander's work and Wolfram's New Kind of Science
* *design patterns* are not a language => there is no private language (Wittgenstein), these patterns are private to a group
    * show side by side images of the 2 books

* Tim Ingold's [Making](https://books.google.fr/books/about/Making.html?id=wwzUj2o42fUC&redir_esc=y&hl=en) 
* example of the [biface](http://www.archaeologywordsmith.com/lookup.php?terms=biface): ubiquitous across archaelogical sites and time. Arch. are projecting their own scheme of *design* on it: They assume it has been designed by some intelligence, then crafted into its current shape.
    * This is taken has evidence for prehistorical hominides' intelligence and consciousness
    * What if the biface we find was actually a residue of sort? Could be that people were using the small fragments as tools, rather than the biface itself
    * Shape of the biface is symetrical to the shape of two joined hands: could be that people were holding it with one hand while hitting it
* medieval buildings were not designed
    * modern buildings are *completed* before handover to inhabitants, strict separation between construction and living 
    * medieval buildings were built using shapes, solid patterns made of wood. Stones are carved one by one and fit in place with other stones
* *objects* --> *things*    

* Ingold (ch.7) : notion of transduction vs. agency
    * agency (Agentivité) is a corollary to embodiment (incarnation), it closes things on themselves and embeds the principles of action inside them
    * examples of kite, potery, reindeerman, cello: thing acts as a *transducer* between the gestures of a  personE and a material
    * duration: preservation of shape (achievement, completion) vs. continuity of a process (ex. Moore's warrior)
    * correspondance, share experience through the exchange of letters, when I read a letter it is as if I was *with* the writer
    * distinction between *interaction* and *correspondance* : 
      * interaction is confrontation of two beings, stillness : none can move forward
      * correspondance is responding to the world through the mediation of transduction, attuning gestures and thoughts to the present world until they become undistinguishable through movement, through *making*

* Notion of shape in Dewey (Art as Experience):
  > Shape is the property that characterizes experience as an experience

  > We can define shape as the operation of forces that confers to the experience of an event, a thing, a scene and a situation its wholeness

* Boundaries of *what* is a "piece of software" are more and more blurred
  * long-lived server parts
  * support systems 
  * thirparty services
  * short-lived cloud functions
  * custom/local elements (Excel, macros, notebooks)
* *Web* of interconnected elements, individuation becomes difficult to characterise, every piece of code seems to come "alive" in interaction with more and more other pieces
* systemic effect => autopoietic ?
    
## Aristotle

* distinguish between poiesis and praxis
    * poiesis: Action which is a mean to an end
    * praxis: Action which is its own end
* autopoietic systems: systems which generate their own ends through their existence, self-reproducing systems

## Simondon

* Technical objects are made of technical elements and part of a technical network
* When the internal workings of a technical object disappears, or is hidden, it becomes magical (Sufficiently advanced technology is undistinguishable from magic) and ripe for cultism

## Latour

* `[TEC]` : mode of existence of technical beings

## Spinoza

* Parallelism of mind and body: Every affect of the body is "reflected" in the mind and vice-versa.
* There is only one substance that takes different modes
* *conatus* is the name of the force that drives *persistence of being* and is part of every *mode* (every manifestation of the unique substance, a *thing* or a *sentience*). But not every mode shares the same *kind* of way to *persist*, or the same mode of existence! 
* [Ethica, III, 6](http://www.ethicadb.org/pars.php?parid=3&lanid=3#306): 

    > Everything, in so far as it is in itself, endeavours to persist in its own being.
* [Ethica, III,22](http://www.ethicadb.org/pars.php?parid=3&lanid=3#321)

    > He who conceives, that the object of his love is affected pleasurably or painfully, will himself be affected pleasurably or painfully; and the one or the other emotion will be greater or less in the lover according as it is greater or less in the thing loved.
* [Ethica, III,54](http://www.ethicadb.org/pars.php?parid=3&lanid=3#354)

    > The mind endeavours to conceive only such things as assert its power of activity.

## Pop References

* [Good Will Hunting](http://www.imdb.com/title/tt0119217/): On the importance of experience vs. knowledge. Will Hunting is a genius that can ingest and memoize the content of complex books in a matter of couple of hours. The character played by Robin Williams insists on the value of experience and time: You can know what love is, but it is a very different thing to experience love.
* [The Martian](http://www.imdb.com/title/tt3659388/): Mark Watney grows potatoes on Mars and manages to survive through his knowledge, science and ingenuity
