------------
title: Weekly Review - Week 15
author: Arnaud Bailly 
date: 2017-04-17
------------

This post is a summary of links related to coding and software I followed and read (sometimes, skimmed over) in the past week. Its purpose is both to serve as a high-level personal log and as a potential source of interesting (or not so interesting) links. Entries are provided in no particular order with minimal comments…

# 2017-04-10 Monday

[qmuli/qmuli: Qmuli - Serverless framework for Haskell](https://github.com/qmuli/qmuli/)

: While preparing my talk about configuration management with Haskell, I went back to this fascinating idea: Execute Haskell code over AWS Lambda.

# 2017-04-11 Tuesday

[Problem 1 - Project Euler](https://projecteuler.net/problem%3D1)

: Thinking about building something like *Project Euler* but dedicated to philosophical questions... 

# 2017-04-12 Wednesday

[run emacs.app or emacsclient on Mac OS X](https://gist.github.com/masaedw/1571296)

: Still working on getting a decent emacsclient-based setup on Mac

[Getting the source directory of a Bash script from within - Stack Overflow](http://stackoverflow.com/questions/59895/getting-the-source-directory-of-a-bash-script-from-within)

: Surprisingly not straightforward esp. if one wants to take care of all corner cases

[AIME - {TEC·REF}](http://modesofexistence.org/crossings/#/fr/tec-ref)

: Back to [AIME project](/posts/eme.html) while working on my talk for newCrafts. Most of the links below are residuals from a trail of surfing the web starting from trying to better understand the TEC/REF crossing and following references from there

[What Is Object-Oriented Ontology? A Quick-and-Dirty Guide to the Philosophical Movement Sweeping the Art World | Art for Sale | Artspace](http://www.artspace.com/magazine/interviews_features/the_big_idea/a-guide-to-object-oriented-ontology-art-53690)

: It seems there exists something called *Object-Oriented Ontology* or *OOO* for short which is quite an earth-shaking news to learn for a former OO programmer slowly shifting towards FP. Maybe there is some reverse movement going on in the field of metaphysics and speculative philosophy where people coming from a purely logical framework (aka. classical logic, logical positivism, intuitionism...) would move towards an *object-oriented* approach. Seems mostly relevant and very active in the aesthetics. 

* [The uses and abuses of object-oriented ontology and speculative realism - Frontpage - e-flux conversations](http://conversations.e-flux.com/t/the-uses-and-abuses-of-object-oriented-ontology-and-speculative-realism/2105)
* [Dictionary of concepts for Graham Harman’s object-oriented philosophy {draft: work in progress} | Avoiding/the\Void](https://avoidingthevoid.wordpress.com/dictionary-of-concepts-for-graham-harmans-object-oriented-philosophy-draft-work-in-progress/)
* [Introduction to Object Oriented Ontology | The DEW Lab](http://www.thedewlab.com/blog/2012/07/12/introduction-to-object-oriented-ontology/)
* [brief SR/OOO tutorial | Object-Oriented Philosophy](https://doctorzamalek2.wordpress.com/2010/07/23/brief-srooo-tutorial/)
* [Processes and Powers – The Pinocchio Theory](http://www.shaviro.com/Blog/?p%3D995)

[Things.pdf](http://www.shaviro.com/Othertexts/Things.pdf)

: This one I read in more details, the author being somewhat supportive of OOO but apparently with some peculiar twists:

 - objects have an existence of their own which is incommensurable to us
 - we go into a state of terror when we realise this as exemplified by the mechanic from the introductory short story whose tools
   come alive and flesh-like
 - following Heidgger, Shaviro posits that objects, things, tools are withdrawn from mere presence, they "cannot be  reduced to its
   presence-at-hand - that is to say to the sum of its delineable properties" => contra correlation of things and human mind
 - but this dissolution, this alienness has a dual movement which manifests itself especially with *broken tools*: things become
   more-than-present, they spring to life *against* us, in their singularity
 - we don't use tools, we *ally* with them to reach our goals
 - things proposition me (Whitehead): They are not only a mere packet of sense-data
 - things have a relation between themselves which is *aesthetical*: feeling an object for its own sake
 - linked to romanticism and *wholeness* (??), the world as a "single gigantic system of references"
 - democracy of fellow creatures
 - anthropomorphism goes against anthropocentrism => imbues things with feelings, "thoughts" and desires
 - vitalism and panpsychism => Gaia ?
 
[Category Theory proofs in Idris - sitr.us](http://sitr.us/2014/05/05/category-theory-proofs-in-idris.html)

: Using Idris to institute a Categorical framework and make proofs with it

[dsn14-bftsmart.pdf](http://www.di.fc.ul.pt/~bessani/publications/dsn14-bftsmart.pdf)

: Byzantine fault-tolerant distributed consensus is the algorithmic problem at the heart of blockchains. This article claims to reach tens of thousands of TPS with its implementation of BFT consensus in Java. There quite a few companies like [Symbiont.io](https://symbiont.io/technology/introducing-symbiont-assembly/#the-api) building upon this technology to provide so-called "smart contracts" capabilities.

[Remote work: For programmers, the ultimate office perk is avoiding the office entirely — Quartz](https://qz.com/950973/remote-work-for-programmers-the-ultimate-office-perk-is-avoiding-the-office-entirely/)

: Another nail in the coffin of office work?

[The economics of software correctness | David R. MacIver](http://www.drmaciver.com/2015/10/the-economics-of-software-correctness/) and [The Rise of ``Worse is Better''](https://www.jwz.org/doc/worse-is-better.html)

: Those two articles go hand in hand...

# 2017-04-13 Thursday

[Le réalisme kitsch | Zilsel](http://zilsel.hypotheses.org/2103#more-2103)

: Une critique du *nouveau réalisme* dont le *réalisme spéculatif* qui a parti lié avec l'Ontologie Orientée-Objet dont je parle plus haut. Ainsi que de Latour, Meillassoux, Garcia et autres philosophes post-post-modernes.

[sdkmanager | Android Studio](https://developer.android.com/studio/command-line/sdkmanager.html)

: On a more practical matter, I paired with Bernard on setting up an environment for mobile apps development based on [Cordova](http://cordova.apache.org). Setting this up is less straightforward than one would like and requires things like installing the [Intel® Hardware Accelerated Execution Manager (Intel® HAXM)](https://software.intel.com/en-us/android/articles/intel-hardware-accelerated-execution-manager)... 

[OCaml Calendar FAQ](https://forge.ocamlcore.org/scm/viewvc.php/trunk/calendarFAQ-2.6.txt?view%3Dmarkup&root%3Dcalendar)

: Working on a follow-up to my first post [mixing DDD and Type DD](/posts/dependent-types-ddd.html), I started shaving some yak: Implement a (naive) dependently-typed date module in Idris. I got a gentle reminder in this [SO question](http://stackoverflow.com/questions/43352519/how-to-properly-handle-fin-n-and-integer-when-computing-dates) that handling dates correctly is extremely challenging, as this detailed post on dates handling from an OCaml library exposes.

[Spotify's Love/Hate Relationship with DNS](https://labs.spotify.com/2017/03/31/spotifys-lovehate-relationship-with-dns/)

: How Spotify leverages DNS in its microservices architecture

# 2017-04-14 Friday

[The Twilight of Becoming (and Process) | Naught Thought](https://naughtthought.wordpress.com/2011/08/17/the-twilight-of-becoming-and-process/)

: Another post related to my exploration of OOO. 

# 2017-04-15 Saturday

[The Programmers’ Stone » Implications for Software Engineers](http://the-programmers-stone.com/about/implications-for-software-engineers/)

: I don't know how I stumbled on this blog which seems to be an online version of a book. Some more comments on [C2 wiki](http://wiki.c2.com/?ProgrammersStone)... Looks like yet another instance of "developers are special" and "there are 10x programmers" myths, resting on some midly interesting research results from neurosciences related to [Kahneman's](https://en.wikipedia.org/wiki/Thinking,_Fast_and_Slow) work on behavioural economics.

[the wackids](http://www.wackids.com/)

: And now for something different: A rock band playing rock classics on toy instruments!

[Arnaud Bailly - Sur "Critique de la raison pure" (2)](http://abailly.github.io/posts/kant2.html)

: Published notes from last year's reading of Kant's CRP

# 2017-04-16 Sunday

[Hexing the technical interview](https://aphyr.com/posts/341-hexing-the-technical-interview) and [Typing the technical interview](https://aphyr.com/posts/342-typing-the-technical-interview)

: Two fantastic posts from Kyle Kingsbury of Jepsen fame, demonstrating the breadth and depth of his knowledge of computer systems, from writing bytecode in clojure to type-level programming in Haskell

[Régler sa vie more geometrico: Spinoza](https://halshs.archives-ouvertes.fr/halshs-00695545/document)

: Comment s'inspirer de l'Éthique ?
