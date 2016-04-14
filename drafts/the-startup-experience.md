------------
title: My startup experience
author: Arnaud Bailly 
date: 2016-04-12
------------

From August 2014 to April 2016 I have been working as CTO of a startup based in Singapore, developing a peer-to-peer lending
platform using Haskell programming language and environment. This experience has now come to an end and this short blog post is
a way for me to look back at those 20 months and reflect on the things I have learnt. I have some vague hopes this experience might
be useful to others. But the main goal is for me to make explicit things that have stayed mostly implicit in order to ensure I can
benefit from my own experience.

# Haskell Rocks #

Nothing new under the sun but working 120% of the time in Haskell over (nearly) the whole stack of our system
really reinforced that belief. The point is, Haskell is not only awesome because of its own merits as a language and a
platform. After all, Haskell is a language that is
[about 30 years old](https://en.wikipedia.org/wiki/Haskell_%28programming_language%29#History) and since its inception has mostly
been confined in academic circles. This is apparent in some weaknesses in the build system, standard data types like Strings and
numbers, lack of support for first-class modules...

Those deficiencies are compensated by a simple[^1] yet powerful type-system, a vibrant eco-system of good to great libraries
providing all the features one needs, a state-of-the-art compiler producing efficient programs, built-in support for powerful
concurrency features... I think Gabriel Gonzalez'
[Worst practices should be hard](http://www.haskellforall.com/2016/04/worst-practices-should-be-hard.html) offers some more solid
arguments in favour of Haskell. 

What sets Haskell apart is the fact that chosing to make it your core technology attracts interesting people. This is one of the
effect mentionned by Paul Graham of using non-[Blub](http://www.paulgraham.com/avg.html) languages: Not using a mainstream language
attracts non-mainstream people. [Bryan O'Sullivan](http://bos.github.io/strange-loop-2011/talk/talk.html)
of[Real World Haskell](http://book.realworldhaskell.org/) book fame also highlighted this point: In a world flooded by noise,
Haskell acts as a signal.

This does not mean that there aren't great people working in Java, Javascript or Php[^2]. And this 
does not mean I would like to work with any Haskeller just because she is a Haskeller. But from my experience hiring people, I found
Haskell acted like an effective filter[^5]: People who cared to answer job ads or reach out the company to enquire for job openings were
more often than not *interesting*. They were more diverse[^3] (different countries, different ages, different professional
experiences...), more curious, more motivated and for those whom I had a chance to pair with, quite good at programming.

# TDD Rocks

I have always been a strong proponent of [Test Driven Development](/posts/tdd.html). After all these years this practice is
[still](http://david.heinemeierhansson.com/2014/tdd-is-dead-long-live-testing.html)
[controversial](http://iansommerville.com/systems-software-and-technology/giving-up-on-test-first-development/) mostly because
people conflate two different things: "writing regression tests" and "using tests to guide your design", a confusion which is caused
by the use of *Test*  in * Test-Driven Development*. In a nutshell, one needs a very different mindset to jump from "I write tests
to verify my program does what I intend it to do, now and in the future" to "I write executable specifications in order to ensure 1/
I understand the problem and 2/ my program does exactly what's intended, no more, no less".[^7] The former mindset usually leads to
so-called white-box tests which are thorough but brittle and painful to maintain.

Even within the Haskell
community TDD is not widely accepted as a standard practice of development. I personally think Haskell is a really great fit for TDD
if you take it in a broad sense, that is if you include all the tools the language and platform provide as part of your TDD
cycle. This means leveraging the interpreter, compiler, type-checker and test running to shorten the feedback loop.

Maintaining tests require (a lot of) effort and discipline, and it is tempting when pressed to deliver to cut corners. And I
sometimes have done it myself: Comment out an flaky test in order to stabilize build. But except in one occasion, I have always come
back to it later and reinstate it. These efforts really pay off in the long run, when you start adding features upon features, when
the complexity of the system becomes too important to "fit in your head", when you need to modify existing features or extend them
to cope with new behaviours. TDD gives you both a safety net - so that it breaks when you change your code and highlights other
parts of the system that need to be adapted - and a guide on current behaviour of the code.

# Remote Development Works #

Although the company's business is exclusively located in Singapore I never lived there and we started working remotely, me in
France and my partner in Singapore. The development team has been distributed for almost all of the past 20 months, with people in
England, Portugal, India, Poland and Malaysia (not all at the same time). And we managed to develop a platform that handles a modest load
but has been working reliably managing financial and personal data since March 2015, steadily adding features, fixing bugs,
deploying new versions several times a week or even a day, building and maintaining build and production infrastructure...

We used some simple form of agile methodology with daily "standup" meetings that allowed us to talk to each other at least once a
day, strict automated tests, continuous integration and frequent releases. This made it possible to have rapid feedback from
business people even if I was not sitting in the same room most of the time. We exposed our work process through Trello, used
communication tools like Slack, Hangout, Skype, and tried a few others, and we managed to build a consensus across the whole
development team on what was going on and what we had to do. We even manage to
[pair program](https://pragprog.com/book/jkrp/remote-pairing) on a somewhat regular basis.

As already advocated in [Remote](https://37signals.com/remote) book, working remotely works under some conditions:

* *Distribute whole team*: Having most of the team colocated with one or two persons distributed does not work, 
* *Trust the people*: You have to trust each other and assume everybody is doing his or her best,
* *Communicate constantly*: You have to be very explicit about what you are doing, even if working alone, and you have to constantly
try to detect and solve potential conflicts, misinterpretations, misunderstandings that could quickly degenerate,
* *Use the right tools*: Emails are a useful tool but one which is often abused[^8], we need "hotter" media like chat, video/phone...

[Buffer](https://open.buffer.com/) is a good example of a company that has chosen to be fully distributed and is very transparent on how it works on a daily
basis. 

# ... But it needs energy

While keeping in touch with development team was always easy, doing same on the business side quickly became very hard. This is of
course related to the very localized nature of the business the company was doing, but also to different background and maturity
with respect to remote working. Remote working is definitely a viable option for software development as the success of a lot of world-spanning
open-source projects has demonstrated.

As I advocated above, working effectively as a remote team works needs some requirements to be met. But even if those requirements
are met, it still can fail if people are not trained and do not make the mental leap to make it work. It might be possible that
developers, having to deal constantly with abstractions, networks, virtualities, are more prone to make that leap. Once you consider
it normal to work on machines located in a data center 10000 kms away, it is a small feat to consider normal to work with another
developer located 10000 kms away. The network becomes an extension to standard Earth geography and there is a form of excitement in the
way modern technology allows us to break distance barriers[^4].

Unfortunately, this particular mindset is not widespread among people practicing other trades. "Business people" who don't need to interact constantly
with developers quickly lose grasp and stop putting energy in maintaining a communication link that's not obvious to them. The whole
zoo of tools we are using appears daunting when compared with the simplicity of Outlook and Excel. When one has a lot more
face-to-face interactions than online ones, she or he is quite prone to drop the latter in favour of the former. Note that
working as a distributed team is *not* to be equated with working *remotely*. Obviously, organizations have been distributed for a
long time: Businesses are used to employ people like salespersons who largely work remotely or to have various business units all
over the planet. But this is different from the kind of real-time cooperation and interactions one needs when developing software
and working as a distributed team.

Then your link to the actual business starts to stretch, to the point it might break.

# Takeaways

You can't summarize 20 months of your life in a couple of bullet points and I definitely think this experience was amazing and has
changed my life and the way I envision my work in a very deep way:

* Working as team remotely can be both satisfying and efficient when done properly,
* Working with people from diverse origins and nationalities in a foreign setting is exciting[^6], and travelling to work with those
  people occasionally is the best way to discover local culture,
* Haskell is *really* practical for large-ish systems development.


[^1]: Yes, simple when compared with something like Scala's or C++ type system. Haskell's type system only uses a few key concepts
that can be rather easily understood and explained and which allow you to build powerful abstractions on top of it without resorting
to syntax-directed tricks (e.g. macros).

[^2]: I personally know quite a few of them...

[^3]: With the notable exception of gender diversity: Over the 50+ person I interviewed I have had a single woman.

[^4]: As people dealing with technology we might also be keener to fall to its trap and seductive power.

[^5]: Of course, there is still the possibility this was just another way of selecting like minded people thus implicitly rejecting
genuine *diversity*

[^6]: Having to make yourself understood in a language (English) which is not the mother tongue of any of the people you work with
is sometimes frustrating, but always interesting. In can provide some natural dampening of feelings and emotions that cannot fail to
crop up in any collective endeavour.

[^7]: For a good rebutal of the previous arguments, see
[Uncle Bob](http://blog.cleancoder.com/uncle-bob/2016/03/19/GivingUpOnTDD.html)'s reply to the "Giving up on TDD" post. 

[^8]:  Emails are terrible  for discussions - exchanging and/or arguing over some more or less complex point - or task tracking - maintaining and updating
  status of some work in progress - yet they are unfortunately often used that way.
