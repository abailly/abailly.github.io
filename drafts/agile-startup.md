------------
title: Building an Agile distributed startup
author: Arnaud Bailly 
date: 2015-10-26
------------

# Planning

* We used [taiga.io](http://taiga.io) to plan the golive, with spring planning. It was a good thing to explain to non-tech people we
  can deliver so much in a period.
* Used 1 week sprints
* Took us a while to get to some form of regularity and cadence, never really get to smooth release and meaningful SP values
* People are having a hard time understanding SP, and when they understand they start trying to game it and discuss evaluations ->
  quickly get back to "when will it be done?" and "can't we really do it faster?" and "what if we did XX?" questions
* People also had a hard time using taiga (and I am not sure it would have been better with any other tool...): response is a bit
  slow, most of the things don't make sense for them, they don't follow really tickets and comments on them... nor do they care much
  about tracking SPs progress
* a month or so after go-live we switched to a kanban style process which is not really well supported by taiga
* we dropped formal estimates, even before golive: what's done is done, when it is not done it is not done. Coping with unrealistic
  expectations and requirements -> make everything visible, deploy very quickly and allow people to test easily, work in short steps
* We don't provide estimates anymore, and people don't ask for them, except in very broad terms
* Switched to using plain trello:
    * "everybody" knows it, very intuitive to pick
    * is super-fast and reactive
    * provides nearly all the features we need (except swimlanes but we use tags instead)
    * available on a wide variety of platforms
    * ...
* We use 2 boards:
    * one for day-to-day work, contains baby step features + bugs, from "Todo" to "Deployed in production"
    * one for roadmapping, contains 3-months columns with coarse-grained features

# Pair Programming

* Remote pairing works ok (if the network has no hiccup...)
    * setup a remote VM containing dev environment
    * connect with ssh + tmux
    * use hangout for voice + video (tip: set your terminal to be slightly transparent so that you can see the remote persons behind
      the code)
* It is more exhausting than face to face pairing, requires more focus and engagement from all parties to avoid miscommunications
* timezone differences makes *Deferred Pairing* useful: Someone starts some work, uses [slack](http://slack.com) as a stream of
  consciousness, then pushes result. Someone else in another timezone can then pick up work 
