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

# Product Development

* I wrote a "walking skeleton" in a month or so, starting from initial idea, while my partner was sorting out the business side of
  things and trying to find seed investors in the project
* This walking skeleton was, well, very skeletal, being able to handle only very basic scenario: register borrower/investor, create
  new simple, loan, commit funding to loan then accept loan
* We accelerated dev in Nov-Dec 2014 with a target release for February
* Initial design was pretty much top-down/waterfallish 

# Pair Programming

* Remote pairing works ok (if the network has no hiccup...)
    * setup a remote VM containing dev environment
    * connect with ssh + tmux
    * use hangout for voice + video (tip: set your terminal to be slightly transparent so that you can see the remote persons behind
      the code)
* It is more exhausting than face to face pairing, requires more focus and engagement from all parties to avoid miscommunications
* timezone differences makes *Deferred Pairing* useful: Someone starts some work, uses [slack](http://slack.com) as a stream of
  consciousness, then pushes result. Someone else in another timezone can then pick up work 
* Timezone differences are actually great for catching date/time related bugs!

# Development Automation

* **Continuous Integration** is core practice of XP and we embraced it fully on the form of continous deployment
* Continuous should be understood in the mathematical sense, e.g. opposite of **discrete**: You are able to deploy at any point in time, not at certain gateways or releases milestones
* This implies lot of infrastructure automation:
  [Infrastructure as Code](http://www.jedi.be/blog/2013/05/24/Infrastructure%20as%20Code/) and DevOps everywhere
* The slogan is: Automate all the things!
* CI is our most important tool for maintaining cohesion in the team and integrity of the software: Any commit pushed to CI triggers
  whole chain of build down to the point we obtain a *deployable application container*
* We can rebuild all our infrastructure from a couple of git repositories
    * **Note**: CI server is itself versioned and deployable
    * VM deployments is not fully automated yet but is scripted
    * configuraiton management is versionned and handled in Haskell too

# On Being Remote

* Great post about *Remote First* culture: http://zachholman.com/posts/remote-first/
* Counter-example: Organisations with a handful of offices, working distributedly. Distribution was an afterthought, something that
came up as a constraint because of mergers, and no amount of tooling can make that kind of culture work
* We are 6-7 hours apart, e.g. it is afternoon in Singapore when I wake up. This means we have about 3-4 hours of overlap, but we
  span 16-17 hours of wall clock time. 
    * removes the need for a dedicated support/ops team, at least as long as we are smallish (see above notes about automation)
    * We do our stand-up meeting at 9am or 10am Paris Time, acts as a sync point
* Use overlap time for discussing stuff (hangout works ok, would probably be better with dedicated software e.g. gotomeeting or
  equivalent) and pairing 
* We use Slack **a lot** (could use IRC too but would require some more infrastructure and it is not as friendly to non tech-savvy
  persons)
    * to chat within the team and discuss technical stuff, post screnshots, code examples, design ideas...
    * as a **stream of consciousness** endpoint: I dump what I am doing, jot down ideas, humours, feelings... When people wake up in
      the morning they have a trace of what I have done apart from the actual code/builds I produced and what I have been thinking
      of
* remote work can help reduce stress, esp. in startups: Being remote means you can simply *remove* pressure from business people
* it can also help minimizes personality clash, cool down interactions. Being face-to-face increases cues and hints about the other
  person's feelings but is also a great way to generate stress and emotions. 
* counterparty is *trust*: People at the other side of the world trust you to do what needs to be done

# Next Steps

* monitoring from infrastructure to business level metrics available to everybody in the company
