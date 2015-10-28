------------
title: The case for Event Sourcing
author: Arnaud Bailly 
date: 2015-10-26
------------

Relational databases have become ubiquitous in computing to the point where it is hard to imagine a system which do not store its
data in a RDBMS. RDBMS however suffer from several shortcomings: 
* They are complex and heavy systems themselves, hard to install, configure and maintain, which means that it quickly becomes
necessary to hire specialist to maintain those sub-systems,  
* They are hard to scale or more precisely they don't provide
  [infinite scaling](http://adrianmarriott.net/logosroot/papers/LifeBeyondTxns.pdf)[^5], 
* They impose a specific model, namely relational algebra, on the data model of the system, 
* They use a specific language (namely SQL, in various vendor-specific versions) with its own, syntax, semantics and primitive data
representations which introduces another level of impedance mismatch with the rest of the system, 
* They store the state of the system as a mutable structure: each change to this state destroys irrevocably the previous state.

Within Capital Match platform, we have chosen to embrace another model for persisting our data, namely the Event Sourcing model: the
system only stores as a log containing a linear sequence of events, each event representing a single atomic change within the
system. The state of the system is then the result of applying each event one at a time, starting from an initial empty state,
entirely kept in RAM.

This model has a number of interesting consequences, some technical and some which have impacts on the business:
* The state is immutable (and persistent), meaning that changing it does not destroy the previous state. It is then always possible to
know what the previous state, going back in time till one reaches the empty (initial) state,
* Because we keep the sequence of events leading to some state, it is possible to analyse that sequence and easily infer new facts
or gain new insights about the business (how long it takes for a user to do some action? How often are they doing something? Is
there a correlation between the action of some users?), 
* The state we keep in memory can be arbitrarily complex and can be queried using our standard development language. Furthermore, it
is very easy to duplicate/transfer/recreate state from a "database" which is a simple file containing the sequence of events. In
particular it is very easy to represent circular dependencies and generally arbitrary graphs, something which requires special care
in RDBMS and SQL, 
* The "database" is very simple, which means it is very robust and maintainable. Once again, this is a simple file, 
* Migrations, the act of changing the data model, is simpler: because each event is stored with a version, one simply has to know
how to interpret events from older versions, whereas migrating a RDBMS is a complex and error-prone task. 

Of course, there is no silver bullet and these advantages come at a price:
* The complete state has to fit in RAM. Given that one can easily use hosts with 64GB of RAM, and there exists system with 1TB of
RAM, this does not seem a real issue in the near future. And when it will start becoming an issue, we will already have tackled this
scalability issue by distributing the model across several nodes, 
* There is a performance price to pay at application's startup, because the state has to be reconstructed,
* One has to learn Haskell to start querying the state, instead of reusing SQL. But then Haskell is not a particularly difficult
  language to learn when one only needs querying and transforming data, and it is always possible if this is needed to interpret the
  event log in such a way as to reconstruct a relational data model. 
