------------
title: On Test-Driven Development
author: Arnaud Bailly 
date: 2013-11-16
------------

* Within Agile circles, TDD is thought to be one of the techniques promoted by XP that is only for developers
* This is something I have been practicing for years now, and I still have to master 
* It occured to me recently, during a session on fractal TDD I gave at Agile Tour Beirut with Craig Morrisson, that there is
  actually more to it than simpy a technique for developers
* This is also tied to what Freeman and Pryce says in their GOOS book
* And I think this was what Kent Beck had in mind when he invented it, explicitly or implicitly, except that using the word Test
  was unfortunate as it lead other people to think it was somehow connected to the actual activity of testing
* TDD is the heart of Agile and from it you can derive all other practices that, along the years have become part of the norm of
  what can be considered agile software development
* When infected by this desire to be able to express in an *executable form* the **purpose** of what I shall be developing, other
  practices become obvious. Given TDD and the need for feedback:
    * I need *continuous integration* because it tells me faster whether or not my purpose is fulfilled, and whether or not all other
      people's purposes are fulfilled
    * I need short and frequent *iterative releases* to validate my tests (my assumptions) against the actual usage of end-users and customers
    * I need *refactoring* to ensure my tests stay manageable  and my code testable
    * I need *simple design* because it promotes testability and clarity
    * I need *pair programming* (or at least peer review) to double-check my tests and my code and to align them against the
      current practice of the team
    * I need *whole team* to ensure all parts of the software can be tested and are tested
    * ...
* Fractal TDD is just a compact and efficient way to say that TDD has most value when practiced at all level and all stages of
  your development process: Each and every artifact produced and designed must be backed with some "test" which expresses the
  purpose of this artifact and provides the ability to consistently, continuously, rigorously and reliably validate the produced
  artifact is actually the right one
* On another perspective, just as Curry-Howard expresses the isomorphism that exists between types and programs on one side, and
  predicates and proofs on the other side ; TDD expresses this same relationship logic has w.r.t. tests and software.
* A test is a *predicate* that should be asserted by the software
* And the fact that not all proofs (actually the vast majority of proofs) cannot be machine-checked does not invalidate the whole
  proving process behind mathematics ; just like the fact that not all tests can be automated does not impact their general
  usefulness 
* Relationship between code and test then goes on 3 levels:
    * Level 1 is actually *writing tests* for the sake of producing a good regression test suite. When done at this level, it is
      irrelevant whether you write your tests first or last
    * Level 2 is actually practicing TDD to drive coding, at the level of *unit tests* or a bit above that. Its benefits are:
      Simpler code, only relevant code is written, clean code...
    * Level 3 is *fractal TDD*, where this logic is applied at all stages and all steps of the development process
* There are probably more levels on the road to enlightment but those are *known unknowns*
* At an even higher-level, TDD is simply a rephrasing of classical scientific method: Make an hypothesis, Design an experiment,
  Validate or invalidate the hypothesis with the experiment, repeat until exhausted (till you die...) ; but in the context not of
  producing knowledge but to change the world
* This is the same mechanism that's at the heart of Lean: The *pull system*[^1] makes all parts of the system dependents on
  initial *pull* by customers/end-users
* *Feature Injection* is another high-level form of TDD: The whole development process is driven by the requested outputs of the
   system which trigger examples that are turned into tests then code.
* *Impact Mapping*, *story mapping*, *goal tables* are other instances of the same class. Starting from a high-level measurable
   business goal, derives intermediate goals, prioritize them, then use this to drive development building use cases
* *Lean Startup* is yet another form of scientific process, where one design experiments to find a business model.
* What are the consequences? For me, it is the acknowledgment that never should I start working on some code without a clear
  understanding of the goal in the form of an **assertable statement of purpose**, a *procedure* which can tell whether or not the
  goal has been reached, that is a **test**.
* Of course this test can takes many forms:
    * A piece of executable code,
    * A fitnesse table or cucumber scenario,
    * An Excel spreadsheet,
    * A more or less formal written statement about the goal, which can followed to deliver a definite assertion about the goal,
    * A description of the shape of some output,
    * ...
* It must have the standard expected characteristics:
    * Repeatable,
    * Self-contained: No external knowledge or artifact[^2] is required to understand and execute the test beyond what the test itself
      provides,
    * Actionable
    * Unambiguous
    * Timebound: One must be able to verify it within a specific (short) timespan
* Reframing this in a more mathematical setting, a test (both the statement and the procedure to verify it) must be:
    * Consistent: If it can run its output is either true or false
    * Complete: If it can run to completion, it gives an output for all inputs
    * Computable: It provides an output in finite (and low complexity) time and space
* A single test is usually not enough, it is just the start of a process so we usually end up with more tests. These tests can be
  viewed as an *executable specification* of the built system in so far as we accept the fact they are a mere *sampling* of the
  possible input/output space of this system, so we expect our set of tests to exhibit another property: Accurate Sampling.
* This sampling is expected to provide an accurate image of the specification, so accurate actually that it should be possible to
  read the tests *as if* they were the actual specification of the system 
* A genuine executable specification would be  a thing used in formal methods to prove properties of the system, or even derive
  the system from a formal definition (eg. as in B or Z) 
* TDD allows developers (and testers) to build such test suites while *designing the system* when there does not exist a
  specification of the system.
* TDD at level 2 is *one* programming technique, not *the* programming technique. When a specification already exists, eg. when no
  learning is expected to take place, it is not useful. 
* TDD at level 2 can be mixed with other techniques for different parts of the software. For example, one could start writing code
  following a given specification, provide a skeletal system providing minimal service and writing tests after code to expose this
  specification, then test-drive the design and development of some components to be included in the system. 
* If we say Level 2 TDD is one programming technique among others, what are these other techniques? In particular, what other
  techniques provide the same kind of guarantees than TDD provide? 
    * *Generative techniques* provide those guarantees: Start from a human-validated model, then automatically generate code from
       this model. This of course implies the generator software is itself validated and verified. The model need not be a complex
       UML diagram, it could a simple descriptive specification, in a DSL, from which more complex code is generated 
    * *Spike and Iterate* (Dan North, Liz Keogh) is the disciplined form of *hack and fix*. Produce an initial *spike*, eg. Minimal
       and experimental, implementation, designed to maximize feedback and learning ; then iterate on this initial spike,
       refactoring and completing tests while investigating detailed design of some crude parts. Here the guarantees are provided
       by end-users direct feedback.  
    * *Property-based testing* allows one to define properties about part of the system then use some automatic generators to produce 
       test cases sampling the input space and try to find counterexamples invalidating the property



[^1]: Saying that pull system is at the heart of Lean is somewhat controversial, I guess. Some would say that *kaizen*, the
continuous quest for perfection is actually at the heart of Lean. 

[^2]: Beyond some basic level of understanding of the context. If we are stating some property of a piece of software then some
assumptions can be omitted in the test, like the fact the software must be run...
