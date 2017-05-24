------------
title: Notes on Reliability & Maintainability of Haskell Code
author: Arnaud Bailly 
date: 2017-05-24
------------

Those notes are my answer to a [question](https://gist.github.com/heath/858a321b5fc96d3011d9b6ea4fca3cb9) asked on the [Elm Slack](https://elm-lang.slack.com/)

1. what are some methods you use to maintain a clean codebase?
    1. basic principles always apply: maintain low-coupling & high-cohesion, separation of concerns
    
       in the case of Haskell/FP, coupling across modules happen when one module depends on another module, possibly on details of data types (e.g. constructors) of this module. Even depending on exported types/functions introduces coupling. Modules should be as much self contained as possible
    2. clear separation between functional components of the system, each being vertically integrated instead of spread out over
       multiple technical layers
       
       By "vertically" I mean that a unit of deployment is a "function" of the system, something that's meaningful from the point of view of the end-user/customer/business, e.g. a *bounded context* in the sense of Domain-Driven Design. This is opposed to a more technical view where units of deployment/organisation of the code are "horizontal" layers, e.g. Web service, applicative components, data access layer,...
        
    3. minimal code sharing, limited to common types. Use common/standard libraries wherever possible, pack common dependencies as libraries
    
       Rather than depending on code at source level, depend on it at binary level, e.g. as a package.
       
    4. standard module structure, repeated over all components of the system to ease navigation and understanding
    5. test, test, test at all levels: unit tests (including QC tests), integration tests (e.g. at boundaries like REST APIs),
       system level tests
    6. limit size of modules, components, functions (I always use a small 13'' screen and everything should fit in it)
    
       Size matters when you need to refactor/edit/change any piece of code. GHC is smart enough to optimise code so there is usually nothing preventing dividing code in manageable chunks
       
    7. no dependencies between components so that I can work on a single one at a time
    
       *component* means *deployable unit of code*, e.g. might be a package or a service
       
    8. type explicitly everything
    9. abstract away actual IO as much as possible. Explicit IO type should appear as late as possible (e.g. in main function),
       favor specific monads with limited effects
    10. use *actual types* wherever possible to represent domain concepts, avoid primitive types and above all *naked strings* (or `Text`)
        when they represent actual values
        
        Types are the interface of functions/modules and they should be as explicit as possible. Compare:
        
        ~~~~~haskell
        checkPassword :: UserName -> Password -> Bool
        checkPassword u p = ...
        
        checkPassword' :: Text -> Text -> Bool` 
        checkPassword' userName password = ...
        ~~~~~

        Haskell provides *type synonyms* so it is tempting to have:
        
        ~~~~~haskell
        type UserName = Text
        type Password = Text
        ~~~~~
        
        But aliases are syntactic and are replaced when typechecking code. Using aliases actually means depending on a concrete representation of the type. `newtype` are cheap in Haskell and it is easy to derive typeclasses to use constant strings or numbers when needed, but they are "actual types" and thus increases readibility and reliability of code.
        
    11. limit exposition of constructors esp. for ADTs, prefer exposing functions and "smart constructors"
 2. how does haskell help you to maintain a clean, decoupled codebase?
    1. being expressive leads to more compact code which means more information fits in a single function which helps focusing
    2. thanks to types, refactoring is very easy: change what's needed to be changed and fix compiler errors
    3. thanks to types, it is easy to generalise cross-cutting concepts that appear in different contexts
    4. compositionality of functions helps defining and using small manageable chunks of logic
    5. "higher" type concepts (typeclasses, GADTs, functors and friends, type families) increase expressivity and allows introducing
       domain-relevant or technical abstractions at all levels
    6. types are very useful to model domain-related concepts and have some constraints checked by the compiler
    7. being both compiled and interpreted decreases the feedback loop when developing/tuning code: I can use the REPL to play with
       the code
 3. what system design benefits do you receive from a statically typed, pure language such as haskell, that you wouldn’t receive in a dynamically typed, but immutable language?
    1. I can have a strong and statically enforced separation between a core domain model's functions and types and connections to
       the outside world, along the line of hexagonal architecture. Thanks to separation of effectful and pure functions, it is easy
       to test/QC/reason about the core domain without having to deal with the impurity of realworld computations. For example, I
       can let concurrency out the picture and later on use STMs (or something else) to manage concurrent access, or I can model my
       system with concurrency in mind (e.g. CRDTs, event streams) and still not embed effects until actual deployment
    2. I can leverage the power of the compiler to chose the level of correctness I want/need, from lousy to proof-like... I can
       actually code in Haskell like I would code in a dynamically typed language (e.g. use simple generic immutable structures like
       list, use primitives...) but I can also go for something close to proof-system.
