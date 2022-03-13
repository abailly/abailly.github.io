---
title: LambdaConf 2018
author: Arnaud Bailly
date: 2018-06-05
---

# Rock-Solid Haskell Services

* The workshop is presented by Roman Gonzales who is working for FP Complete and it leverages the company's experience building highly-reliable services in Haskell
* The overall architecture of the system is based on Matt Parson's [Three Layer Haskell Cake](http://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html), which is very reminiscent of Alistair Cockburn's Hexagonal Architecture and DDD
* It boils down to enforcing a strict separation between *resources*, *business services* and *core domain logic*
* To those 3 layers, Roman, drawing from his experience as a Clojure developer, adds a *Zero Layer* which is responsible for managing the lifecycle of _components_ in such a way that they can be disposed of and *restarted* at will. This gives the developer the ability to interact with the "complete" system directly in the REPL and be able to reload any part of it at will
* The [componentm](http://hackage.haskell.org/package/componentm) library provides the needed tools for this, and the ability to report failures at startup/teardown time, as well as some timings on how much time each component took to startup and teardown. Note that I am slightly annoyed that all logging done happens to be text-based which to me seems like an anti-pattern: Logging issued at the application level should be structured and only ever converted to human-readable format when it is needed
* Of course the greatest benefit of this approach is the shortening of the feedback loop it provides
* Roman spends quite a long time presenting another tool he wrote to manage configuration, [etc](https://hackage.haskell.org/package/etc).
  * `etc` allows one to define a lot of different sources of configuration like files, env variables, command-line flags, and merge them in a single JSON tree that can then be used by the application
  * I must say I am not a big fan of those big Yaml configurtion files although I must confess I sometimes indulge in them
  * Following a discussion with my colleague [Chris Vogt](https://twitter.com/cvogt), I more and more like the idea of limiting configuration to the bare minimum, e.g. to 0
  * Having non configurable executables that are produced and packaged for specific environment, which means moving the configuration up the stack, say at compile time
  * when we need to change some dependency, we recompile a process and redeploy it
  * this implies a solid deployment strategy and thec capability to rollback on errors
  * need to couple that with supervision trees in order to manage errors in an orderly manner
* the presenter then introduces [capataz](https://github.com/roman/Haskell-capataz) which is a Haskell implementation of *supervision* trees
  * supervision trees provide the ability to automatically restart components or components trees when an error happens
* the workshop proceeds to deployment and production concerns, with [Release It!](https://pragprog.com/book/mnee2/release-it-second-edition) as a reference pooint, and some definitions on various issues that can happen to a system:
  - *impulses* are rapid shocks to the system
  - *strain* is stress over time, which might be caused by dependency on another system that starts operating at half-capacity
     - signs are unresponsive app, spikes on system-level metrics, excess I/O
   - strain produce *Cracks* that can lead *cascading failures*
   - *faults* => *errors* => *failures*
* source of strains:
  - lack of timeouts on outbound requests
  - resource pools drained because of failures at lower level
  - system resource exhaustion (files, ports, RAM...)
  - not limiting size of resources (bounded queues)
  - limiting size of query results
* timeouts are very rarely transient which means retries should be delayed
* *circuit breakers* allow  keeping track of the state of a 3rd party system
  - when num of failures pass some threshold -> make all subsequent requests fail or return default value, marking the circuit open
  - after some time -> try again (canary requests) and close the  circuit again if it succeeds (e.g. 3rd party system goes back to normal state)
  - use timeouts on every use of allocated resource
* the last stage of the workshop goes to deploying all the services that have bben built. The presenter introduces (localstack)[https://localstack.cloud/] a tool to simulate part of AWS stack locally.

# From Zero to Live with Functional Javascript

This 2 hours workshop was given by Ian Thomas, CTO of SkyBet. It was a fast-paced overview on how to build modern javascript applications based on FP principles. The workshop is based on his [git repo](https://github.com/anatomic/zero-to-live-fp-js-workshop) and builds a complete 12 factor app from the ground up. It contains lot of materials and the session was way too short to get more than a glimpse of it, but it's definitely interesting material when one wants to build JS-based microservices.

Ian Thomas also gave a keynote on how FP helps break the _fear cycle_ which prevents large code bases from changing.

# Seeking Refuge from Unsafe JavaScript

Another workshop dedicated to Functional Javascript, presented by David Chambers. It goes through the various pure FP constructs provided by [sanctuary](), a library which basically provides Haskell on top of JS. Sanctuary provides algebraic data types and all the classical functions we have come to love from Haskell, and more.
