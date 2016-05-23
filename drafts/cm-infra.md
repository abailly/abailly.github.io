------------
title: Haskell-based Infrastructure Management
author: Arnaud Bailly 
date: 2015-10-26
------------

In a [previous post](/posts/cm-arch-design.html) I described the overall design and architecture of our core system. This post shall provide more details on our development and operations environment which uses mostly Haskell tools and code. I consider both development and production environments as a single integrated system as, obviously, there is a porous membrane between the two especially in a small company with 4 developers.

# Principles

When we started to setup this environment, we were guided by a few principles:

* Every system-level part should be containerized,
* There should be a single versioned source of authority for configuration,
* Use as much Haskell as possible.

## Everything Docker

[docker](http://docker.io) is still a controversial technology, esp. among system and cloud specialists, and the topic of hot debates which is a sure sign it is a game changer. And back in 2014 when we started developing Capital Match's platform, docker was in its infancy. I have had some experience in the past working with [VServer](http://linux-vserver.org/Welcome_to_Linux-VServer.org) and [LXC](https://linuxcontainers.org/) and containers are definitely great as a way to package (parts of) a system. Using docker allows us to:

* Provide seamless integration of development and production environments: The exact same software can be produced anywhere and used anywhere, whatever OS or configuration the actual developer is using, and the production configuration can be reproduced easily for testing or staging purposes,
* Encapsulate components in immutable "packages" that require minimal system-level configuration, e.g. no more fiddling with ports, machine names, environment variables... docker-compose takes care of running everything, and we can make services dependent to stable names,
* Simplify "hardware" configuration: All we need is something that is running docker (and a compatible kernel of course...) which means machines provisioning becomes a no-brainer,
* Isolate build and run components from system-level dependencies conflicts,
* Provide some level of reuse across containers and components thanks to layered FS.

Note that we stuck to the initial docker "philosophy" of *one process per container*, except for some very specific needs (e.g. Selenium testing): It is not possible to ssh into our applicative containers.

## Single Source of Authority

> Everything should be versioned and we should be able to recover the whole environment from a single git repository.

This means we should of course version our application's code, but also the system's configuration and as much dependencies as possible.
Ideally, we should be able to reconstruct the whole system from a handful commands:

* `git clone <the repo> cm`
* `cd cm; ./build ; ./deploy`

In practice this is quite a bit more complicated as there are some glue parts missing to ensure the whole system can be rebuilt from scratch, but still we came quite close to that ideal. We are using 2 different repositories, one for the application code and one for the environment, mostly for technical reasons related to how our configuration management software works. The only unversioned part is the description of the "hardware" and the provisioning part which is still done "manually".

## Everything Haskell

There is not a dearth of tools when it comes to configuration management, systems provisioning and deployment, build tools... When starting small you usually don't want to invest a lot of time in learning new tools hence a common choice is simply to start small with shell scripts. But tools usually exist for a reason: Scripts quickly become a tangled maze of scattered knowledge. Yet we have at our disposal a powerful tool: Haskell itself, the language and its ecosystem, hence we decided to try as much as possible to stick to using Haskell-based tools. Beside the obvious simplification this brings us (one compiler, one toolchain, one language...), the advantages Haskell provides over other languages (type safety, lazy evaluation, immutable data structures) seemed to be equally valuable at the applicative level than at the system level.

# Overview

![](/images/system-architecture.png){width=900}

The above figure gives a high-level overview of the system:

* Developers work on local machines (or *dev boxes*, see below), pushing changes to [git](http://git-scm.com),
* Pushing to remote repository triggers build on *continuous integration* server [bake](https://github.com/ndmitchell/bake),
* The final output of CI is a bunch of containers which are deployed to a private repositories on [docker hub](https://hub.docker.com/),
* When we are ready to deploy, we update the system configuration in another git repository then run [propellor](http://propellor.branchable.com/),
* This triggers configuration of *run* system which usually entails downloading correct container from repository and running it,
* Data (also stored in a container) is regularly backed-up on S3,
* Various components of the system feed events to [riemann](http://riemann.io) monitoring system.

# Build & Toolchain

## Build

### Cabal 

For building the Haskell part, we started obviously with [Cabal](https://www.haskell.org/cabal/) which is the defacto build system/package manager in Haskell. The structure of GHC+Cabal packages system makes it quite hard to create insulated and **reproducible** build environments as there are various interactions between what's installed globally (with GHC) and what's installed per user and per project. There was no [stack](http://docs.haskellstack.org) two years ago so we had to roll our own. Here are some key features of our cabal-based build:

* We used cabal snapshots with pinned down versions of all dependencies through `cabal freeze`
* Build became more complex when we started to add subpackages and sharing dependencies (>100) seemed like a good idea. We shared a single sandbox by setting `CABAL_SANDBOX_CONFIG` to point to toplevel directory sandbox configuration file for all packages, then `add-source` sub-packages. This make it easier to simultaneously:
    * Build everything in one go,
    * Work in a sub-package,
    * Work in main (top-level) package,
* This does not prevent rebuilds when moving across packages as the build directory used by cabal is still located within each package,
* We are still dependent on a globally available GHC. When upgrading GHC versions, you need to change globally installed GHC before rebuilding,
* Several concurrent versions can coexist and be used in the same directory as GHC maintains version/OS dependent packages database, but care need to be taken with `PATH`s as the cabal version is likely different too...

### Stack

[stack](https://github.com/commercialhaskell/stack/) represented a huge improvement for managing our build but it took us a few months to ensure it built consistently.

* Stack provides truly repeatable builds and segregate build environments tightly, including the tooling (compiler, lexer, parser...), managing downloads, setting package databases paths...
* There is a single executable to install which makes creating build containers much easier: install stack, run setup and you have a fully configured container at required version. It is also very easy to upgrade,
* Stack manages dependencies through [stackage](https://www.stackage.org/) meaning you only have to provide a single version number and you are guaranteed to have compatible versions of libraries. This might be sometimes problematic if you require some specific version of a library that is not part of the dependency package, but it is still possible to provide custom versions, 
* I was sometimes surprised by stack not reusing some previous build result, although I could make it work manually,
* The biggest hurdle we had to overcome to make stack work for us were the tests. Some tests relied on specific files to be present which means we had to manage relative paths: depending on whether or not tests are run from toplevel (which is the case in CI for example) or from local package directory (which is the case when using stack to build a tree of packages), relative directory may not be correctly set. Moreover stack runs tests in parallel, which is a good thing to force you to implement parallelizable tests but failed for us as we relied on starting server on some port for integration tests. We should get rid of hardwired port and allow the server to use some randomly allocated one but we chosed the simplest path and configured stack to run tests sequentially.
* A minor annoyance is (was?) that stack maintains a build directory in each sub package, even when run from the toplevel, which is not the case when using cabal sandbox. This implies that reusing previous builds is a bit more cumbersome as one needs to save each `.stack-work` directory.

### Leiningen

[leiningen](http://leiningen.org/) is (was?) the prominent build tool for clojure and clojurescript. We chose Clojurescript for the UI mostly because this allowed us to develop it using the excellent [Om](https://github.com/omcljs/om/) wrapper over React. It took us quite a lot of time to get our project build comfortable and it did not evolve as quickly as the Haskell one.

* When to distinguish various build targets: Development mode where we want some interactive reload within the browser, test mode to run unit tests (automatically or in batch) and production mode which needs to be optimiszed,
* Getting the tests to run correctly proved difficult and is still dependent on some manual configuration: To run clojurescript tests, we need to install [phantomjs](http://phantomjs.org/) and configure correctly [externs](https://github.com/cljsjs/packages/wiki/Creating-Externs) for the few javascript libraries we use and compile both code and tess **with only whitespace** optimizations (tests don't run in fully optimized mode),
* This actually means code is compiled as much as 3 times, which takes some time...
* The CSS part of the UI is written using [garden](https://github.com/noprompt/garden), which means we have to compile it to proper CSS then pack and compress all CSS files together to improve load time. In retrospect, this was probably a mistake: We don't use clojure's power to write our CSS and it is still a mess, so we would have been better off using some standard CSS language like Less or Sass (although this adds the burden of running some thirdparty tool as part of the build...).

### Javascript

When we introduced the mobile UI for Capital Match, we had to integrate its build inside our process. This caused some headache as this part of the system is developed in pure Javascript using [Emberjs](http://emberjs.com/) and relies on various tools in the JS ecosystem I was not familiar with. It also used [sass](http://sass-lang.com/) to write CSS which means we needed ruby to run the compiler. 

* We packaged all system-level dependencies into a single docker container. Note that official distribution's package for node and npm were outdated hence we had to install them "by hand" in the container which is apparently the right way anyway,
* There is a single top-level script which builds everything and is ran from the container.

### Shake

Given the diversity of tools and components we are building, we needed a way to orchestrate build of the full solution which could be easily run as part of *Continuous integration*. We settled on [shake](http://shakebuild.com) which is a Haskell-based tool similar to *make*.

* Rules are written in Haskell and shake provides support for all system-level tasks one would need to do in a build, including running arbitrary processes, manipulating their output, manipulating files... Using this embedded DSL makes it possible to track dependencies more easily. Shake maintains a database of dependencies that can be populated with arbitrary data,
* The default target of our build is a list of docker containers, one for each service of the system plus the nginx container,
* At the root of the dependency graph lies our build containers: one for *ghc + clojurescript* and one for building *javascript* code. Those containers are only expected to change (and be built) when we upgrade our toolchain. They are somewhat expensive to build as they need to provide all the needed tools to build (and run) our system,
* On the server side, we use the `ghc-clojure` container to run a top-level build script that builds all services and the web UI. Given it takes ages to download and build all dependencies, then build all the various parts of the system, we tried to maximize reuse across builds: The build artifacts are exported as data containers and we link to the $n-1$ build container in the $n$ build run,
* In order to minimize the size of our containers, we extract each service's executable from the full build container and repack it into another container which contains a minimal runtime environment. We initially tried to do something like [haskell-scratch](https://github.com/fpco/haskell-scratch/) but this did not work when our code needed to issue client-side HTTPS request: For some network and SSL reason the service fails to initialize properly. We resorted to use the standard busybox image, to which we add some prepackaged runtime libraries. This allows us to deploy containers with a "small" size: Our main application container weighs in at about 27MB and a typical service weighs 10MB. Note this has the additional benefit of drastically limiting the attack surface of the containers as they only contain the bare minimum to run our services and nothing else,
* Shake build also contains rules to run different class of tests: Unit/integration server-side tests, UI tests and end-to-end-tests (see below), and rules to clean build artifacts and docker containers.

## Development Environment

### Haskell

There has been various attempts at providing an IDE for Haskell: 
* [leksah](http://leksah.org/) is an Eclipse-based Haskell IDE, 
* [Haskell for Mac](http://haskellformac.com/),
* FPComplete used to provide some web-based environment.

Having used emacs for years, I feel comfortable with and besides there are actually benefits using a plain-text tool for coding when you are part of a distributed team: It allows you to easily setup a distributed pairing environment with minimal latency. Yet configuring a proper Haskell development environment in Emacs can be a challenging task, and it seems this is a moving target

* Started from http://tim.dysinger.net/posts/2014-02-18-haskell-with-emacs.html
* More recent stuff:
    * https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md
    * http://blog.hoersten.co/post/110096363794/modern-emacs-haskell-mode
* [ghc-mod](http://www.mew.org/~kazu/proj/ghc-mod/en/) is useful but somewhat hard to setup with cabal sandboxes and multiple packages... Better with stack

### Clojurescript

When building in development mode, we use of [figwheel](https://github.com/bhauman/lein-figwheel) which 

### Devbox


# Testing

## Continuous Integration

## Production

## Monitoring

# Conclusion


[^1]: Or is it my startup experience as a Haskeller?
