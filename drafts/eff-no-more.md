------------
title: Kill Your Darlings
author: Arnaud Bailly
date: 2019-09-11
------------

After a few months of internal discussions and heated debate within the team, I have finally deleted nearly _all_ Haskell code from my current project's code-base. The only remaining part is the use of [Shake](https://shakebuild.com) as our main build script but we are planning to decommission it in the short term. Being the kind of Haskell enthusiast I am this was a heartbreaking decision to make but a very rational one. Here I provide some details about the situation and the rationale behind this decision in the hope they are useful to other people or to my future self.

* Our codebase is mainly Python for the backend and React/Typescript for the frontend. Haskell was used either to build some services that would be deployed as part of the solution, or to build tools (code generation, analysis and build-related stuff) and as such was not essential to the core of the system
* We are trying very hard to ensure everyone in the team understands and can touch every part of the system. This means that all team members would have to pick enough Haskell to modify existing code
*


As they say, [Kill Your Darlings](http://wiki.c2.com/?KillYourDarlings)!



I have started working on my current project about a year ago. At that time, I was the only developer on the project and, although this project's code was not going to be written in Haskell, it was natural for me use Haskell code for some non-essential parts of the projects, mainly falling in two areas:

* Additional API and "microservices" that could be deployed indepedently as part of the solution,
* Tooling and helpers that would help us build, analyse, maintain the whole system

Because
