------------
title: Weekly Review - Week 18
author: Arnaud Bailly 
date: 2017-05-08
------------

This post is a summary of links related to coding and software I followed and read (sometimes, skimmed over) in the past week. Its purpose is both to serve as a high-level personal log and as a potential source of interesting (or not so interesting) links. Entries are provided in no particular order with minimal comments…

# 2017-05-01 Monday<a id="sec-1" name="sec-1"></a>

[99 Bottles of Beer](https://github.com/abailly/xxi-century-typed/blob/master/idris/src/BottlesOfBeer.idr)

: I managed to complete my Idris implementation of the 99 Bottles of Beer classical Kata. This attempt was triggered by a discussion on Slack over [Sandi Metz's book](https://www.sandimetz.com/99bottles) which a couple of friends have read and is apparently excellent.

# 2017-05-02 Tuesday<a id="sec-2" name="sec-2"></a>

[Corfu: A distributed shared log | the morning paper](https://blog.acolyer.org/2017/05/02/corfu-a-distributed-shared-log/)<a id="sec-2-1" name="sec-2-1"></a>

: Very interesting paper about an implementation of a distributed strongly consistent persistent log, something handy when one wants to implement event sourcing. The code is open-sourced as [CorfuDB/CorfuDB](https://github.com/CorfuDB/CorfuDB) and there is a recent follow-up I did not delve into yet.

[Ask HN: What is the biggest untapped opportunity for startups? | Hacker News](https://news.ycombinator.com/item?id%3D13576236)

: I found it funny the first answer to this question is exactly what I am trying to do with [Dr.Code](http://drcode.io): Short consulting sessions to help teams get over thorny issues or have access to affordable expert knowledge and skills.

[Extreme Programming Rules](http://www.extremeprogramming.org/rules.html)<a id="sec-2-4" name="sec-2-4"></a>

: Back to basics... XP is still unsurpassed as a coherent set of guidelines and practices for software development.

# 2017-05-03 Wednesday<a id="sec-3" name="sec-3"></a>

[Multi-Stage Docker Builds for Creating Tiny Go Images](https://medium.com/travis-on-docker/multi-stage-docker-builds-for-creating-tiny-go-images-e0e1867efe5a)<a id="sec-3-1" name="sec-3-1"></a>

: Something I have been doing by hand for couple of years now and which is now a supported feature of docker: Use several containers in your build to trim down images size.

[Lambdas — Gordon 0.6.0 documentation](https://gordon.readthedocs.io/en/latest/lambdas.html)<a id="sec-3-2" name="sec-3-2"></a>

: I have been working last year on using Haskell to deploy  [AWS Lambda](https://github.com/abailly/aws-lambda-haskell) functions, a work that has been superseded by [qmuli](https://github.com/qmuli/qmuli/) which aims at packaging properly Haskell executables and manage the workflow. Gordon does just that for officially supported runtimes.

# 2017-05-04 Thursday<a id="sec-4" name="sec-4"></a>

[How to Make an Emacs Minor Mode « null program](http://nullprogram.com/blog/2013/02/06/)<a id="sec-4-1" name="sec-4-1"></a>

: I am thinking of writing an proper indentation mode for a proprietary language I am working with and looked up what support Emacs provide for doing so. Still looks a bit daunting unfortunately.

# 2017-05-05 Friday<a id="sec-5" name="sec-5"></a>

[(13) 2 10 Elaborator Reflection: Extending Idris in Idris - YouTube](https://www.youtube.com/watch?v%3DpqFgYCdiYz4)<a id="sec-5-1" name="sec-5-1"></a>

: Talk about using built-in elaborator engine in Idris to extend the language, similar to what one can achieve using Template Haskell.

[Is Fit Dead? A debate on Twitter | Eric Lefevre-Ardant on Java & Agile](http://ericlefevre.net/wordpress/2009/03/06/is-fit-dead-a-debate-on-twitter/)<a id="sec-5-2" name="sec-5-2"></a>

: It looks there is not much debate going on anymore on BDD/ATDD and I am not sure whether this is due to the fact it is now considered mainstream or has been dropped altogether. I have always been a big fan of FitNesse and *Executable Specifications* approach but this is something that requires a lot of energy to setup properly on a project. Not all teams and settings are ready to invest the time to build a common language across developers and business analysts.

# 2017-05-07 Sunday<a id="sec-6" name="sec-6"></a>

[NStack - Composable Data Analytics](https://nstack.com/index.html)<a id="sec-6-2" name="sec-6-2"></a>

: An interesting new proposal in the distributed computing space, implemented in Haskell. I am however not a big fan of having to use a custom language instead of an already existing scripting or programming language to describe workflows. Code is available on github: [nstack/nstack: Type-safe, composable microservices for data analytics](https://github.com/nstack/nstack)<a id="sec-6-1" name="sec-6-1"></a>

[Exercice n°1: renverser pour mieux voir - Simone et les philosophes](http://simoneetlesphilosophes.fr/imaginer-linversion/)<a id="sec-6-3" name="sec-6-3"></a>

: Exercer son esprit critique en commencant par le plus évident et le plus méconnu : les inégalités homme-femme. 
