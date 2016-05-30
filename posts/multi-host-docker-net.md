------------
title: Multi-host Docker Networking
author: Arnaud Bailly 
date: 2016-05-30
------------

A while ago I grew the desire to experiment implementing *multi-host docker networking* to deploy [Capital Match system](/posts/cm-infra-2.html). This system is made of several interlinked containers and [docker-compose](https://docs.docker.com/v1.8/compose/) does not (did not?) work across several hosts. It seemed to me the [official solution](https://docs.docker.com/engine/userguide/networking/get-started-overlay/) based on `docker-machine`, `swarm` and service registry was a bit complicated: Our configuration is mostly static, e.g. number, distribution and relationship between containers in known at deploy time. Hence I looked for a simpler solution, something that would be more *networky*: I am indebted to [hashar](https://fr.wikipedia.org/wiki/Utilisateur:Hashar) for suggesting a GRE-based solution and to  the following references for actual technical details:

* [https://goldmann.pl/blog/2014/01/21/connecting-docker-containers-on-multiple-hosts/](https://goldmann.pl/blog/2014/01/21/connecting-docker-containers-on-multiple-hosts/)
* [https://wiredcraft.com/blog/multi-host-docker-network/](https://wiredcraft.com/blog/multi-host-docker-network/)

I did some experiment in shell, jotted down a couple of notes in my journal and moved on to other, more urgent duties. I had a couple of hours left on Friday last week and I stumbled on those notes which were sitting there, on my hard disk, and I decided it was a good time to write a blog post about this experiment.

I started writing this post embedding script fragments but I quickly wanted to check what I wrote actually worked, so I began running those scripts fragment. But then it made this experiment non repeatable which is definitely annoying if you make a mistake, want to restart from scratch, change some parameters... So I decided this stuff would warrant a minor project of its own where I could provide all the needed code to configure multi-host networking in docker based on GRE tunnels. I have done quite a share of system configuration and operations management and have been able to use or create some useful tools to streamline ops in Haskell, so it quickly became obvious I would need to write some Haskell code. So what started as a mundane journal cleanup ended up being a full-blown yak-shaving session whose result can be found in this github [repository](https://github.com/abailly/multi-host-docker).

