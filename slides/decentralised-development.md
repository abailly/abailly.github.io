---
title: Decentralising the Development Process
subtitle: An experience with Radicle and some ideas on how to increase teams' autonomy
author: Arnaud Bailly - @abailly.bsky.social
institute: Pankzsoft
date: 2025-10-02
revealjs-url: /reveal.js
---

# A short fiction...

----

![](/images/trump-macron-14-juillet.jpg)

----

![](/images/44bzh.jpg)

----

![](/images/entartage.jpeg)

---

![](/images/trump-angry.jpeg)

---

![](/images/trump-tech-dinner.jpeg)

---

![](/images/accounts-suspended.png)


::: notes

* On 14th of July of 2026, President Trump is invited at the Palais de l'Élysée for a reception following Bastille Day celebrations
* A commando of yellow jackets manages to sneak into the Palais and throws custard pie at him, spoiling his brushing
* Back in the US, he sends a commando of Navy Seals to abduct the terrorists but they are ambushed by a party of angry farmers and fail their mission
* As a retaliation against France he instructs U.S. tech companies to delete all accounts and data associated with French companies and ban them access to those platforms

:::

# Introduction

## Agenda

* Why decentralise software development?
* Experience report with Radicle
* Conclusion

## Where do I speak from?

* Senior Dev/Tech Lead/Architect/Consultant/🤡 for 30+ years
* Dedicated _eXtreme Programming_ Practitioner
* In search of _autonomy_ since 1969

## Too Long; Didn't Stay

* Centralised services trade _convenience_ for _control_ (and/or a fee). This is fine ...
* ... until it's not
* Decentralised solutions (re-)emerge like [Radicle](https://radicle.xyz)
* It affords teams with both convenience **and** control over coding process

# Why decentralise software development?

## A bit of terminology

* Decentralisation ≠ Distribution
* Distributed teams have become commonplace in software development
* Decentralised organisations are rather uncommon
* Decentralised _coordination_ is **hard**

## Why _centralise_ development tools?

## Convenience

* Offload complexity of tools setup and management on specialists
* Afford better UI & UX
* Ease of integration with other tools/services

## Efficiency

* Decrease TCO by mutualising infrastructure costs
* Offload cost of infrastructure management on someone else
* Specialisation ↦ Focus on _core_ domain

## Control

* Provide single point of control for organisations
* "Industrialise" practices and tools across organisation and whole sectors

## Why _decentralise_ development tools?

## Reclaim _ownership_

* Avoid unfettered value extraction from "megacorps"
* Avoid censorship from governments
* Avoid vendor lock-in

## Empower teams & individuals

Remember the [Agile manifesto](https://agilemanifesto.org/)?

![](/images/agile-manifesto.png)

## Empower teams & individuals

* There's no _One size fits all_ solutions
* Allow teams to find solutions that fit _their_ context
* Increase local efficiency and avoid bloat

## Increase Resilience

* Remove _Single Point of Failure_
* Distribute load across the "network"
* Adapt to changing environment

## Fight against Enshittification

![](/images/this-is-fine.jpg)

# Experience report

## A little bit of context...

* Working on [Cardano](https://cardano.org), a blockchain network and crypto-currency
* Decentralisation is (or should be) at the heart of blockchain
* Yet 100% of the code is hosted on GitHub!

## Experiments

* [HAL Team](https://app.radicle.xyz/nodes/seed.hydra.bzh/rad%3Az4QBQYzMP9DtUJmejVyDHkCyTVj8G) has been experimenting with [Radicle](https://radicle.xyz) since May 2025
* We have also been working on providing decentralised access to [Antithesis](https://cardano-foundation.github.io/antithesis)

## What is Radicle?

> Radicle is an open source, peer-to-peer code collaboration stack built on Git. Unlike centralized code hosting platforms, there is no single entity controlling the network. Repositories are replicated across peers in a decentralized manner, and users are in full control of their data and workflow.

More info at [https://radicle.xyz](https://radicle.xyz)

## In a Nutshell

![](https://radicle.xyz/assets/images/p2p-network.svg)

## Demo

![](/images/bonaldi-demo.jpg)

## Working with Radicle

Team's feedback is overall positive

* Great Developer Experience with CLI
* Smooth collaboration workflow
* Has all the core features one need to collaborate on "small" projects

----

Some warts and issues:

* Features mismatch between UI and CLI
* Not having a central authority caused some "fumbles"
* Requires deeper knowledge of git
* How to do Trunk-Based Development properly?

# Conclusion

## More Experiments

![](/images/buzz.jpg)

## Radicle CI

![](/images/radicle-ci-ui.png)

::: notes

* Work in progress to provide CI triggered by radicle events from a node
* Integrate with various run engines, including GHA, Concourse, CircleCI or custom (Ambient, Native, containers)
* Registers _job_ COBs that are available to all peers
* Great combo with reproducible builds -> allow decentralised build artifacts reuse

:::

## Resources Access Control & Audit

Started collaborating with [Antithesis](https://antithesis.com)

* Antithesis is a SaaS for _Deterministic Simulation Testing_
* We wanted to provide this service for the whole Cardano community
* Built a CLI tool to trigger and trace test runs over Cardano blockchain
* More info on [Repository](https://app.radicle.xyz/nodes/seed.hydra.bzh/rad%3Az2a7Te5b28CX5YyPQ7ihrdG2EEUsC)

## More ideas & projects

* Sharing locally built artifacts
* Decentralised Web-of-Trust for Open-Source Software packages
* Using [ATProto](https://atproto.com) for team(s) collaboration
* ...

## Takeaways

* Decentralised processes & organisations are more resilient
* This is the original promise of the _World Wide Web_ and DVCS like Git
* This promise has been captured by megacorps and governments
* There are tools like _Radicle_ that offer both _convenience_ and _control_

# Credits

* Le Soir: [Trump & Macron 14th of July](https://www.lesoir.be/sites/default/files/dpistyles_v2/ena_16_9_extra_big/2017/07/14/node_104609/4412350/public/2017/07/14/B9712609397Z.1_20170714123746_000+GAM9EKRAF.1-0.jpg?itok=Y9XgK-Ky1553016802)
* INA: [Delors entarté](https://mediaclip.ina.fr/media/videos/imagettes/886x498/332/CAB97106147.jpeg)
* [Trump angry](https://pbs.twimg.com/media/C3VSo4JWMAA7TA8?format=jpg&name=4096x4096)
* [Tech dinner](https://s.yimg.com/ny/api/res/1.2/L2FAIS8kafiVyJ_DPqv16w--/YXBwaWQ9aGlnaGxhbmRlcjt3PTk2MDtoPTY0MDtjZj13ZWJw/https://media.zenfs.com/en/cbs_news_897/5bd253ee5a00c50c6a66ea0770db3749)
* ["This is fine" meme analysis](https://medium.com/@CWSkelly/analysis-this-is-fine-meme-e8980ff61e78)
* [Bonaldi Demo effect](https://medium.com/future-haigo/comment-briller-en-démo-lorsque-lon-est-développeur-bef02e9f86db)
