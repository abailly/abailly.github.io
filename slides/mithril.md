---
title: Mithril Network
subtitle: Secured Snapshots for Faster Bootstrap
author: arnaud.bailly@iohk.io - @dr_c0d3
date: 2022-11-20
theme: virgil-black-iohk
revealjs-url: /reveal.js
---

# Introduction

![](/images/mithril-logo.png)

## Agenda

* Why Mithril?
* What is Mithril?
* Demo
* What's next?
* Conclusion

## Who?

* Developing software professionally since 1994
* Technical Architect for [Hydra](https://hydra.family)
* Lead Architect for [Mithril](https://mithril.network)
* Head of Architecture for Cardano at [IOG](https://iohk.io)

# Why Mithril?

## Security

![](/images/cardano-network.png)

::: notes

* Cardano's blockchain is extremely secure thanks to the Ouroboros consensus
* Ouroboros' _Proof-of-Stakes_ consensus relies on random block production & diffusion based on stake distribution
* Security is provided by the distribution of stake over a large number of SPOs (~3000)

:::

## Speed

Synchronising a node with the chain takes a while...

![](/images/sync_times_2.png)

::: notes

* It's a case for seldom used wallets -> resyncing after a few months can take ages
* For newcomers it's a major pain
* Starting a cardano-node from scratch on mainnet currently takes about 2 days

:::

## Security & Speed

* **First use case for Mithril**: Fast bootstrapping of cardano-node
* There are other use cases being explored: More secure Lightwallets, voting systems, ...

# What is Mithril?

## Research Paper

![](/images/mithril-paper.png)

## Mithril Multi-Signature Protocol {transition=none}

![](/images/mithril-proto-1.png)

## Mithril Multi-Signature Protocol {transition=none}

![](/images/mithril-proto-2.png)

## Mithril Multi-Signature Protocol  {transition=none}

![](/images/mithril-proto-3.png)

## Mithril Multi-Signature Protocol  {transition=none}

![](/images/mithril-proto-4.png)

## Mithril Multi-Signature Protocol  {transition=none}

![](/images/mithril-proto-5.png)

## Mithril Multi-Signature Protocol  {transition=none}

![](/images/mithril-proto-6.png)

## Mithril Multi-Signature Protocol  {transition=none}

![](/images/mithril-proto-7.png)

## Mithril Certificates Chain

![](/images/mithril-certificate-chain.jpeg)

## Mithril Network

![](/images/mithril-architecture.jpeg)

# Demo

![](/images/mithril-explorer.png)

# What's next?

## Decentralise Network

![](/images/mithril-decentralised.png)

## Extend Usage

* Provide Mithril signature for _parts_ of the chain state, eg. UTxO set, Stake Distribution, ...
* Mithril as the backbone of a voting system
* Mithril on other blockchains?

# Conclusion

## Come play with Us

* Mithril is open-source and developed in collaboration with the community
* Check-out [`https://mithril.network`](https://mithril.network) for more info
* Join us on [Discord](https://discord.gg/EGFTe2TT)
* Contribute on [GitHub](https://github.com/input-output-hk/mithril)
