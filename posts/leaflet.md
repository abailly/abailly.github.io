---
title: Publishing to leaflet.pub from Sensei
author: Arnaud Bailly
date: 2025-12-22
---

This week I have spent some time off exploring how I could leverage my existing initial support for [Bluesky](https://bsky.app) in [Sensei](https://abailly.github.io/sensei/) (documentation outdated) to publish [blog articles](https://abailly.github.io) with [leaflet.pub](https://leaflet.pub). I also took this opportunity to better understand how to use coding agents, mostly focusing on [Claude Code](https://github.com/anthropics/claude-code), and how much of a help it was in implementing features.

This was definitely an interesting endeavour, and I learnt quite a lot. This short blog post tries to summarize what I learnt, and what I would like to do with that knowledge, collecting and sharing a number of links in the journey.

## Agentic Development

It all started with me watching the [Claude Code in Action](https://anthropic.skilljar.com/claude-code-in-action) lessons online. So far, my usage of LLMs for coding had been limited to Copilot completion in IDE (Emacs) and a bit of agentic development using [aider](./using-aider.html). It's a conversation over lunch with my friend Jeff, whose competence I deeply respect that lead me to wish to try this approach on something significant.

The experience was very smooth and I must admit I have been impressed by how sophisticated those tools are now. I have used Claude for nearly 100% of the features I wanted to implement, the exception being the Markdown-to-Leaflet conversion logic which was a fun and interesting problem.

Here are my `/stats` so far.

```
   Overview   Models   (tab to cycle)

      Dec Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
      ··················································▒·
  Mon ····················································
      ·················································▒▒
  Wed ·················································░▓
      ··················································█
  Fri ·················································█░
      ·················································▓█

      Less ░ ▒ ▓ █ More

  Favorite model: Sonnet 4.5      Total tokens: 553.8k

  Sessions: 22                    Longest session: 3d 1h 17m
  Current streak: 0 days          Longest streak: 5 days
  Active days: 10/12              Peak hour: 8:00-9:00

  You've used ~4x more tokens than The Hobbit
```

Claude Code was able to autonomously:

* Implement [Timestamp Identifiers](https://atproto.com/specs/tid) from scratch using only the specification,
* Add a complete new set of `article` commands to publish, update, and delete articles,
* Map the [publications](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/publication.json) and [documents](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/document.json) lexica to Haskell data types,
* Add endpoints to the Servant API mapping a tiny subset of the [ATProto](https://docs.bsky.app/docs/api) for updating and deleting records,
* Add tests for the various features implemented, or sometimes tests I asked for specifically,
* Create a new `install` command in my [shake](https://shakebuild.com) build file,
* Update database schema to separate storage of articles' body from events and take care of adding migration scripts,
* Extract YAML frontmatter from Markdown file to populate document's body,
* ...

It wasn't entirely flawless, though:

* It introduced one bug because of the non-standard way in which `Event` are serialised to/from JSON in sensei. The bug was caught by property tests running in the CI and it took me a couple of hours to troubleshoot and fix, with the help of Claude which correctly fixed serialization once I pointed it,
* It messed up the API implementation, incorrectly using HTTP verb `DELETE` to implement article deletion instead of the more common `POST`, or implementing request data as query params instead of request body,
* Some of the tests it wrote were mostly useless, eg. get/set of data structures, or were quite complicated,
* It did not implement fully some features, sometimes stopping at the edge of the API, or leaving dummy placeholders.

Even though I found out the general advice to keep the leash tight on the agent, work in small increments, focus on one task at a time, were definitely right, I also experienced how exhilarating and addictive those tools could be. Being able to write a few sentences in a prompt and then witness Claude Code generate code, run `cabal build` and `cabal test`, fix errors, etc. feels initially like a super-power and liberating : while the agent is doing its magic, you can do things more interesting like reading, setting up [Pacific War's](https://bsky.app/profile/pankzsoft.net/post/3mae4qwemp22b) campaign, testing the output of previous steps, work on something else in parallel... And it quickly becomes tempting to delegate more to the agent, to work in increasingly larger and more complicated steps which is where bugs, scope creep, and bloat start to hit the codebase.

I am still a beginner with this stuff when I compare with what [some](https://github.com/NTCoding/claude-skillz/tree/main) [other](https://github.com/bastien-gallay/rhetorical-analysis-skill) [people](https://github.com/guillaumejay/AlienStressManager) around me are doing, but I must admit I am impressed and feel like it's a game-changer.

## ATProto

[Leaflet](https://leaflet.pub) is a publishing platform based on the [AT Protocol](https://atproto.com) aka. _Atmosphere_ or _ATProto_, the protocol propulsing Bluesky and a whole ecosystem of more or less decentralised tools. It offers an online tool to create "articles" grouped under a common _publication_, which are then rendered at `xxx.leaflet.pub` based on structured data, markup and theme choices. The data is stored in a [Personal Data Server](https://atproto.com/specs/account) which in theory can be [self-hosted](https://atproto.com/guides/self-hosting) or hosted by anyone, but in practice is most often [BlueSky](https://bsky.app).

So what _Leaflet_ does is to offer a specialised interface and set of _lexicons_ (schemas) to define _records_, then it leverages the ATProto to store and retrieve the publications and documents for some particular _identity_. [Taproot](https://atproto.at/) is an web-based tool that can be used to visualise the data for a particular _identity_ pointed at by a [_DID_](https://github.com/did-method-plc/did-method-plc/blob/main/website/spec/v0.1/did-plc.md). In my case, one can see:

* The [publication](https://atproto.at/viewer?uri=did:plc:s5wwr2ccjuqricdxiyiuspfv/pub.leaflet.publication/3m7zleg5tyc2b) to group all articles related to my blog,
* The collection of [documents](https://atproto.at/viewer?uri=did:plc:s5wwr2ccjuqricdxiyiuspfv/pub.leaflet.document), each describing a single entry in my blog.

I wrote a year ago some code to be able to publish _notes_ I take with sensei as BlueSky posts. This mostly took care of the authentication logic, leveraging some existing JWT infrastructure handling I already had in place and the biggest challenge was to refactor the sensei backend to accomodate for connection to BlueSky. I (or rather Claude Code) only had to extend the existing [Servant API](https://github.com/haskell-servant/servant/) to accomodate for more endpoints.

## Markdown

The trickiest part of the project was the conversion of my existing blog posts from Markdown to Leaflet document. For the past 10+ years I have been using [Hakyll](https://jaspervdj.be/hakyll/) which relies on [Pandoc](https://pandoc.org) which is a fantastic document conversion tool. While the latter does, obviously, not support Leaflet documents out-of-the-box, it turns out Pandoc's author John McFarlane has written an independent Markdown parser called [mmarkdown-hs](https://github.com/jgm/commonmark-hs/tree/master) that can easily be embedded: All one has to do is to implement a couple of typeclasses for [Inline](https://github.com/jgm/commonmark-hs/blob/master/commonmark/src/Commonmark/Types.hs#L65) and [Block-level](https://github.com/jgm/commonmark-hs/blob/master/commonmark/src/Commonmark/Types.hs#L97) the parser will call as it traverses the markdown structure.

The main difficulty stems from the fact the underlying models do not always match nicely. While roughly compatible, there are quite a few gaps and misalignments which made the implementation less straightforward - but more fun - than one would have hoped for:

* Letting aside images or specialised blocks like code fragments, Leaflet documents (at least `linearDocument`s) are basically a list of text `blocks` which contain some `plaintext` and a list of `facets`,
* Blocks can mostly not be nested, so for example a Leaflet `unorderedList` can only contain `text`, `image` or `header` blocks, whereas a `blockquote` cannot contain other blocks,
* Markdown's `Inline` elements can be nested down to some text fragment, where Leaflet's `facets` are attributes of a block that apply to that block's content,
* mmarkdown's ranges are 1-based and expressed as line/columns coordinate, where Leaflet's `facets` are 0-based and expressed as _bytes_ offsets in the block's plaintext they are applied to,
* Leaflet only supports block-level math where Commonmark exposes both inline and display math through its [extensions](https://github.com/jgm/commonmark-hs/blob/master/commonmark-extensions/src/Commonmark/Extensions/Math.hs) mechanism,
* ...

This is the part I ended up coding manually, and which I am not satisfied with. While it works and I can now publish all my blog posts, the way I am converting from one model to the other is convoluted and inelegant, and there are still quite a few glitches I need to fix.

## What's next?

This deeper foray into the _Atmosphere_ ecosystem lead me to wish to invest some more time into exploring ATProto, possibly hosting my own PDS or even trying to implement one in Haskell. According to [Claude](https://claude.ai/share/6e7463e4-3751-4f1d-8eab-e7560d4978dd) the surface of the API to implement to be part of the network is not large and amount to ~20 endpoints. The bulk of the complexity lies in the data structures that are exchanged, [DIDs](https://github.com/did-method-plc/did-method-plc/blob/main/website/spec/v0.1/did-plc.md#how-it-works), [CARs](https://ipld.io/specs/transport/car/carv1/), [DAG-CBOR](https://ipld.io/specs/codecs/dag-cbor/spec/), and what not. Fortunately, there's some [prior work](https://github.com/monadic-xyz/ipfs) in Haskell linked to IPFS which could possibly be reused. I might give a shot to implementing [Merkle Search Trees](https://inria.hal.science/hal-02303490/document) which seem to be a key component of how PDS exchange data and notably diffs.
