---
title: What's architecture and what does an architect do?
author: Arnaud Bailly
date: 2021-12-29
---


What's the difference between architecture and design? They are both about decisions, the former focusing on hard or costly to change decisions, the latter on lots of small-scale decisions within the code. It's not so much a difference of nature than a difference of scale. Then an architect is the person that takes or influences those hard or costly to change decisions. This is a definition quoted by Ralf Johnson in Martin Fowler's [Who needs an Architect?](https://martinfowler.com/ieeeSoftware/whoNeedsArchitect.pdf) paper.

Design is more "internal", tries to find useful abstractions for the team, whereas architecture would be more "external" (observable properties), expressing abstractions meaningful within a broader context. The architect is the one who finds the _right abstraction_, but what is this? Is this the same thing than XP's [system Metaphor](https://wiki.c2.com/?SystemMetaphor), which in itself is not a very clear concept? Perhaps she or he takes care of the emergent properties of the software.

Architecture is an activity that should be done within the team developing the software, it's about understanding how a piece of software works overall and this requires collaboration and communication. It can also be viewed as a support function for team, to help the team members create and maintain that understanding, to make their work easier.

An architect thus helps teams to become autonomous as much as possible and fostering cross-pollination between teams, she is not a substitute for people's doing their job, eg. actually developing the software. Like the saying goes, a good architect should teach other engineers fishing rather than give them fish. An architect _does_ code but _does not_ produce deliverable software and he or she must not be on the critical path to delivering the software.

The precise definition of an architect's role depends obviously on the organisation, and which main function he is mainly sponsored by: Engineering and development ; product, marketing and sales ; strategy, top-level management, finance or shareholders. In the first case, it will mostly be about choice of technologies and design solutions, in the second one it will be focused on communicating desirable or existing properties of the software, in the third case it will be concerned with strategic decisions about tech stacks, unification of practices, economies of scale perhaps.

Architecture is an activity that's always done, consciously or unconsciously whereas architect is a role that's only needed "at scale".

Perhaps less seriously, an architect is someone that draws a lot of boxes and arrows, and who likes that. Or it's just a title that's given in some organisations to appraise particular individuals or justify a higher paycheck.

Some links:

* An ironic presentation about Architects' role: https://www.infoq.com/presentations/We-Dont-Need-No-Stinkin-Architects/
* A classical paper about OO architecture: http://sunnyday.mit.edu/16.355/intro_softarch.pdf
* A recent article on the topic: https://martinfowler.com/articles/scaling-architecture-conversationally.html
