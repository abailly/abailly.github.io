---
title: Décentraliser le Processus de Développement
subtitle: Pourquoi ? Comment ? Retours d'expérience
author: Arnaud Bailly - @panzksoft.net
institute: Pankzsoft
date: 2026-01-16
revealjs-url: /reveal.js
---

# Merci

![](/images/snowcamp-sponsors.png)

# Une fable contemporaine

----

![](/images/trump-macron-14-juillet.jpg)


::: notes

* Le 14 juillet 2026, le Président Trump est invité au Palais de l'Élysée pour une réception suivant les célébrations du 14 juillet

:::

----

![](/images/autonomistes-savoyards.jpg)


::: notes

* Le 14 juillet 2026, le Président Trump est invité au Palais de l'Élysée pour une réception suivant les célébrations du 14 juillet
* Un commando d'autonomistes savoyards  parvient à s'infiltrer dans le Palais

:::

----

![](/images/entartage.jpeg)


::: notes

* et lui lance une tarte à la crème, gâchant son brushing

:::

---

![](/images/trump-angry.jpeg)


::: notes

* il est très en colère

:::

---

![](/images/trump-tech-dinner.jpeg)


::: notes

* De retour aux États-Unis, il envoie un commando de Navy Seals pour enlever les terroristes mais ils tombent dans une embuscade tendue par un groupe d'agriculteurs en colère et échouent leur mission
* En représailles contre la France, il ordonne aux entreprises technologiques américaines

:::

---

![](/images/accounts-suspended.png)


::: notes

* de supprimer tous les comptes et données associés aux entreprises françaises et de leur interdire l'accès à ces plateformes

:::

# Introduction

## Plan

* Pourquoi décentraliser le développement logiciel ?
* Retour d'expérience - Radicle
* Retour d'expérience - Amaru
* Conclusion

## D'où parlé-je ?

* Dev Senior/Tech Lead/Architecte/Consultant/🤡 depuis plus de 30 ans
* Praticien convaincu de l'_eXtreme Programming_
* Dans "la blockchain" pendant 8 ans
* À la recherche de l'_autonomie_ perdue

## Trop Long ; Pas Resté

* Les services centralisés échangent de la _commodité_ contre du _contrôle_. C'est pratique...
  * ... jusqu'à ce que ça ne le soit plus !
* Des solutions décentralisées (ré-)émergent pour gérer le code source
  * Par exemple [Radicle](https://radicle.xyz)
* Décentraliser, c'est difficile...
  * ... mais nécessaire à la conservation de notre libre-arbitre

# Pourquoi décentraliser le développement logiciel ?

## Un peu de terminologie

## Libertaire ≠ Libertarien

![](/images/rand-kropotkine.png)

::: notes

* blockchain et réseaux p2p souvent associés aux méchants libertariens
* il y a pas mal de libertaires dedans aussi
* libertaires = anarchistes "collectivistes", intéressés par l'autonomie de groupes d'affinités oeuvrant pour le bien commun dans le respect de chacun·e et de la planète
* libertariens = anarchistes "invidualistes" et "capitalistes"

:::

## Répartir ≠ Décentraliser

![](/images/distributed-vs-decentralised.png)

::: notes

* Les équipes réparties sont devenues courantes dans le développement logiciel
* Les organisations décentralisées sont plutôt rares
* La _coordination_ décentralisée est **difficile**
* distribution est un anglicisme, le vrai mot c'est répartition
* coordination décentralisée est un problème insoluble en informatique
* c'est un continuum: la fédération est un compromis possible

:::

## Pourquoi _centraliser_ le développement ?

* Par commodité
* Pour plus d'efficacité
* Pour plus de contrôle
* Pour plus de sécurité

::: notes


* Déléguer la complexité de la configuration et de la gestion des outils à des spécialistes
* Offrir de meilleures interfaces et "expériences" utilisateur
* Faciliter l'intégration avec d'autres outils/services
* Déléguer la sécurité à des tiers

* Diminuer le _TCO_ en mutualisant les coûts d'infrastructure
* Déléguer le coût de la gestion de l'infrastructure à quelqu'un d'autre
* Se spécialiser ↦ Se concentrer sur le domaine _principal_ de l'organisation


* Fournir un point de contrôle unique pour les organisations
* "Industrialiser" les pratiques et outils à travers l'organisation et/ou des secteurs entier
* "Simplifier" la sécurisation des données et processus

* sécurité = centralisation authentification et authorisation est plus simple
* PKI difficile

:::

## Pourquoi _décentraliser_ le développement ?

## Reprendre la _contrôle_

* Éviter l'extraction de valeur effrénée des "mégacorporations"
* Éviter la censure des gouvernements
* Éviter le verrouillage par les fournisseurs

## Reprendre le _contrôle_

![[Quentin Adam - Travailler pour une (Big) Tech US : que signifie « collaborer » ?](https://www.linkedin.com/pulse/travailler-pour-une-big-tech-us-que-signifie-collaborer-quentin-adam-kjgce/?trackingId=NyTmPbR8AAzhIklktKZZ3A%3D%3D)](/images/collaborer-quentin-adam.png)

## Donner du pouvoir aux équipes et aux individus

Vous souvenez-vous du [Manifeste Agile](https://agilemanifesto.org/) ?

![](/images/agile-manifesto.png)

## Donner du pouvoir aux équipes et aux individus

* Il n'y a pas de solution _universelle_
* Permettre aux équipes de trouver des solutions qui correspondent à _leur_ contexte
* Augmenter l'efficacité locale et éviter le "bloat"
* Responsabiliser les équipes et les individus

## Augmenter la Résilience

* Supprimer le _Point Unique de Défaillance_
* Distribuer la charge à travers le "réseau"
* S'adapter à un environnement changeant

# Retour d'expérience - Radicle

## Expérimentations

L'[équipe HAL](https://app.radicle.xyz/nodes/seed.hydra.bzh/rad%3Az4QBQYzMP9DtUJmejVyDHkCyTVj8G) a expérimenté [Radicle](https://radicle.xyz) entre mai 2025 et novembre 2025

## Qu'est-ce que Radicle ?

> Radicle est une pile de collaboration de code open source et peer-to-peer construite sur Git. Contrairement aux plateformes d'hébergement de code centralisées, il n'y a pas d'entité unique contrôlant le réseau. Les dépôts sont répliqués entre pairs de manière décentralisée, et les utilisateurs ont le contrôle total de leurs données et de leur flux de travail.

Plus d'infos sur [https://radicle.xyz](https://radicle.xyz)

## Démo

![](/images/bonaldi-demo.jpg)

::: notes

* create a new radicle node with a new id
  * curl -sSLf https://radicle.xyz/install | sh
  * configure to add z6MkfiRENtzUJiU1kxLhxWMWFCiGGxGi6jEbj33Pq9zBVQkK@seed.hydra.bzh:8776 as preferred seed
* Create a public repository
  * commit and push some code
* rad clone on MBP outside the VM
  * create and push a patch
  * show patch appearing in VM
  * validate patch
* Add did:key:z6MkhgPg6WShnhJcmfwox4G5yL3EvJ2zW8L31SZLD95yUi11 as delegate
* on MBP, modify main then push
* check changes appear on the other side
* change visibility to public (?)

:::

## [Radicle UI](https://app.radicle.xyz/nodes/seed.hydra.bzh/)

![](/images/radicle-web-ui.png)

## [Radicle CI]()

![](/images/radicle-ci-ui.png)

::: notes

* Travail en cours pour fournir une CI déclenchée par les événements radicle depuis un nœud
* Intégration avec divers moteurs d'exécution, notamment GHA, Concourse, CircleCI ou personnalisés (Ambient, Native, conteneurs)
* Enregistre des COBs de _travaux_ qui sont disponibles pour tous les pairs
* Excellente combinaison avec les builds reproductibles -> permet la réutilisation décentralisée des artefacts de build

Quick demo with sensei:

* push a change in the sensei repo
* show the .radicle/native.yaml configuration
* show the radicle-ci configuration -> control who can run what
* show the result of the build (should be fast)

:::

## Comment ça marche?

![](/images/radicle-arch-simple.png)

::: notes

* réseau de noeuds pair à pair
* chaque noeud est associé à une paire de clés, la clé publique définissant l'identité du noeud
* les noeuds s'échangent des événements signés relatifs à des référentiel
* à chaque référentiel de code est associé un ensemble de méta-données elles-même versionnées

:::

## Travailler avec Radicle

Les retours de l'équipe sont globalement positifs

* Excellente expérience développeur avec la CLI
* Flux de travail collaboratif fluide
* Possède toutes les fonctionnalités essentielles nécessaires pour collaborer sur des "petits" projets
* Équipe très compétente et réactive

----

Quelques défauts:

* Décalage de fonctionnalités entre l'interface utilisateur et la CLI
* Ne pas avoir d'autorité centrale a causé quelques "tâtonnements"
* Nécessite une connaissance plus approfondie de git
* 🧩 Comment faire du Trunk-Based Development correctement ?

## Alternative - [tangled.org](https://tangled.org)

![](/images/tangled-org.png)

# Retour d'expérience - Amaru

## Un peu de contexte...

* [Cardano](https://cardano.org)  est une _blockchain_ (et une crypto-monnaie) ie. un journal de transaction réparti pair-à-pair
* [Amaru](https://amaru.global) est un projet open-source de création d'un noeud pour le réseau Cardano
* La décentralisation est (ou devrait être) au cœur de la blockchain
* **Problème**: Comment gérer de manière décentralisée et transparente un projet comme Amaru?

## Les acteurs du projet

![](/images/amaru-treasury-high-level.png)

::: notes

* PRAGMA: Member based organisation, legal umbrella for various projects but does not participate in the operations/governance of the projects themselves
* Amaru Maintainers Committee: Small group of people responsible for a specific _scope_ within the overall project
* Contributors: people or organisations contributing to the project possibly for profit
* Cardano: blockchain
* Stakeholders: People holding Ada and contributing to the governance of Cardano through their vote

:::

## Le processus de paiement

![](/images/amaru-disbursement.png)

::: notes

* contributor signs a service agreement with AMC
* contributor contributes some code materialised through commits -> uniquely identifiable
* contributor issues invoice to AMC
* AMC creates disbursement transasction

:::

## Le chaînon manquant

* Cardano et IPFS sont des infrastructures décentralisées et résilientes...
  * ... mais pas une "forge" centralisée!
* **Problème**: Comment "garantir" la pérennité du lien entre les contributions et les transactions?
* **Solution**: Un _oracle_ enregistrant sur la chaîne une structure de type "Arbre de Merkle"

----

![[Merkle-Patricia Forestry Service](https://mpfs.plutimus.com)](/images/mpfs.png)

# Conclusion

## Au delà du code source

* Décentraliser l'intégration continue
  * [radicle CI](https://radicle.xyz/2025/07/23/using-radicle-ci-for-development), cache partagé
* Décentraliser la distribution des paquets de logiciels Open-Source
  * [IPFS](https://ipfs.tech), MPFS
* Décentraliser le financement de projets _open source_
  * [Drips](https://www.drips.network)
* Décentraliser les outils de communication et collaboration
  * [ATProto](https://atproto.com)

::: notes

:::

## Et l'IA dans tout ça?

* Une nouvelle force centralisatrice
* Un nouveau moyen de captation de valeur
* Des alternatives auto-hébergées et/ou open-source émergent
  * [Ollama](https://ollama.com), [HuggingFace](https://huggingface.co), [vLLM](https://github.com/vllm-project/vllm)

## Points à retenir

* Les outils et processus décentralisés sont plus résilients, ouverts et libres
* C'est la promesse originelle de l'informatique personnelle, du _World Wide Web_, des DVCS...
* Cette promesse est souvent oubliée en échange de _commodité_
* Et si on commençait par libérer notre principal outil, le code, de la centralisation ?

# Postface

## Les histoires d'amour...

![](/images/trou-noir-github.jpg)

## Les histoires d'amour...

* Fin 2025, l'équipe [décide](https://github.com/cardano-foundation/hal/blob/be1bec5a41ca594ad8a4e541e5034acf90cb8223/docs/radicle.md#L123) de retourner sur GitHub:
  * Barrière à l'entrée pour les contributeurs potentiels
  * Besoin de certaines fonctionnalités de GH (Pages, Actions)
  * Difficulté à synchroniser Radicle et GitHub

## Avis

![](/images/snowcamp-26-rating.png)

## Crédits

* Le Soir : [Trump & Macron 14 juillet](https://www.lesoir.be/sites/default/files/dpistyles_v2/ena_16_9_extra_big/2017/07/14/node_104609/4412350/public/2017/07/14/B9712609397Z.1_20170714123746_000+GAM9EKRAF.1-0.jpg?itok=Y9XgK-Ky1553016802)
* INA : [Delors entarté](https://mediaclip.ina.fr/media/videos/imagettes/886x498/332/CAB97106147.jpeg)
* [Trump en colère](https://pbs.twimg.com/media/C3VSo4JWMAA7TA8?format=jpg&name=4096x4096)
* [Dîner des Tech Bros](https://s.yimg.com/ny/api/res/1.2/L2FAIS8kafiVyJ_DPqv16w--/YXBwaWQ9aGlnaGxhbmRlcjt3PTk2MDtoPTY0MDtjZj13ZWJw/https://media.zenfs.com/en/cbs_news_897/5bd253ee5a00c50c6a66ea0770db3749)
* [Et la Savoie redevint indépendante](https://www.ledauphine.com/politique/2020/10/05/et-la-savoie-redevint-independante)
* [Effet Démo Bonaldi](https://medium.com/future-haigo/comment-briller-en-démo-lorsque-lon-est-développeur-bef02e9f86db)
* [Trou noir](https://trustmyscience.com/wp-content/uploads/2016/11/trou-noir-supermassif-1024x576.jpg)
* [Pierre Kropotkine](https://www.babelio.com/auteur/Pierre-Kropotkine/63248)
* [Ayn Rand](https://vocal.media/viva/ayn-rand-the-unknown-ideal-woman)
