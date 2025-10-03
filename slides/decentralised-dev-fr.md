---
title: Décentraliser le Processus de Développement
subtitle: Une expérience avec Radicle et quelques idées pour augmenter l'autonomie des équipes
author: Arnaud Bailly - @abailly.bsky.social
institute: Pankzsoft
date: 2025-10-02
revealjs-url: /reveal.js
---

# Une courte fiction...

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

* Le 14 juillet 2026, le Président Trump est invité au Palais de l'Élysée pour une réception suivant les célébrations du 14 juillet
* Un commando de gilets jaunes parvient à s'infiltrer dans le Palais et lui lance une tarte à la crème, gâchant son brushing
* De retour aux États-Unis, il envoie un commando de Navy Seals pour enlever les terroristes mais ils tombent dans une embuscade tendue par un groupe d'agriculteurs en colère et échouent leur mission
* En représailles contre la France, il ordonne aux entreprises technologiques américaines de supprimer tous les comptes et données associés aux entreprises françaises et de leur interdire l'accès à ces plateformes

:::

# Introduction

## Plan

* Pourquoi décentraliser le développement logiciel ?
* Retour d'expérience avec Radicle
* Conclusion

## D'où parlé-je ?

* Dev Senior/Tech Lead/Architecte/Consultant/🤡 depuis plus de 30 ans
* Praticien convaincu de l'_eXtreme Programming_
* À la recherche de l'_autonomie_ depuis 1969

## Trop Long ; Pas Resté

* Les services centralisés échangent de la _commodité_ contre du _contrôle_ (et/ou des frais). C'est bien...
* ... jusqu'à ce que ça ne le soit plus
* Des solutions décentralisées (ré-)émergent comme [Radicle](https://radicle.xyz)
* Cela offre aux équipes à la fois commodité **et** contrôle sur le processus de développement

# Pourquoi décentraliser le développement logiciel ?

## Un peu de terminologie

* Décentralisation ≠ Distribution
* Les équipes distribuées sont devenues courantes dans le développement logiciel
* Les organisations décentralisées sont plutôt rares
* La _coordination_ décentralisée est **difficile**

::: notes

distribution est un anglicisme, le vrai mot c'est répartition

:::

## Pourquoi _centraliser_ les outils de développement ?

## Commodité

* Déléguer la complexité de la configuration et de la gestion des outils à des spécialistes
* Offrir de meilleures interfaces et "expériences" utilisateur
* Faciliter l'intégration avec d'autres outils/services

## Efficacité

* Diminuer le _TCO_ en mutualisant les coûts d'infrastructure
* Déléguer le coût de la gestion de l'infrastructure à quelqu'un d'autre
* Se spécialiser ↦ Se concentrer sur le domaine _principal_ de l'organisation

::: notes

TCO = Total Cost of Ownership
:::

## Contrôle

* Fournir un point de contrôle unique pour les organisations
* "Industrialiser" les pratiques et outils à travers l'organisation et des secteurs entiers

## Pourquoi _décentraliser_ les outils de développement ?

## Reprendre la _contrôle_

* Éviter l'extraction de valeur effrénée des "mégacorporations"
* Éviter la censure des gouvernements
* Éviter le verrouillage par les fournisseurs

## Donner du pouvoir aux équipes et aux individus

Vous souvenez-vous du [Manifeste Agile](https://agilemanifesto.org/) ?

![](/images/agile-manifesto.png)

## Donner du pouvoir aux équipes et aux individus

* Il n'y a pas de solution _universelle_
* Permettre aux équipes de trouver des solutions qui correspondent à _leur_ contexte
* Augmenter l'efficacité locale et éviter l'inflation

## Augmenter la Résilience

* Supprimer le _Point Unique de Défaillance_
* Distribuer la charge à travers le "réseau"
* S'adapter à un environnement changeant

## Lutter contre l'Enshittification

![](/images/this-is-fine.jpg)

# Retour d'expérience

## Un peu de contexte...

* Travail sur [Cardano](https://cardano.org), un réseau blockchain et une crypto-monnaie
* La décentralisation est (ou devrait être) au cœur de la blockchain
* Pourtant 100% du code est hébergé sur GitHub !

## Expérimentations

L'[équipe HAL](https://app.radicle.xyz/nodes/seed.hydra.bzh/rad%3Az4QBQYzMP9DtUJmejVyDHkCyTVj8G) expérimente [Radicle](https://radicle.xyz) depuis mai 2025

## Qu'est-ce que Radicle ?

> Radicle est une pile de collaboration de code open source et peer-to-peer construite sur Git. Contrairement aux plateformes d'hébergement de code centralisées, il n'y a pas d'entité unique contrôlant le réseau. Les dépôts sont répliqués entre pairs de manière décentralisée, et les utilisateurs ont le contrôle total de leurs données et de leur flux de travail.

Plus d'infos sur [https://radicle.xyz](https://radicle.xyz)

## En résumé

![](https://radicle.xyz/assets/images/p2p-network.svg)

## Démo

![](/images/bonaldi-demo.jpg)

## Travailler avec Radicle

Les retours de l'équipe sont globalement positifs

* Excellente expérience développeur avec la CLI
* Flux de travail collaboratif fluide
* Possède toutes les fonctionnalités essentielles nécessaires pour collaborer sur des "petits" projets

----

Quelques défauts et problèmes :

* Décalage de fonctionnalités entre l'interface utilisateur et la CLI
* Ne pas avoir d'autorité centrale a causé quelques "tâtonnements"
* Nécessite une connaissance plus approfondie de git
* 🧩 Comment faire du Trunk-Based Development correctement ?

# Conclusion

## Plus d'expérimentations

![](/images/buzz.jpg)

## Radicle CI

![](/images/radicle-ci-ui.png)

::: notes

* Travail en cours pour fournir une CI déclenchée par les événements radicle depuis un nœud
* Intégration avec divers moteurs d'exécution, notamment GHA, Concourse, CircleCI ou personnalisés (Ambient, Native, conteneurs)
* Enregistre des COBs de _travaux_ qui sont disponibles pour tous les pairs
* Excellente combinaison avec les builds reproductibles -> permet la réutilisation décentralisée des artefacts de build

:::

## Contrôle d'Accès aux Ressources et Audit

Collaboration démarrée avec [Antithesis](https://antithesis.com)

* Antithesis est un SaaS pour le _Test de Simulation Déterministe_
* Nous voulions fournir ce service à toute la communauté Cardano
* Construction d'un outil CLI pour déclencher et tracer les exécutions de tests sur la blockchain Cardano
* Plus d'infos sur le [Dépôt](https://app.radicle.xyz/nodes/seed.hydra.bzh/rad%3Az2a7Te5b28CX5YyPQ7ihrdG2EEUsC)

## Plus d'idées et de projets

* Partager les artefacts construits localement
* Réseau de Confiance Décentralisé pour les packages de logiciels Open-Source
* Utiliser [ATProto](https://atproto.com) pour la collaboration d'équipe(s)
* ...

## Points à retenir

* Les outils et processus décentralisés sont plus résilients et ouverts
* C'est la promesse originelle de l'informatique personnelle, du _World Wide Web_, des DVCS...
* Cette promesse est souvent oubliée en échange de _commodité_
* Des outils comme _Radicle_ offrent à la fois _commodité_ et _contrôle_

# Crédits

* Le Soir : [Trump & Macron 14 juillet](https://www.lesoir.be/sites/default/files/dpistyles_v2/ena_16_9_extra_big/2017/07/14/node_104609/4412350/public/2017/07/14/B9712609397Z.1_20170714123746_000+GAM9EKRAF.1-0.jpg?itok=Y9XgK-Ky1553016802)
* INA : [Delors entarté](https://mediaclip.ina.fr/media/videos/imagettes/886x498/332/CAB97106147.jpeg)
* [Trump en colère](https://pbs.twimg.com/media/C3VSo4JWMAA7TA8?format=jpg&name=4096x4096)
* [Dîner Tech](https://s.yimg.com/ny/api/res/1.2/L2FAIS8kafiVyJ_DPqv16w--/YXBwaWQ9aGlnaGxhbmRlcjt3PTk2MDtoPTY0MDtjZj13ZWJw/https://media.zenfs.com/en/cbs_news_897/5bd253ee5a00c50c6a66ea0770db3749)
* [Analyse du meme "This is fine"](https://medium.com/@CWSkelly/analysis-this-is-fine-meme-e8980ff61e78)
* [Effet Démo Bonaldi](https://medium.com/future-haigo/comment-briller-en-démo-lorsque-lon-est-développeur-bef02e9f86db)
