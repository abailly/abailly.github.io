---
title: Fonctionnellement vôtre
author: Arnaud Bailly - @dr_c0d3
institute: Input Output Global
date: 2023-09-25
theme: beige
revealjs-url: /reveal.js
---

# Introduction

## Plan

* Pourquoi ?
* Théorie de l'_Event Sourcing_
* Travaux pratiques
* Conclusion

## D'où parlé-je ?

* Architecte des projets [Hydra](https://hydra.family) et [Mithril](https://mithril.network) chez [IOG](https://iohk.io)
* Praticien de l'_eXtreme Programming_ depuis 15 ans
* Compagnon de route du _Domain Driven Design_

# Pourquoi ?

## Le modèle de données relationnel

----

![](/images/relational-model.gif)

## Les avantages des SGBDR

* Très pratique pour effectuer des requêtes: _SQL_ est un très bon langage !
* Conceptuellement simple : tout est _table_ (_relation_)
* Pléthore d'outils et de services

## Les inconvénients des SGBDR

* Les insertions et mises à jour sont complexes
* *Défaut d'adaptation d'impédance* : la plupart des domaines ont des données de type arborescentes ou même graphes
* Avoir une seule base de données pour tout $\Longrightarrow$ *SPOF*
* Une base de données c'est comme une **variable globale mutable**

## États et Transitions

![](/images/state-transitions.png)

## États et Transitions

* Les SGBDR stockent **l'état** du domaine à un instante $t$...
* ... mais les **transitions** sont aussi intéressantes...
* ... et l'état peut toujours être reconstruit à partir d'une *séquence de transitions*.

::: notes

* ceci suppose que les transitions soient déterministes

:::

# Théorie de l'_Event Sourcing_

## Le modèle _Event sourcing_

> Event Sourcing ensures that all changes to application state are stored as a sequence of events. Not just can we query these
> events, we can also use the event log to reconstruct past states, and as a foundation to automatically adjust the state to cope
> with retroactive changes.

> [Martin Fowler](http://martinfowler.com/eaaDev/EventSourcing.html)

## Stocker un flux d'événements

rend plus facile:

* $\dots$ d'écrire des tests
* $\dots$ d'auditer l'état courant du système et son passé
* $\dots$ d'implanter un mécanisme de _undo/redo_
* $\dots$ d'exécuter des simulations sur des données "live"
* $\dots$ de gérer les migrations de format de données et les versions multiples
* $\dots$ de gérer des changements potentiellement conflictuels

::: notes

* undo/redo => évévenements réversibles
* migration -> versionage des événements
* conflits -> l'utilisation d'un journal des opérations est le mécanisme sous-jacent implanté par les SGBD(R) pour gérer les conflits et les propriétés ACID

:::

## Événements et "domaine métier"

* Les événements représentent les aspects _dynamiques_ d'un modèle
* L'[Event Storming](http://ziobrando.blogspot.sg/2013/11/introducing-event-storming.html) est une technique efficace de modélisation partageable entre experts métiers et experts logiciels
* Ils sont une pierre angulaire du [Domain Driven Design](http://www.amazon.fr/Domain-Driven-Design-Tackling-Complexity-Software/dp/0321125215) $\longrightarrow$ Un langage ubiquitaire et des modèles plus adéquats

## _Event Sourcing_ et _Command Sourcing_

![](/images/transducer.jpg)

## _Event Sourcing_ et _Command Sourcing_

* _événement_ et _commandes_ sont équivalents uniquement si l'interprétation d'une commande est déterministe
* interpréter une commande nécessite l'intégralité des règles métiers
* interpréter un événement est _indépendant_ des règles métiers

# Travaux pratiques

## Un _Core domain_ pure

* Une _Commande_ calcule un événement en fonction de l'état courant

    ```haskell
    eval :: Command -> State -> Event
    ```

* Un _événement_ met à jour l'état

    ```haskell
    apply :: Event -> State -> State
    ```

## Des _Services_ impures

> Services are used to orchestrate interaction between one or more business models and the outside world

* Un _Service_ opère sur *plusieurs contextes*
* Un _service_ peut être _synchrone_ ou _asynchrone_
* Il n'y a pas *transactions réparties*: Un service doit gérer les erreurs et incohérences de chaque contexte

## Méta-données d'événement

Un exemple de structure d'événement:

```
data StoredEvent s = StoredEvent { eventVersion :: EventVersion
                                 , eventType    :: EventType s
                                 , eventDate    :: Date
                                 , eventUser    :: UserId
                                 , eventRequest :: Encoded Hex
                                 , eventSHA1    :: Encoded Hex
                                 , event        :: ByteString
                                 }
```

## Stockage des événements

* Chaque événement a une version qui est utilisé pour sa représentation binaire
* Un événement comporte des méta-données utiles pour l'audit : l'utilisateur ayant initié la commande, un horodatage, un identifiant de requête, etc.
* Une grande variété d'options, depuis un simple fichier en mode _ajout_ jusqu'à une base de données répliquée
* Le _backend_ de stockage doit garantir la linéarisation des écritures

## Exemple : comptes bancaires

* Un compte bancaire a un _identifiant_ et un _solde_
* Un utilisateur peut effectuer des _dépôts_, des _retraits_ et des _transferts_
* Un dépôt est toujours possible
* Un retrait n'est possible que si le solde est supérieur ou égal au montant
* Un transfert n'est possible qu'entre 2 comptes différents et si le solde du compte débité est suffisant

# Conclusion

## Event sourcing + Haskell = :rocket:

* Implante nativement le "pattern" [Noyau fonctionnel, Enveloppe impérative](https://www.destroyallsoftware.com/screencasts/catalog/functional-core-imperative-shell)
* Suit une approche de _Conception Orientée Domaine_
* Adéquat pour le _Développement dirigé par les tests_

## Et si on ne fait pas de Haskell ?

Les principes sont valables dans tous les langages:

* Un flux d'événements uniques, horodaté, versionné, structuré...
* Des _commandes_ et des _événements_ immutables définis avec le métier
* Un modèle purement _calculatoire_ facilement testable

## Pour aller plus loin

* Intégrer les événements systèmes et métiers dans un flux unique $\Longrightarrow$ [One Log](https://codesync.global/media/one-log-arnaud-bailly-yann-schwartz/)
* Une formation à l'[architecture fonctionnelle](https://funar.pankzsoft.com/)
