---
title: Et si votre SI parlait votre métier ?
subtitle: Les bénéfices de la programmation fonctionnelle pour votre organisation
author: Arnaud Bailly - @dr_c0d3
date: 2019-04-24
transition: none
---

# Plan #

## Plan #

* Introduction
* M.Jourdain programme fonctionnellement
* Qu'est ce qu'un langage fonctionnel ?
* Développer
* Vérifier & Valider
* Le métier au coeur de l'architecture
* Conclusion

## There is no silver bullet...

> The hardest single part of building a software system is deciding precisely what to build. [..]
> Therefore one of the most promising of the current technological efforts, and one which attacks the essence, not the accidents, of the software problem, is the development of approaches and tools for rapid prototyping of systems as part of the iterative specification of requirements.

Frederic P. Brooks, Jr., 1986

# M.Jourdain programme fonctionnellement

## Excel

[Alerte de température](https://docs.google.com/spreadsheets/d/1bYNfa_ZNuJfRi4UWLVmXqwVTRL-lXfq5ACaJzXQCjZA/edit#gid=1186998741)

![](images/fp-excel-example.png)

## The Elm Architecture

![](images/the-elm-architecture.png)

## React/Redux

# Qu'est ce qu'un langage fonctionnel ?

## Fonctions

https://vimeo.com/113588389

![](images/function-universal-patterns.png)

## Structures de données immutables

## Filtrage de motifs

aka. _Pattern Matching_

## (Optionnel) Fonctions pures

## (Optionnel) Types

## Zoo

* Clojure: LISP moderne pour la JVM, typage dynamique
* Erlang: Systèmes répartis, typage dynamique
* Scala: FP & OO sont dans un bateau
* OCaml: FP + OO + Imperatif, français 🇫🇷
* F#: OCaml pour C#
* Haskell: Pure, paresseux, fortement typé
* Idris: Pure, typage dépendant, R&D

# Développer

## REPL

## Hole-Driven Development

## Refactoring

# Vérifier & Valider

## Typage statique "fort"

http://spiff.rit.edu/classes/phys317/lectures/boom/mars_co.html

![](images/mars-climate-orbiter.jpg)

## Property-Based Testing

## Types dépendants

## Vérification formelle

# Architecture & Design

## Architecture hexagonale

![](images/ports-and-adapters.gif)

d'après [Alistair Cockburn](http://alistair.cockburn.us/index.php/Hexagonal_architecture)

## DDD

# Conclusion
