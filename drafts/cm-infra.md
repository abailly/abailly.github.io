------------
title: Haskell-based Infrastructure Management
author: Arnaud Bailly 
date: 2015-10-26
------------


In a [previous post](/posts/cm-arch-design.html) I described the overall design and architecture of our core system and my
experience developing this system over the past year. In this installment of the series I plan to write on my Haskell experience as
a startuper [^1] I shall describe how we setup our development and operations environment using mostly Haskell tools and code.

# Principles

* Everything Docker and containers
* One Single Source of Authority: Everything should be versioned and we should be able to recover the whole environment from some
git repository

# Components

## Build

* Using Cabal (there was no stack one year ago...)
* On top of it we setup shake to orchestrate building of various containers

## Continuous Integration

## Production

## Monitoring

# Conclusion


[^1]: Or is it my startup experience as a Haskeller?
