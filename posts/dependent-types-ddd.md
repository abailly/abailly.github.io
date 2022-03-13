---
title: Domain Driven Design, meet Dependent Types
author: Arnaud Bailly 
date: 2017-04-06
---

> This article aims to be the first post in a series exploring connection between [Domain Driven Design](https://en.wikipedia.org/wiki/Domain-driven_design) and [Dependent types](https://en.wikipedia.org/wiki/Dependent_type) as implemented in [Idris](http://idris-lang.org). I plan to go through all the examples scattered across Eric Evan's [seminal book](https://www.abebooks.com/products/isbn/9780321125217/22376984258), revisiting them in the light of functional programming with dependent types. My intuition is that using DT languages will allow us to produce better and safer designs.

Let's go first through some ceremonies in order to please the Gods of programming: 

~~~~idris
> module Cargo
> import Data.List
> %default total
~~~~~

We will go through the first example Eric Evans gives, p.17 of his book: A simple model describing booking of `Cargo` for `Voyage`s. We have some very simple data structure describing cargos and voyages. A cargo is described by its identification string and a size.

~~~~~idris
> record Cargo where
>   constructor MkCargo
>   cargo_id : String
>   size : Int
~~~~~

And a `Voyage` is a list of cargos, a total capacity and a confirmation order[^1]

~~~~idris
> record Voyage where
>   constructor MkVoyage
>   capacity : Int
>   orderConfirmation : Int
>   cargos : List Cargo
~~~~

Given a voyage, it is simple matter to compute the total booked cargo size for this voyage:

~~~~idris
> bookedCargoSize : Voyage -> Int
> bookedCargoSize = sum . map size . cargos
~~~~

Then booking a `Cargo` for a `Voyage` checks the voyage can accomodate the given cargo's size before adding it to its load:

~~~~idris
> makeBooking : Cargo -> Voyage -> Voyage
> makeBooking cargo voyage =
>   if bookedCargoSize voyage + size cargo < capacity voyage
>   then record { cargos = cargo :: cargos voyage } voyage
>   else voyage
~~~~

However, it is customary for shipping company to accept *overbooking*, say 10%. Our booking function then becomes:

~~~~idris
> makeBooking' : Cargo -> Voyage -> Voyage
> makeBooking' cargo@(MkCargo _ size) voyage@(MkVoyage capacity orderConfirmation cargos) 
>              =  if cast (bookedCargoSize voyage + size) > 1.1 * cast capacity
>                    then  voyage
>                    else record { cargos = cargo :: cargos } voyage 
~~~~

Obviously, this has the huge drawback of mixing different concerns: Updating the voyage with the cargo and computing the overbooking rule. What we want to do is to make this *overbooking policy* more explicit, say in a type:

~~~~idris
> OverbookingPolicy : Type 
> OverbookingPolicy = Cargo -> Voyage -> Bool
~~~~

Then our standard 10% overbooking policy is reified in its own function:

~~~~idris
> tenPercentOverbooking : OverbookingPolicy
> tenPercentOverbooking cargo@(MkCargo _ size) voyage@(MkVoyage capacity orderConfirmation cargos) = 
>   cast (bookedCargoSize voyage + size) > 1.1 * cast capacity
~~~~

and later on used to compute booking:

~~~~idris
> makeBooking'' : Cargo -> Voyage -> OverbookingPolicy -> Voyage
> makeBooking'' cargo voyage isAllowed
>              =  if isAllowed cargo voyage
>                    then voyage
>                    else record { cargos = cargo :: cargos } voyage 
~~~~

Simple and efficient. 

However, this function is somewhat lacking with respect to how much information it provides in its type: We know nothing about the transformed `voyage` that it produces and in particular we don't know if the `OverbookingPolicy` has been applied or not and if the `cargo` is part of the load. Having this information around in the type system would be handy to clients that need to take more decisions from this booking...

Enters dependent types: We will use a type to express the *proposition* that some `Voyage` contains some `Cargo` load which will be part of the outcome of the `makeBooking` function. This means we would like our `makeBooking` function to have the following signature:

~~~~idris
makeBooking'' : (cargo : Cargo) -> Voyage 
              -> OverbookingPolicy 
              -> (voyage' : Voyage ** Dec (HasCargo cargo voyage'))
~~~~

The return type is a *dependent pair* which associates an instance of `Voyage`, a value, with another value whose type depends on `voyage'`. Just like a standard pair this allows us to pack more information in our return type, namely the possibly updated `voyage` and a *proof* that `cargo` is part of `voyage'`, or not.

In order to decide whether a voyage `HasCargo` or not, we need to be able to decide whethere two `Cargo`s are equal or not which in Idris terms means implementing the interface `DecEq` for the `Cargo` type:

~~~~idris
> mutual
>   implementation DecEq Cargo where
>     decEq (MkCargo cid _) (MkCargo cid _) = 
>       case decEq cid cid of 
>         (Yes prf)   => rewrite prf in Yes Refl
>         (No contra) => No $ contra . fst . cargoInj
~~~~

The `cargoInj` is a utility function that allows us to relate a proof that the two cargo's ids differ (`contra`) to actual `Cargo` inhabitants.[^2]

~~~~idris
>   private
>   cargoInj : (MkCargo cid s = MkCargo cid' s') -> (cid = cid', s = s')
>   cargoInj Refl = (Refl, Refl)
~~~~

We can now define `HasCargo` type:

~~~~idris
> data HasCargo : (cargo : Cargo) -> (voyage : Voyage) -> Type where
>   CargoConfirmed : { auto prf : Elem cargo cargos } 
                   -> HasCargo cargo (MkVoyage cap order cargos)
~~~~

`HasCargo`'s only constructor, `CargoConfirmed`, given a proof that `cargo` is an element of `voyage`'s `cargos`, yields a proof that `cargo` is confirmed to be part of `voyage`. This is a convoluted way to assert the fact a cargo is inside the list of cargos carried by a voyage. Note that we expect the compiler to be able to provide the proof autoatically from the environment and so it is left *implicit*.

To be useful in our `makeBooking` function, we need to equip this type with a decision procedure, e.g. a function that produces a `DecEq (HasCargo ...)` instance given some cargo and some voyage:

~~~~idris
> mutual
>   hasCargo : (cargo : Cargo) -> (voyage : Voyage) -> Dec (HasCargo cargo voyage)
>   hasCargo cargo (MkVoyage capacity orderConfirmation []) = No voyageIsEmpty
~~~~

The case where the list of `cargos` of a `voyage` is empty is easy: Simply return a `No` with a contradiction `voyageIsEmpty` (see later). 
The non-empty case is a little bit trickier:

~~~~idris
>   hasCargo cargo (MkVoyage capacity orderConfirmation cargos) = 
~~~~

First we pattern-match to check whether or not the `cargo` is in the list which gives us a `DecEq (Elem cargo cargos)` instance:

~~~~idris
>     case isElem cargo cargos of
~~~~

In the `Yes` case, we simply produce `CargoConfirmed` and the compiler can use the `prf` proof of membership in scope to satisfy the requirements of the constructor:

~~~~idris
>       (Yes prf)   => Yes CargoConfirmed
~~~~

In the `No` case, we need to transform our proof by contradiction an element is not present in a proof by contradiction `HasCargo` does not hold, which means (again) composing functions to extract the proof from a `CargoConfirmed` instance and pass it to `contra`:

~~~~idris
>       (No contra) => No (contra . cargoConfirmed)
~~~~

This code makes use of the following utility functions as contradictions:

~~~~idris
> 
>   voyageIsEmpty : HasCargo cargo (MkVoyage capacity orderConfirmation []) 
>                 -> Void
>   voyageIsEmpty CargoConfirmed impossible
> 
>   cargoConfirmed : HasCargo cargo (MkVoyage capacity orderConfirmation cargos)
>                  -> Elem cargo cargos
>   cargoConfirmed (CargoConfirmed {prf}) = prf
~~~~

We are now fully armed to define `makeBooking'''` function which is just our `makeBooking''` function augmented with an actual proof telling us whether or not the `cargo` has been actually confirmed.

~~~~idris
> makeBooking''' : (cargo : Cargo) -> Voyage 
>                -> OverbookingPolicy
>                -> (voyage' : Voyage ** Dec (HasCargo cargo voyage'))
> makeBooking''' cargo voyage@(MkVoyage capacity orderConfirmation cargos) isAllowed = 
>   let voyage' = if isAllowed cargo voyage
>                   then MkVoyage capacity orderConfirmation (cargo :: cargos)
>                   else voyage
>   in (voyage' ** hasCargo cargo voyage') 
~~~~

In the original function (or method) defined in the book, `makeBooking` returned an integer which was supposed to be booking confirmation order, or -1 if the booking could not be confirmed. It seems to me the above formulation improves over this simple but potentially obscure *coding* of return type, explicitly embodying the success or failure to add the cargo to the voyage in the return *type* while concurrently updating the voyage. What I found really interesting is that while we are not only improving the cohesion/coupling through the separation of concerns `OverbookingPolicy` yields, we are also *opening* the function to other use thanks to the more precise return type.

Dependent types (or even non-obviously-dependent-yet-sophisticated type systems like Haskell's or OCaml's) really allows (or forces) us to reason in two stages: Things we can reason about at compile time and things we can reason about at run-time, with the added value of being able to express the two using the same expressions. I suspect these capabilities could be useful to provide more robust and expressive designs for real-world problems, and not only as a tool for automated theorem proving, and this is what I intend to explore in the next installments of this series.

> Thanks to [Rui Carvalho](https://twitter.com/rhwy) for the feedback and to [Crafting Software Meetup](https://www.meetup.com/fr-FR/Crafting-Software/events/238241119/) to give me the incentive to explore these topics

[^1]: This one I copied verbatim from the book but I don't use really in the code...

[^2]: I borrowed the technique from `Reflection.idr` source code. It is another instance of Curry-Howard where function composition is equated to transitivity of implication.
