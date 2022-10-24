---
title: Dependently Typed Programming for Dummies
subtitle: More powerful types for better domain modeling
author: Arnaud Bailly - @dr_c0d3
date: 2022-10-24
theme: virgil-black-iohk
revealjs-url: /reveal.js
---

## Agenda

* Why?
* Gregorian dates
* Double-Entry Accounting
* An Online Wargame
* Conclusion

# Why?

## The Quest for Better Software

* _Test-Driven Development_ $\rightarrow$ Executable "specification" of goal
* _Domain-Driven Design_ $\rightarrow$ Expressing solution in the language of the problem
* _TDD + DDD_ $\rightarrow$ _Type-Driven Development_?

::: notes

* Question mark implies I am still _exploring_ the relevance of using dependent types in domain modelling
* There's definitely a dimension of fun and learning new stuff => never did any Coq or Agda before!
* I became involved in Idris because I was on of Edwin's book reviewers

:::

## Goal(s) for this Talk

* Make PLT researchers :smile:
* Give people incentive to start hacking in and on Idris 2
* Demonstrate 1st class _Dependent Types_ are useful to grow software
* Spark discussions on possibles use cases for Idris 2 w/in Cardano

::: notes

* :smile: both from the naiveté of my approach and the amusement to see people using their stuff to do "mundane" programming tasks

:::

## Caveat Emptor

![](/images/dont-know-what-am-doing.jpeg)

## Caveat Emptor

> No theorem has been harmed in making this talk

* This talk is an experience report from a professional developer but not being paid for coding in Idris
* It definitely leans towards the D side of R&D!

# A first dip: Dates

## Problem Statement

> Define a Gregorian calendar `Date` type along with an `addDays` function. The `Date` must always be correct, eg. it should not be possible to define a `Date` like 2022-02-29 or 2020-06-31.

## Base types: `addDays`

```idris
addDays :  (d : Date)
        -> (n : Nat)
        -> Date
```

::: notes

* Some minor differences with Haskell
* Types are declared with single colon character
* Type of parameters can be named, which is useful for both documentation and referring to those names as variables in in other parts of the signature

:::

## Base types: `Date`

The `MkDate` constructor embeds constraints we are interested in

```idris
Year : Type
Year = Nat

data Month = January | February | March | ...

data Date : Type where
  MkDate : (year  : Year) -> (month : Month ) -> (day : Nat)
         -> { auto dayFitInMonth : LTE day (daysInMonth month year) }
         -> { auto dayGreaterThanOne : LTE 1 day }
         -> Date
```

::: notes

* type aliases are just type-level CAFs or functions
* data types can be declared like in Haskell but usually are declared with GADTs syntax
* This might not be the representation, something I learnt later: Adding constraints to constructors this way is cumbersome in larger programs
* Would have been better to define a proper types for days

:::

## Implementing `addDays`

Grunt of the work is done by the `addOneDay` function

```idris

addDays d Z     = d
addDays d (S k) = addDays (addOneDay d) k
```

::: notes

* usual pattern matching and recursion

:::

## Implementing `addOneDay`

The easy case

```idris
addOneDay (MkDate year month day) =
  case order {to=LTE} (S day) (daysInMonth month year) of
    Left _  =>
      MkDate year month (S day)
```

::: notes

* `order a b` result type is `Either (LTE a b) (LTE b a)` -> we get a proof of the relation between 2 numbers
* `Left` is the easy case: the given day is strictly less than the number of days in the month so we can just add it

:::

## Implementing `addOneDay`

Another easy case!

```idris
Right _ =>
  case month of
    December => MkDate (year + 1) January 1
```

## Implementing `addOneDay`

The slightly trickier case

```idris
Right _ =>
  case month of
    December => ...
    _        =>
        let firstDayOfMonth = aMonthHasOneDay (nextMonth month) year
        in MkDate year (nextMonth month) 1
```

::: notes

* we need to provide a proof that 1 is a valid day for the next month

:::

## Implementing `addOneDay`

```idris
monthDuration : Month -> Year -> (days: Nat ** LTE 1 days)
...

aMonthHasOneDay : (month : Month) -> (year : Year)
               -> LTE 1 (daysInMonth month year)
aMonthHasOneDay month year with (monthDuration month year)
  | (_ ** prf) = prf

```

::: notes

* We make use of Σ-types, eg. type of tuples where the second type depends on the fist one to construct an `LTE` proof that 1 is always
`LTE` than the days in the month

:::

## Testing in the REPL

```
λΠ> :let date = MkDate 2022 December 25
λΠ> addDays 3 date
MkDate 2022 December 28 : Date
λΠ> addDays 7 date
MkDate 2023 January 1 : Date
```

## Enforcing Invariants at the Hexagon's Sides

![](/images/hexagonal-architecture-complex.gif)

## Parser

A building block for a better parser combinator-style parser

```idris
toDate : String -> String -> String -> Either String Date
toDate y m d = do
  year <- mkYear y
  month <- mkMonth m
  day <- mkDay d
  case isLTE day (daysInMonth month year) of
    (Yes prf) =>
       case isLTE 1 day of
         (Yes prf') => pure $ MkDate year month day
         (No _) => Left "Error: ..."
    (No _) => Left "Error: ..."
```

::: notes

* use the `DecEq` type and `isLTE` function to produce needed proofs that the day is within the bounds for the given month
* the `prf` and `prf'` are in scope when calling the  `MkDate` constructor so they are implicitly passed
* this is a building block for a better parser combinator-style parser which is provided by `lightyear` package
* the types force us to take care of the various malformed cases in `String`
:::

## Testing in the REPL

```
Type checking ./Date.idr
λΠ> toDate "2022" "2" "28"
Right (MkDate 2022 February 28) : Either String Date
λΠ> toDate "2022" "2" "29"
Left "invalid number of days 29 in month 2 and year 2022" : Either String Date
```

# A more involved example: Accounting

## Double-Entry Accounting Domain

From [Wikipedia](https://en.wikipedia.org/wiki/Double-entry_bookkeeping_system)

> In the double-entry accounting system, at least two accounting entries are required to record each financial transaction. [..] Recording of a debit amount to one or more accounts and  an equal credit amount to one or more accounts results in total debits being equal to total credits for all accounts in the general ledger.
> ...

## Business rules

* A _transaction_ comprises at least 2 _entries_
* An _entry_ records a _debit_ or _credit_ amount in an _account_
* An _account_ can fall into 5 different categories: _asset_, _liability_, _equity_, _expense_, or _revenue_
* The aggregate _balances_ for debit and credit of all accounts should be equal
* A _book of accounts_ should preserve a _fundamental equation_ that ensures `asset = liability + equity`

::: notes

* technically, the "fundamental equation" requires adding revenues and expenses to form net profit which balances the equation each year
* check with your local accountant for the details...

:::

## First Model

`Balance` as a simple amount + `Direction`

~~~~idris
Balance : Type
Balance = (Nat, Direction)

data Account : Type where
  MkAccount : String -> { type : AccountType } -> Account

record Entry where
  constructor MkEntry
  amount : Balance
  account : Account
~~~~

## First Model: Transaction

A `Transaction` is some metadata + constrained vector of `Entries`

~~~~idris
data Entries : Type where
  MkEntries : (entries : Vect n Entry) ->
              { auto need2Entries : LTE 2 n } ->
              { auto balanced : balance entries = (0, Cr) } ->
              Entries

record Transaction where
  constructor Tx
  label : String
  date : Date
  entries : Entries
~~~~

::: notes

* A transaction has at least 2 entries
* The entries must be balanced => sum up to 0

:::

## First Model: Book of Accounts

`BookOfAccounts` embeds the "fundamental equation" constraint

~~~~idris
data BookOfAccounts : Type where
  BookTransactions :
     (txs : Vect k Transaction) ->
     { auto fundamentalEquation :
         inverse (assets txs <+> expenses txs) =
           liabilities txs <+> capital txs <+> revenues txs } ->
     BookOfAccounts
~~~~

::: notes

* directly express the equation as a type-level equality

:::

## Unit Testing with the Compiler

The `(=)` type allows us to write simple tests as types!

~~~~idris
valid2 : balance [ MkEntry (100, Cr) Bank,
                  MkEntry (100, Dr) Capital ] = (0, Cr)
valid2 = Refl

invalid : Not (balance [ MkEntry (100, Cr) Bank,
                         MkEntry (101, Dr) Capital ] = (0, Cr))
invalid = \ Refl impossible
~~~~

::: notes

* We can have negative tests too
* I like having the tests close to the code
* In general the testing story in Idris (2) is not so great (more on this later)

:::

## A Flaw in the Model

Trying to prove the `Monoid` properties for a `Balance` forced me to realize my model was wrong:

* I am conflating two concepts: The _Balance_ and the _Amount_ of each entry
* `Balance` should have a proper domain-relevant representation
* Idris' compiler has a good way to tell you your design is wrong: Proving properties becomes increasingly difficult (© Ohad Kammar)

## Better Types

Separate the `Amount` from the `Direction` of the `Balance`

~~~~idris
data Amount : Type where
  MkAmount : (n : Nat) -> { auto notZero : LTE 1 n } -> Amount
~~~~

~~~~idris
data Balance : Type where
  Zero : Balance
  Bal : (n : Amount) -> (d : Direction) -> Balance
~~~~

::: notes

* we distinguish the 0 (base) case just like for Peano numbers
* there might be a more suitable representation to make `Amount` strictly positive

:::

## A proper `Monoid` instance

Key function is `compensate` which produces a correct `Balance` (which is isomorphic to $\mathbb{Z}$)

```idris
Semigroup Balance where
  Zero      <+> y           = y
  x         <+> Zero        = x
  (Bal n d) <+> (Bal n' d') with (decEq d d')
    | (Yes prf) = Bal (n + n') d
    | (No contra) = compensate n d n' d' { notEqDir = contra }

Monoid Balance where
  neutral = Zero
```

::: notes

* `Balance` is actually a proper group
* coding `compensate` was somewhat involved, and I needed a bunch of lemmas to prove the substraction of the two numbers was not zero

:::

## Enforcing Invariants upon Reading

To build a `BookOfAccounts` we need to provide the required proof transactions are balanced.

```idris
parseBookOfAccounts : Parser BookOfAccounts
parseBookOfAccounts = do
  rawTxs <- sepBy parseTransaction (many endOfLine)
  let txs = fromList rawTxs
  case decEq (inverse (assets txs <+> expenses txs))
             (liabilities txs <+> capital txs <+> revenues txs) of
    (Yes prf) => pure $ BookTransactions txs
    (No contra) => fail "Error"
```
## Arnaud In Wonderland

Things start to get tricky when trying (again) to prove `Monoid` properties on a `Balance`

![](/images/balance-proof.png)

## Arnaud In Wonderland {transition=none}

* [Proofs](https://github.com/abailly/xxi-century-typed/blob/master/idris/src/Accounting/Proofs.idr) of *associativity* quickly became bogged down into a maze of various cases
* Embedding proof obligations in _data types_ means we have to _discharge_ those proofs to construct values, which leds to more work
* The `Balance` representation is probably still wrong and needs to be _distilled_`

# A Complete Application: Bautzen 1945

## Background

![](/images/vaevictis-135.jpeg)

::: notes

* I used to play wargames when I were younger but stopped out of lack of time and partners
* Vae Victis is a magazine that proposes short and easy to play games every 2 months (similar to Strategy & Tactics in the US)
* I wanted to do something more involved in Idris
* A game sounded like the right fit

:::

## Challenges

* Idris does (or did) not have much support for Web Apps: No native HTTP(S) server, no API/UI/HTML framework, no JSON...
* Edwin Brady developed Idris 2 while I was half-way through the game!
* Support for concurrency, multithreading, or networking is also quite nascent

## Short history

* Spring 2019: Started coding pure game logic in Idris1
* Autumn 2019: Converted existing code to Idris2
* 2020: Client and server executables, stuck with networking
* 2021: Not much work, busy building Hydra
* 2022: Make it playable with UI in browser

::: notes

* basic event-sourced architecture in mind (eg. `Command`/`Event` )
* got a basic client-server single game executable working reasonably quickly
* had to port networking code from Idris1 to 2
* got stuck for a while because of a subtle issue in how C FFI works in Chez scheme (someone else found the issue)
* put a haskell proxy in front of server to provide WS support

:::

## Idris 1 $\rightarrow$ Idris 2

* Idris2 was a full rewrite of the compiler using Idris (1, then 2) itself
* It's based on [Quantitative Type Theory](https://arxiv.org/abs/2104.00480) ([Atkey, 2018](https://www.researchgate.net/publication/326024161_Syntax_and_Semantics_of_Quantitative_Type_Theory))
* It (initially) used [Chez Scheme](https://cisco.github.io/ChezScheme/) as its code generation backend
* Had degraded DevX initially

::: notes

* QTT generalises irrelevance and linearity by allowing to add quantities 0, 1, or ω to symbol's declarations
* using scheme seems quite a smart move as it gives one a full blown RTS and optimising compiler for "free"
* Lack of libs, lots of small differences that might entail significant changes in the code, lack of good IDE support...
* it's getting better over time of course

:::

## The project today

![](/images/bautzen-map.png)

## The project today

* 45 Idris2 source files for a total of 4360 LoC
* Got basic interactions and game flow working
* Far from being able to play a full game but it's taking shape!
* checkout project on [GitHub](https://github.com/abailly/hsgames/)

## Juicy Bits: Command and Event

```idris
data Command : Type where
  Place : (unitName : String) -> (pos : Pos) -> Command
  MoveTo : (unitName : String) -> (to : Pos) -> Command
...
```

::: notes

* Command expresses the "inputs" to the game and as such is not dependent: Possibly the user can issue an invalid command at any time
* simpler type
:::

## Juicy Bits: Command and Event {transition=none}


```idris
data Event : (segment : GameSegment) -> Type where
  Placed : (unit : GameUnit) -> (pos : Pos) -> Event Setup
  Moved : (unit : GameUnit) -> (from : Pos) -> (to : Pos) -> (cost : Cost)
        -> { auto prf : LTE (toNat cost) (currentMP unit) }
        -> Event Move
```

::: notes

* Events need to be richer and express proper constraints of the underlying model (game)
* Type is indexed by the segment in which the event is valid
* `Moved` has an additional constraint to guarantee cost is within MPs of the unit

:::

## Juicy Bits: Command and Event {transition=none}

Type-safe event-sourced application

```idris

act : (game : Game) -> Command
    -> Either GameError (Event (curSegment game))

apply : (st : GameState) -> Event (st.stateSegment)
      -> GameState
```

::: notes

* act guarantees it produces an `Event` which is valid for the `Game`'s segment
* apply guarantees it can only ever be called with an event valid for the current game segment

:::

## Dependent Pair in Action

Using dependent pairs to provide a proof the cost of movement is valid

```idris
movementCost : (unit : GameUnit) -> ...
    -> Either GameError (cost : Cost ** LTE (toNat cost) (currentMP unit))
movementCost unit .... with (cost ...)
  movementCost unit  | Impossible = Left (ForbiddenTerrain from to)
  movementCost unit  | c =
    case isLTE (toNat c) (currentMP unit) of
      Yes prf => Right (c ** prf)
      No _    => Left (NotEnoughMPs unit from to (toNat c))
```

::: notes

* This is required to build a proper `Moved` event!

:::

## Juicy Bits: Domain Language

Trying to stay as close as possible to how the rules are expressed in English (actually French...)

```idris
stacking units terrain =
  if moreThan2DifferentFormations || unitsFromDifferentNations
  then CannotStack
  else let sp = sum $ map sps units
       in if sp <= stackingLimit terrain
          then Stacked sp
          else Overstacked sp
```

## Juicy Bits: Functional Data Structures

Implement A* algorithm using [Okasaki](https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf)'s Heap

```idris
computeShortestPath :
  (fuel : Nat) -> (units : List (GameUnit, Pos))
   -> (gameMap : Map) -> (unit : GameUnit)
   -> (costsMap : SortedMap Pos Nat)
   -> BinaryHeap AState
   -> List Pos
```

::: notes

* Tried to implement a Binomial Heap but I gave up after a while because of the complexity of binary arithmetics
* Tried to get inspiration from implementations in Agda and Coq but failed
:::

## Juicy Bits: Functional Data Structures

There are some interesting use of `rewrite` here...

```idris
  mergeTree (Node elem left right {k} {n})
            (Node elem' left' right' {k=k1} {n=n1}) | False =
    rewrite sym (plusSuccRightSucc (k + n) (k1 + n1)) in
    rewrite plusCommutative (plus k n) (plus k1 n1) in
    rewrite plusSuccRightSucc (k1 + n1) (k + n) in
    rewrite sym (plusAssociative k1 n1 (S (k + n))) in
    rewrite plusCommutative n1 (S (plus k n)) in
    makeNode elem' left'
       (mergeTree (Node elem left right) right')
```

## Juicy Bits: Functional Data Structures {transition=none}

By the way, I lied!

```idris
  mergeTree (Node elem left right {k} {n})
            (Node elem' left' right' {k=k1} {n=n1}) | False =
    rewrite sym (plusSuccRightSucc (k + n) (k1 + n1)) in
    rewrite plusCommutative (plus k n) (plus k1 n1) in
    rewrite plusSuccRightSucc (k1 + n1) (k + n) in
    rewrite sym (plusAssociative k1 n1 (S (k + n))) in
    rewrite plusCommutative n1 (S (plus k n)) in
    makeNode elem' left'
       (assert_total $ mergeTree (Node elem left right) right')
```

::: notes

* I was unable to convince the typechecker this function is _total_ although it looks so as we keep reducing the size of our arguments
* Used the "atomic weapon" to still declare the function is total
* What am I doing wrong?

:::

## Juicy Bits: Arithmetics

Use `Fin n` finite integers to ensure positions are always within the map's bounds

```idris
data Loc : (c : Nat) -> (r : Nat) -> Type where
  Hex : (col : Fin c) -> (row : Fin r) -> Loc c r

```

::: notes

* The grid is hexagonal
* Fin n are somewhat cumbersome to manipulate in general but they are a good fit here because the map is small (eg. 23 x 13 elements

:::

## Juicy Bits: Arithmetics

Use some [clever](https://www.redblobgames.com/grids/hexagons/) intermediate representation for distance in hexagonal grid

```idris
data Cube : Type where
  MkCube : (x : ZZ) -> (z : ZZ) -> Cube

cubeDistance : Cube -> Cube -> Nat
cubeDistance (MkCube x z) (MkCube x' z') =
  let y  = negate x - z
      y' = negate x' - z'
  in max (max (absZ (x - x')) (absZ (y - y'))) (absZ (z - z'))
```

## Error Handling

There are no exceptions in Idris but it provides syntax to short-circuit flow in monadic code

```idris
serve log sock clients gamesOutput gamesState = do
  log "awaiting clients"
  Right (s, addr) <- accept sock
    | Left err => pure (Left "Error")
  log $ "client connecting from " ++ show addr
  Right clientId <- clientHandshake log s
    |  Left err =>
          do log "Error"
             close s
             serve log sock clients gamesOutput gamesState
```

# Conclusion

## Takeaways: Modelling

* Proving things about your domain's model is _fun_ and _rewarding_
* Relating domain-level concepts to more abstract concepts can be a source of insights on the domain itself
* Trying to prove the relevant properties gives more insights on the _representation_ used

## Takeaways: Idris vs. Haskell

* :white_check_mark: Dependent Types are much easier to work with
* :white_check_mark: Better DevX in interactive development
* :x: Much smaller eco-system
* :x: Very much WIP so expect bumps
* :white_check_mark: Easy to add backends and use FFI
* :question: Low barrier of entry for contributing

::: notes

* I did not show interactive editing and use of IDE tooling
* It's still rough but already provides a lot of stuff that we are only recently getting in Haskell: navigation, local types
* Existing backends for JS, C, various Schemes... so could be a good alternative to native libraries

:::

## Things Not Covered

(Either not shown here or unknown to me...)

* Interactive editing
* Linear types and linear I/O
* Elaborator Reflection and meta-programming
* Packages management
* ... $\rightarrow$ Edwin Brady's [video](https://www.youtube.com/watch?v=nbClauMCeds)

## The Book

![](https://images.manning.com/720/960/resize/book/1/453215a-afa1-443f-9f2d-3b6bf24c34db/Brady-TDDI-HI.png)

## Links

* Idris [Discord Server](https://discord.gg/AbuHWhzh) is very welcoming
* This talk stemmed from a series of blog posts: [http://abailly.github.io/posts/dependently-typed-accounting.html](http://abailly.github.io/posts/dependently-typed-accounting.html)
* Code for  `Date` and `Accounting`: [https://github.com/abailly/xxi-century-typed](https://github.com/abailly/xxi-century-typed/blob/master/idris/src/Accounting.idr)
* Code for Bautzen 1945 [https://github.com/abailly/hsgames/blob/master/bautzen1945/](https://github.com/abailly/hsgames/blob/master/bautzen1945/)

## Questions?

![](/images/puzzled.jpg)
