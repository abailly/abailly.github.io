------------
title: A Dependently Typed Date
author: Arnaud Bailly 
date: 2017-04-19
------------

> This article is the [second installment](/posts/dependent-types-ddd.html) of a planned series of articles exploring connection between [Domain Driven Design](https://en.wikipedia.org/wiki/Domain-driven_design) and [Dependent types](https://en.wikipedia.org/wiki/Dependent_type) as implemented in [Idris](http://idris-lang.org), revisiting examples and patterns from Eric Evan's [book](https://www.abebooks.com/products/isbn/9780321125217/22376984258).

I was planning to work on the *Specification* pattern from chapter 9, pp.224 seq. of the book which is something that fits nicely, along with other patterns from this chapter, in a strongly typed purely functional framework. But I was sent off-track and ended up shaving a yak, or more precisely defining a `Date` type along with an `addDays` function, because this is something that was needed to define a proper *DelinquentInvoice* specification. This was a challenging exercise for me to undertake and I learnt a things or two in the process that might be worthwhile to share.

## Code 

So let's start defining our module. We will require all our functions to be *total* and export everything as public (probably something we want to restrict later on...).

~~~~idris
> module Date 
> 
> import Decidable.Order
> 
> %default total
> %access public export
~~~~~ 

Our first important type will be the `Month` which is straightforwardly implemented as an enumeration of months' names[^1].

~~~~idris
> data Month : Type where
>   January    : Month
>   February   : Month
>   March      : Month
>   April      : Month
>   May        : Month
>   June       : Month
>   July       : Month
>   August     : Month
>   September  : Month
>   October    : Month
>   November   : Month
>   December   : Month
~~~~~ 

There is an `Enum` typeclass/interface in Idris but it requires quite a lot of functions to be defined, so let's do something simpler and define only a `toNat` function which will be handy to compare months:

~~~~idris
> toNat : Month -> Nat
> toNat January    = 1
> toNat February   = 2
> toNat March      = 3
> toNat April      = 4
> toNat May        = 5
> toNat June       = 6
> toNat July       = 7
> toNat August     = 8
> toNat September  = 9
> toNat October    = 10
> toNat November   = 11
> toNat December   = 12
~~~~

It is then easy to compare months, providing implementations for `Eq` and `Ord` interfaces:

~~~~idris
> implementation Eq Month where
>   m1 == m2 = toNat m1 == toNat m2
> 
> implementation Ord Month where
>   compare m1  m2 = compare (toNat m1) (toNat m2)
~~~~

Now, let's tackle `Year`s. Easy enough, we will stick to simply representing years as a natural number, which of course will prevent us at this stage to represent dates *B.C.* 
~~~~idris
> Year : Type
> Year = Nat
~~~~ 

An important thing to know about a year is whether or not it is a *leap year*: This will be critical to know what's the duration of the year and the number of days of `February`. There is a simple algorithm for that: A year which is dividible by 4 is a leap year, except when it is dividible by 100 and *not* dividible by 400 (1900 is not a leap year but 2000 is).

~~~~idris
> isLeapYear : Year -> Bool
> isLeapYear y = check4 && check100 || check400
>   where
~~~~

Because we are working with `Nat`s (underlying representation for years) we need to use some special functions to compute modulus, namely `modNatNZ` which requires us to provide a proof the divisor is not 0.

~~~~idris
>     check4 : Bool
>     check4 = modNatNZ y 4 SIsNotZ == 0
> 
>     check100 : Bool
>     check100 = modNatNZ y 100 SIsNotZ /= 0
> 
>     check400 : Bool
>     check400 = modNatNZ y 400 SIsNotZ == 0
~~~~ 

Equipped with this predicate we can compute the duration of a month:

~~~~idris
> monthDuration : Month -> Year -> (days: Nat ** LTE 1 days) 
~~~~

Note the return type of `monthDuration` is a *dependent pair* associating the number of days of the month (a `Nat`) with a proof this number of days is always greater than 1. This will become important later on when we want to add days to dates...

Defining the function is a simple matter of case analysis over the constructors of `Month`:

~~~~idris
> monthDuration January _      = (31 ** LTESucc LTEZero)
> monthDuration February year  = if isLeapYear year 
>                                then (29  ** LTESucc LTEZero)
>                                else (28 ** LTESucc LTEZero)
> monthDuration March _        = (31 ** LTESucc LTEZero)
> monthDuration April _        = (30 ** LTESucc LTEZero)
> monthDuration May _          = (31 ** LTESucc LTEZero)
> monthDuration June _         = (30 ** LTESucc LTEZero)
> monthDuration July _         = (31 ** LTESucc LTEZero)
> monthDuration August _       = (31 ** LTESucc LTEZero)
> monthDuration September _    = (30 ** LTESucc LTEZero)
> monthDuration October _      = (31 ** LTESucc LTEZero)
> monthDuration November _     = (30 ** LTESucc LTEZero)
> monthDuration December _     = (31 ** LTESucc LTEZero)
~~~~

Because having to pattern match on pair is not very handy, let's provide some helpers functions to get only the relevant part of the pair:

~~~~idris
> daysInMonth : Month -> Year -> Nat
> daysInMonth month year with (monthDuration month year) 
>   | (days ** _) = days
> 
> aMonthHasOneDay : (month : Month) -> (year : Year) -> LTE 1 (daysInMonth month year)
> aMonthHasOneDay month year with (monthDuration month year) 
>   | (_ ** prf) = prf
~~~~

Had we defined a proper `Enum` implementation, we would be able to use `pred` and `succ` to navigate between months... But we only need at the moment to compute `nextMonth` which is definitely simple:

~~~~idris
> nextMonth : Month -> Month
> nextMonth January   = February
> nextMonth February  = March    
> nextMonth March     = April    
> nextMonth April     = May      
> nextMonth May       = June     
> nextMonth June      = July     
> nextMonth July      = August   
> nextMonth August    = September
> nextMonth September = October  
> nextMonth October   = November 
> nextMonth November  = December 
> nextMonth December  = January
~~~~

We are at last equipped to define a `Date` type that will prevent us to define invalid dates:

~~~~idris
> data Date : Type where
>   MkDate : (year  : Year) -> (month : Month ) -> (day : Nat) 
~~~~

So a `Date` is built from a `Year`, a `Month` and `Nat` for the `day`, with the additional constraints that `day` should be between 1 and the length of the `month`, a property which is easily defined as implicit dependent types:

~~~~idris
>          -> { auto dayFitInMonth : LTE day (daysInMonth month year) } 
>          -> { auto dayGreaterThanOne : LTE 1 day } 
~~~~

To build a `Date`, one will then need to provide *proofs* the `day` respects the given constraints.

~~~~idris
>          -> Date
~~~~

Here are some examples of buildiing valid and invalid dates at the REPL:

Let's try to build a date with 0 as day number:

    λΠ> :let d = MkDate 2017 February 0
    (input):1:17:When checking argument gtOne to constructor Date.MkDate:
            Can't find a value of type 
                    LTE 1 0

And what about the 29th of February, 2017 ?

    λΠ> :let d = MkDate 2017 February 29
    (input):1:17:When checking argument dayFitInMonth to constructor Date.MkDate:
            Can't find a value of type 
                    LTE 29 28

However, 29th of February, 2016 is fine:

    λΠ> :let d = MkDate 2016 February 29
    defined
    λΠ> d
    MkDate 2016 February 29 : Date

We can now tackle the problem which triggered that development: Adding some days to a `Date` to compute a new `Date`... The general definition follows the classical inductive principle over the number of days in order to guarantee totality of the function. 

~~~~idris
> mutual   
>   addDays : Date -> Nat -> Date
>   addDays d Z     = d
>   addDays d (S k) = addDays (addOneDay d) k
~~~~

The grunt of the job is delegated to the `addOneDay` function which is the most interesting one:

~~~~idris
>   addOneDay : Date -> Date
>   addOneDay (MkDate year month day) = 
~~~~ 

We do a case analysis to using the `order` function from the `Ordered` interface that, given a *total relation* (`LTE` in our case) and two elements states which one is greater than other, providing a proof packed in an `Either` union:

~~~~idris
>     case order {to=LTE} (S day) (daysInMonth month year) of
~~~~ 

The `Left` case is easy. Simply increment the number of days by one while keeping everything the same. The `dayFitInMonth` property is satisfied by the (anonymous) value provided in the `Either`:

~~~~idris
>       Left _  => 
>         MkDate year month (S day)
~~~~ 

The `Right` case (meaning the `day` is the last day of the month) requires another case analysis on the `month`:

~~~~idris
>       Right _ => 
>             case month of 
~~~~ 

If we are at the end of the year, increment `year` and set the day to 1st of January:

~~~~idris
>                     December => MkDate (year + 1) January 1
~~~~ 

Otherwise, move to first day of next `month`. The `firstDayOfMonth` variables provides the necessary proof that 1 is indeed within the limits of the allowed number of days for next month. This proof is provided by the `monthDuration` function and allows us to build the required `Date` in a typesafe way.

~~~~idris
>                     _        => let firstDayOfMonth = aMonthHasOneDay (nextMonth month) year
>                                 in MkDate year (nextMonth month) 1
~~~~ 

## Discussion

It took me a while and help from [Stack Overflow](http://stackoverflow.com/questions/43352519/how-to-properly-handle-fin-n-and-integer-when-computing-dates) and the `#idris` channel at `irc.freenode.net` to figure out how to implement `addDays`. One dead end was trying to use a `Fin n` as a representation for days, believing it would already provide some guarantees that would simplify definition of other types and functions. It proved a mistake for at least 2 reasons:

* The `Data.Fin` module provides a much poorer set of functions and properties than what `Nat` natively provides. In particular, it is not possible to do arithmetics with `Fin n` numbers nor compare them directly. This lead my code to be much more convoluted than what it is now, with a lot of to and fro conversions between `Nat`, `Integers` and `Fin n`,
* The representation is actually not congruent with the *domain* (days within a month): A `Fin n` is a natural number strictly lower than $n$ which means it has all numbers from 0 to $n-1$, so the type of `March` would be `Fin 31` and April `Fin 30` but the days would be numbered from 0 to 30 or 29, which is incorrect. 

Moving to representing days as a `Nat` with explicit constraints in the constructor's type for `Date` made everything simpler. 

I also struggled a lot with making `addDays` total: 

* Initially, I had only one function and did not use induction over number of days. This prevents the compiler from proving termination hence totality of the function as it is not able to infer that recursive calls to the function are made with a value which is *smaller* than initial call, something which is obvious with explicit pattern matching on the various constructors of the data type,
* I initially wrote the first test comparing the number of days to length of the month first as a simple if/then/else, then using a decidable property returning a `Dec (LTE a b)` which proved fruitless because we need 2 different proofs (and not a negation) for the 2 different cases, something which is provided by the (totally) `Ordered` interface. Once I had those 2 paths covered, moving forward was easier.

The last difficult piece, and something that remains a little bit puzzling to me, was the need for a proof that `LTE 1 (monthDuration $ nextMonth month)` that arose from the last branch of the cases. Apparently, an explicit proof is needed because Idris does not reduce the expression appearing in types *unless* the expression is used in a `with xxx | lhs = rhs ` view or is part of the definition of some value[^2]. This bit me also when defining `daysInMonth` and `aMonthHasOneDay`. 

Something that I tried to do but could not manage to complete quickly is defining the `LeapYear year` type to expose the leapyearness as a proposition that could then be carried around in types and would remove the need for explicit if/then/else. As a general rule of thumb and a piece of advice that was given to me on SO, it is a good idea to use functions and types that provide richer information than `Bool` or `Maybe a`: You usually want and need a proof that some property holds or does not hold to build richer and more accurate types.

[^1]: Note we are defining a simple version of dates within the Gregorian calendar. For a lot of ugly and painful details on how to properly handle dates, one can have a look at this [Ocaml library FAQ](https://forge.ocamlcore.org/scm/viewvc.php/trunk/calendarFAQ-2.6.txt?view%3Dmarkup&root%3Dcalendar)

[^2]: Idris is strict by default: Arguments to functions are evaluated before the right-hand side and I assume this also applies to `let x = y in z` blocks which are traditionally translated to `(\ x -> z) y`.
