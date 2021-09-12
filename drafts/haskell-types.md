------------
title: A Tale of Two Languages
subtitle: Making sense of Haskell (and Elm) Type System
author: Arnaud Bailly 
date: 2016-10-16
------------

This post was triggered by a short discussion in Elm's Slack channel on how difficult learning Haskell is when compared with other languages, especially Elm, and by numerous questions I have seen over the past few weeks from beginners regarding the meaning of expressions like `List Int` or `foo : Int -> String -> Int`. It is an attempt to explain how it works targeted at programmers who have never been exposed to formal type theory, built on my own fifteen years' old experience trying to understand Haskell's type system, especially as it manifests itself in the form of more or less cryptic compilation errors. While this post is mostly focused on Haskell, I think it will also help people who come to Elm as its type system is quite close to Haskell's and actually much simpler.

# What are types?

Types often appear as a given in programming languages, as a intrisic part of the language one has to learn to live with, and hopefully master one day, but more often than not the actual *motivation* for types is lacking. This seems by contrast a good way to make untyped (or dynamically typed) languages more attractive and more sensible to a lot of programmers: With dynamicity, types cease to be an annoyance while writing programs but become a part of the runtime system, hence of the world the program lives in and something one can understand, play with, eventually cheat with...

For want of the needed skill and knowledge, I won't go into the details of the history of *types*. Suffice it to say that types as we know them were invented by XXst century mathematicians, most notably Bertrand Russell, as a way to solve paradoxes in naive set theory. Types (or hierarchy of types) are a way to *talk about things*, to *classify* them formally from the *outside* of a formal system those things form, because trying to classify things from the *inside* leads to inconsistencies. Gödel's famous incompleteness theorems express this idea in a very precise way.

This gives some insight on how to understand types in the case of Haskell (and Elm): Types are actually a formal system, a *language*, that allows us (and the compiler) to classify programs and expressions. The programmer should then think of Haskell not as a single language, but as as system made of 2 related but different languages: The language of *values*, which is what we use to actually *do* things with Haskell, and the language of *types*, which is used to annotate, classify, describe, constrain values. The great thing about Haskell's type system is that the programmer can choose to let the latter stay implicit and within the confines of the typechecker's execution, focusing on the first, more operational, language of values. But she can also choose to use explicitly to annotate his programs in a way which will help the typechecker *and* more importantly the next programmer. The language of types can then be viewed as a *model* of the program and a great way to provide precise (formal) descriptions of its behavior: By using precise, unambiguous, expressive types the programmer documents its code in a better way.

# Properties of the Haskell type language

* Every expression has a type, or more precisely every expression *must* be assigned a single, unambiguous, most general type, to be considered valid by the typechecker. This is true for most constructs of a Haskell or Elm code fragment, except for type expressions themselves (but wait till we talk about *kinds*), import statements, modules declarations and of course comments,
* Types are often associated with the definition of data structures but this is not mandatory. Defining a new data structure always give rise to a new type but the converse is not true,
* A type $T$ is said to be *inhabited* if it is possible to define terms which can be typed with $T$. The word "inhabited" is used to emphasize types *are not* sets and the things they type are not members of a set. The conflation of types and data structures gives rise to this ambiguity between types and sets,
* Types in Haskell and Elm are *inferred* by the typechecker and not be explicitly written by the programmer, through a process we will describe later,
* However type-level expressions can be written explicitly to *annotate* values, to document the (design of) the code as well as sometimes to help the typechecker resolve potentially ambiguous situations,
* Even in this case, the typechecker will run and make sure annotations written explicitly by the programmer are *compatible* with the inferred type. There is no such thing as *cast* or *type coercion* in Haskell, at least not in the way you can do it in C or Java,
* Explicit type annotations are a great way to document code and its design, and write code in a TDD-like way,
* The Haskell type-system is part of the family of Damas-Hindley-Milner type systems, which itself rests upon Martin-Löf's work on intuitionistic typed logic and Girard's System-F. The definitive "practical" guide on type systems for programmers is Benjamin Pierce's  [Types and Applications to Programming Languages](http://www.cis.upenn.edu/~bcpierce/tapl/) or TAPL for short. It is a great (and heavy!) book that provides detailed proofs and algorithms for a wide variety of type systems.

## Typechecking Haskell code

For a formal exposition of typechecking in ML systems, please refer to TAPL. [Typing Haskell in Haskell](https://web.cecs.pdx.edu/~mpj/thih/thih.pdf) is a great paper that provides lots of insights on how typechecking works by going through a detailed implementation in Haskell. Simon Peyton-Jones' book [Implementation of Functional Programming Languages](http://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/) is another classic source on the same topic. I only want to try to give an actionable intuition of how typechecking works in order to help programmers understand typing errors and better use types. 

Typechecking is a recursive process that tries to assign a most general type to each and every fragment of a program, within a given *context*. During that process, the typechecker will assign types to expressions and subexpressions depending on their *structure*. Oftentime, the typechecker will need to *unify* two potentially conflicting assignments for a single construction. This *unification* process is similar to the [Robinson's unification algorithm](https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm) in logic programming and boils down to matching expression tree fragments. 

Informally it goes that way:

* Start from some context containing known assignments from *symbols* (names) to *types*. This context might be empty or, more often, prepopulated with basic definitions (e.g. called `Prelude` in Haskell) for common symbols,
* To typecheck any expression $E$ within a context $C$:
    * If $E$ is an immediate value, e.g. a constant like `1`, `"Foo"` or `True`, assign to it the appropriate type,
    * If $E$ is a function application, recursively typecheck both the function and its arguments, then *unify* the type of the function and the types of its arguments, creating new assignments if needed,
    * If $E$ is a symbol, extend $C$ to $C'$ by assigning a fresh *type variable* $t$ as the type of $E$: Without more information, this is the most general way we can type something, e.g. by saying $E$ can have *any* type,
    * If $E$ is a symbol whose type is already known (because it is a library function, or because the user assigned it explicitly a type through an annotation), extend $C$ to $C'$ by assigning some type $t$ as the type of $E$,
    * There are similar rules for all syntactic constructions of the language...
    * Recursively typecheck $E$'s subexpressions within the context $C'$ 
* If a non-conflicting assignment of symbols to types can be built, typechecking succeeds, otherwise it fails and provides a hopefully useful message on what error the programmer made.

One of the most interesting properties of the algorithm is that it is guaranteed to assign the *most general type* to any valid expression of the language, thus maximising reusability of defined expressions. When the user explicitly assign a type to an expression, the algorithm checks that this type can be unified with the inferred type: It is possible to assign a type only if it is *less general* than what the compiler infers. Contrary to what happens in other, more permissive languages like Java or C, there is no *cast* allowing you to pretend some expression has another type than what is inferred by the compiler. 

# The Haskell Types

## Literal Types

* The simplest types are the types of *atoms*, e.g. literal expressions of the language denoting immediate values:
    * `()` aka. *unit* is the smallest type, which contains only one inhabitant also named `()`,
    * Booleans have type `Bool` with 2 values, `True` and `False`,
    * Numbers can have various types depending on precision and internal representation `12.1 :: Double`, `3 :: Int`, `1231323123123213 :: Integer`,
    * Strings appear as a basic, primitive type `String` but actually are more complex.


## Constructing Types

* Types for literal are the basic building blocks of Haskell's type language, but of course there is a need to be able to build more complex types out of simpler ones. This is the role of **type constructors** which are functions (or operators) that can be used to build other types.
* *Tuples* are the types of values that are grouped together, also known as *product types* e.g. `(1, 2.0) :: (Int,Double)`, `(1,2,3) :: (Int,Int,Int)`. Note that *values* of tuple *types* are built using the same operator tupling symbols `(,)`, `(,,)` ... than the type itself, which might be confusing.
* *Functions* are of course one of the most ubiquitous types in a functional programming languages and they are types using the `->` operator:

    ~~~~~~~~~.haskell
    double :: Int -> Int
    double i = 2 * i
    ~~~~~~~~~~
    
* The `->` operator is a *type constructor* that defines the type of a function taking its input from some type `a`  and returning a value of some other type `b`. Because types can be built from other types, functions with more than one argument are actually built from functions with one argument: 

    ~~~~~~~~~.haskell
    multiply :: Int -> Int -> Int
    multiply x y = x * y
    ~~~~~~~~~

* The type of `multiply` above can be interpreted in 2 ways:
  * It is a function that takes 2 `Int` arguments and returns an `Int` 
  * It is a function that takes 1 `Int` argument and returns a function that takes 1 `Int` argument and returns an `Int`,
* This way of defining functions is known as *currying* and highlights the fact the type system is really a language very similar to other programming languages: Type expressions are built syntactically from basic building blocks and operators.

## User Defined Types

* User defined types are introduced with the `data` keyword (which is a bit confusing for beginners) and by providing *value constructors* 
  
      The expression
      
    ~~~~~~~~~.haskell
    data Foo = Bar Int Bool
    ~~~~~~~~~

    defines a type `Foo` and a (value) *constructor* `Bar`
* The constructor `Bar` is a *value-level* thing and declaring a constructor actually declares a function, the type of which depends on the constructor's arguments adn the type of value it constructs. In this case, the above code is equivalent to declaring:

    ~~~~~~~~~.haskell
    Bar :: Int -> Bool -> Foo
    ~~~~~~~~~~
        
    The compiler will provide the definition of the function, of course
* A powerful feature of Haskell's types is that one can provide *several*  ways of constructing values of the same type, thanks to *union types* (not to be confused with sum types that will be introduced later on):

    ~~~~~~~~~.haskell
    data Temperature = Farenheit Double | Celsius Double | Kelvin Double
    ~~~~~~~~~~
    
    Each member of the union defines a different function that can be used to construct values of the enclosing declaring type
* Records are not a type-level construction in Haskell but syntactic sugar over *product types*:

    This expression:
    
    ~~~~~~~~~.haskell
    data User = User { firstname :: String, lastname :: String }
    ~~~~~~~~~~

    yields 3 functions that are defined by the compiler:
    
    * `User :: String -> String -> User`
    * `firstname :: User -> String`
    * `lastname :: User -> String`
        
## Parametric Types

Types seen so far are called *ground types* as their inhabitants are defined as part of the declaration of the type itself: Everything there is to know about them is immediate and does not require further work from the compiler. If we draw a parallel with the world of values, we could say that such types are the *constants* which form the building blocks of the types language. But the real power of HML type system comes from the fact types can actually be constructed through *functions* or *type constructors*: These are also called parametric types,  rank-1 types  or polymorphic types.

* A simple polymorphic type `Message` that pairs an arbitrary `payload` along with a timestamp can be defined as:

    ~~~~~~~~~.haskell
    data Message a = Message { date :: Date, payload :: a }
    ~~~~~~~~~~

  This declares `Message` as a *type constructor* or a *type level* function, that needs to be applied a *type* to yield another *type*, e.g.

    ~~~~~~~~~.haskell
    message :: Message Double
    message = Message 2016-09-10 23.4
    ~~~~~~~~~
    
* type synonyms allow defining *aliases* of other types, but they do not define new types:

> type Value = Message Double

* recursive use of type: A declared type can be used immediately within its constructors

> data Stream a = Cons a (Stream a)

* newtypes, like type synonyms on steroids: declares a new type but no overhead at runtime

> newtype Password = Password { password :: ByteString }

* constraints:

> sort :: (Ord a) => [a] -> [a]

* type classes: interfaces with default implementations

> class 

* Multi-parameter type classes: describe some relationship between two types

> class Nat f g where
>   nat :: f a -> g a

# Some common type errors and their meaning

* skolem, rigid type variables -> type variable cannot be unified because there is not enough information

# Type-level Literals

```.haskell
import           Data.Proxy
import           GHC.TypeLits

-- | Type list membership test.
type family Find (x :: k) (ys :: [k]) :: Nat where
  Find x ys      = Find' x ys 0

type family Find' (x :: k) (ys :: [k]) (n :: Nat) :: Nat where
  Find' x (x  ': xs) n = n
  Find' x (x' ': xs) n = Find' x xs (n + 1)

getProxy :: (n ~ Find x xs) => Proxy x -> Proxy xs -> Proxy n
getProxy _ _ = Proxy

find :: (Find x xs ~ n, KnownNat n) => Proxy x -> Proxy xs -> Integer
find p p' = natVal (getProxy p p')

-- >>> find (Proxy :: Proxy Bool) (Proxy :: Proxy '[Int, Bool])
-- 1
--
-- >>>  find (Proxy :: Proxy Bool) (Proxy :: Proxy '[])
-- 
-- <interactive>:89:1: error:
--     • No instance for (GHC.TypeLits.KnownNat
--                          (Find' Bool '[] 0))
--         arising from a use of ‘find’
--     • In the expression:
--         find (Proxy :: Proxy Bool) (Proxy :: Proxy '[])
--       In an equation for ‘it’:
--           it = find (Proxy :: Proxy Bool) (Proxy :: Proxy '[])
```
