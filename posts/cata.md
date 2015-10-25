------------
title: On Free DSLs and Cofree interpreters
author: Arnaud Bailly 
date: 2015-06-04
------------

This post has been triggered by a [tweet](https://twitter.com/etorreborre/status/605562458279944192) from Eric Torreborre on a talk
by David Laing presenting the interaction of Free DSLs and Cofree interpreters at the Brisbane Functional Programming Group. I am
currently engaged in the development of a Haskell-based system for [Capital Match](http://www.capital-match.com) which is basically
an API for managing peer-to-peer lending, and I am trying to formalise the API of the system as the result of a composition of
several domain-specific languages.

The ultimate goal is to be able to use these DSLs to define complex actions that could be interpreted in various ways: a
command-line client sending RESTful queries to a server, a Webdriver-based test executor or a simple test recorder and comparator,
or even by a core engine interpreting complex actions in terms of simpler sequencing of service calls.

The rest of the post is a simple literate Haskell style explanation of what I came up with today exploring the specific topic of the
composition of DSLs and interpreters: Given we can compose DSLs using *Free* monads and *Coproduct*, how can we *Pair* a composite
DSL to the composition of several interpreters? The answer, as often, lies in the category theoretic principle for duality: *Reverse
the arrows!* One composes interpreters into a *Product* type which is then lifted to a *Cofree* comonad paired to a *Free Coproduct* monad. 

This post has no original idea and is just rephrasing and reshaping of work done by more brilliant people than me:

* Dan Piponi's [Cofree meets free](http://blog.sigfpe.com/2014/05/cofree-meets-free.html) blog post
* This [thread on Stack overflow](http://programmers.stackexchange.com/questions/242795/what-is-the-free-monad-interpreter-pattern)
  about free monads
* Runar Bjarnason talk on [Reasonably Priced Monads](https://dl.dropboxusercontent.com/u/4588997/ReasonablyPriced.pdf)
* An [Haskell implementation](https://gist.github.com/aaronlevin/87465696ba6c554bc72b#file-reasonable-hs) of the above by Aaron
  Levin
* [Comonads are objects](http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html) by Gabriel Gonzalez
* [Data types à la carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf) by Wouter Swiestra

I would not dare to say I really *understand* all of this, but at least I got some code to compile and I have some ideas on how to
turn this into a useful "pattern" in our codebase. 

# Free Coproduct DSLs

So let's start with some usual declaration and imports...

~~~~~~~~~ {.haskell .numberLines}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Capital.Client.Free  where

import           Control.Applicative
import           Control.Comonad.Cofree
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Identity
import           Control.Monad.Trans    (MonadIO, liftIO)
~~~~~~~~~~

This relies on the [free](https://hackage.haskell.org/package/free) package which defines standard *free* Constructions for
`Applicative` and `Monad`, and *cofree*  for `Comonads`.

We define our basic business-domain specific functors, one for logging some messages and another for persisting some string
value. The actual functors defined are not important, what interests us here is the fact we define those "actions" independently but
we want in the end to be able to "Assemble" them yielding more complex actions which can at the same time log messages and persist
things. 

~~~~~~~~~ {.haskell .numberLines}
data Logging a = Logging String a  deriving (Functor)

data Persist a = Store String a deriving Functor
~~~~~~~~~~

Our composite DSL should be able to interpret actions which are either logging actions, or persist actions, so we need a way to
express this alternative at the type-level, introducing the notion of *Coproduct* or *Sum*. This work has already been packaged by
Ed Kmett in the [comonads-transformers](https://hackage.haskell.org/package/comonad-transformers-2.0.3) package but let's rewrite it
here for completeness' sake.

~~~~~~~~~ {.haskell .numberLines}
newtype Coproduct f g a = Coproduct { getCoproduct :: Either (f a) (g a) }
~~~~~~~~~

A `Coproduct` of two functors is then simply the type-level equivalent of the familiar `Either` type, for which we provide smart
constructors to inject values from left or right and a suitable `Functor` instance.

~~~~~~~~~ {.haskell .numberLines}
left :: f a -> Coproduct f g a
left = Coproduct . Left

right :: g a -> Coproduct f g a
right = Coproduct . Right

coproduct :: (f a -> b) -> (g a -> b) -> Coproduct f g a -> b
coproduct f g = either f g . getCoproduct

instance (Functor f, Functor g) => Functor (Coproduct f g) where
  fmap f = Coproduct . coproduct (Left . fmap f) (Right . fmap f)
~~~~~~~~~

We want to be able to implicitly "lift" values from a component into its composite without resorting to explicit packing of the
various parts of the alternative formed by a `Coproduct` type, something which would be extremely cumbersome to express, hence the
introduction of a *natural transformation* `Inject` expressed in Haskell as a typeclass.

~~~~~~~~~ {.haskell .numberLines}
class (Functor f, Functor g) => f :<: g where
  inject :: f a -> g a
~~~~~~~~~

To be useful we provide several interesting instances of this typeclass that defines how to inject functors into a `Coproduct`. Note
that this requires the `OverlappingInstances` extension otherwise the compiler[^1] will refuse to compile our programs. I think this
stuff could be expressed as *type families* but did not manage to get it right, so I gave up and resorted to original formulation by
Wouter Swiestra.

~~~~~~~~~ {.haskell .numberLines}
instance (Functor f, Functor g) => f :<: Coproduct f g where
  inject = left

instance (Functor f, Functor g, Functor h, g :<: h) => g :<: Coproduct f h where
  inject = right . inject

instance (Functor f) => f :<: f where
  inject = id
~~~~~~~~~

Finally, we provide "smart constructors" that generates `Free` monadic expressions out of the individual instructions of our two
tiny DSLs. We use a `inFree` function combining lifting into `Free` monad and possible transformation between functors so that each
expressed action is a `Free` instance whose functor is polymorphic. This is important as this is what will allow us to combine
arbitrarily our DSL fragments into a bigger DSL.

~~~~~~~~~ {.haskell .numberLines}
inFree :: (Functor f, f :<: g) => f a -> Free g a
inFree = hoistFree inject . liftF

log :: (Logging :<: f) => String -> Free f ()
log msg = inFree (Logging msg ())

store :: (Persist :<: f) => String -> Free f ()
store s = inFree (Store s ())
~~~~~~~~~

Equipped with all this machinery we are ready to write our first simple program in a combined DSL:

~~~~~~~~~ {.haskell .numberLines}
type Effect = Coproduct Logging Persist

prg :: Free Effect ()
prg = store "bar" >> log "foo"
~~~~~~~~~

# Cofree Product Interpreters

We are now done with the DSL part, let's turn to the interpreter part. First we need some atomic interpreters which should be able
to interpret commands from each of our DSL. We will prefix these functors with `Co` to demote the relationship they have with the
DSL functors. Something which is not obvious here (because our DSL functors only have a single constructor) is that these
interpreters should have a dual structure to the DSL functors: Given a DSL expressed as a sum of constructors, we need an
interpreter with a product of intepretation functions. The DSL presented in David's post are more expressive...

~~~~~~~~~ {.haskell .numberLines}
data CoLogging a = CoLogging { cLog :: String -> a }  deriving Functor

data CoPersist a = CoPersist { cStore :: String -> a }  deriving Functor
~~~~~~~~~

Of course we need concrete interpretation functions, here some simple actions that print stuff to stdout, running in `IO`.

~~~~~~~~~ {.haskell .numberLines}
coLog :: (MonadIO m) => m () -> String -> m ()
coLog a s = a >> (liftIO $ print s)

coStore :: (MonadIO m) => m () -> String -> m ()
coStore a s = a >> (liftIO . print . ("storing " ++)) s
~~~~~~~~~

To be able to compose these interpreters we need a `Product` type whose definition is straightforward:


~~~~~~~~~ {.haskell .numberLines}
newtype Product f g a = Product { getProduct :: (f a, g a) }

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap f (Product (a,b)) = Product (fmap f a, fmap f b)
~~~~~~~~~

class (Functor f, Functor g) => Pairing f g where
  pair :: (a -> b -> r) -> f a -> g b -> r

instance Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

instance Pairing ((->) a) ((,) a) where
  pair p f = uncurry (p . f)

instance Pairing ((,) a) ((->) a) where
  pair p f g = pair (flip p) g f

instance Pairing f g => Pairing (Cofree f) (Free g) where
  pair p (a :< _ ) (Pure x)  = p a x
  pair p (_ :< fs) (Free gs) = pair (pair p) fs gs

instance Pairing CoLogging Logging where
  pair f (CoLogging l) (Logging m k) = f (l m) k

instance Pairing CoPersist Persist where
  pair f (CoPersist s) (Store v k) = f (s v) k

instance (Pairing g f, Pairing k h) => Pairing (Product g k) (Coproduct f h) where
  pair p (Product (g,_))  (Coproduct (Left f)) = pair p g f
  pair p (Product (_,k)) (Coproduct (Right h)) = pair p k h

type Effect = Coproduct Logging Persist

type Interp = Product CoLogging CoPersist

interpretEffect :: Cofree Interp (IO ())
interpretEffect = interpretM f (return ())
  where
     f a = Product (CoLogging $ coLog a, CoPersist $ coStore a)

~~~~~~~~~

Un entier est ici construit à l'aide de la méthode `succ` et de la constante `Zero`:

- soit 0 ;
- soit le successeur d'un autre entier.
 
La question que l'on peut se poser, si l'on s'intéresse à ce genre de choses, c'est : comment peut-on caractériser algébriquement
l'ensemble des entiers ainsi défini, si ce n'est de manière tautologique ? Il peut nous apparaître très naturel d'utiliser un type
dans sa propre définition mais c'est parce que nous sommes habitués à raisonner récursivement.  

### Point Fixe

Pour répondre à la question posée, on peut reformuler de manière "compacte" le problème en cherchant à définir le type `Natural`
comme la solution d'une équation algébrique (+ joue ici le rôle de `OU`):
```
Natural = Zero  + Succ (Natural),
``` 
équation dont la solution est problématique puisque la variable apparaît des deux côtés de l'équation !

Si l'on substitue naïvement la définition de `Natural` en partie droite, alors on obtient quelque chose comme
```
Natural = Zero + Succ (Zero + Succ ( Zero + Succ (Zero +...
```
ce qui peut se réécrire en
```
Natural = Zero + Succ (Zero) + Succ (Succ (Zero)) +...
```
`Natural` apparait bien comme un ensemble infini d'éléments qui sont soit `Zero` soit de la forme $Succ^n Zero$ pour tout $n$
entier.

Du point de vue mathématique, la solution d'une équtation de la forme $x = f(x)$ est appelée un *point fixe*, ce qui est bien la
forme de l'équation de Natural. On peut donc dire que `Natural` est le point fixe de l'équation $X = Zero + Succ (X)$. Nous disons
*le* point fixe, mais ce n'est pas tout à fait exact : comme on ne considére que des nombres finis (même si l'ensemble lui-même
est de taille infinie), il s'agit là du *plus petit point fixe*. Il existe en effet des ensembles qui sont des points fixes de
cette équation mais dont la cardinalité est plus grande que N car ils contiennent des nombres infinis (en quantité infinie...).


Ce type de définition étant très courant, il a paru utile de généraliser cette notion de *plus petit point fixe*, d'où
l'introduction l'opérateur *μ*. Pour toute fonction f, μf est le plus petit point fixe de f, plus formellement: 
$$
  μf = x \in dom(f),  
  x = f(x) et
  \forall x' \in dom(f), x'= f(x') \Rightarrow x' \geq x
$$

Or ici la définition de Natural ne semble pas être une fonction. En fait, pour qu'une définition de type soit une fonction, il
faut qu'elle soit une fonction sur des types, prenant en argument des types et retournant des types, en d'autres termes un
foncteur. Mais c'est exactement ce que dit la forme $Zero + Succ (X)$ où X désigne un type quelconque, et donc on peut
légitimement définir `Natural  = μ(Zero + Succ(x))` comme un ensemble d'éléments point fixe d'un foncteur. 

### Définition explicite

Toute cette mécanique est rendu implicite dans tous les langages, même les plus plus sophistiqués comme Haskell, Scala ou
Caml. Pour définir un type de données récursif, nul besoin d'utiliser l'opérateur μ, on se contente d'utiliser les possibilités
syntaxiques du langage qui autorise l'utilisation du nom d'un type dans sa définition. Mais pour pouvoir généraliser les
mécanismes de récursions sous forme de FOS, il est nécessaire de déconstruire cette vision et d'introduire explicitement la
récursion. 

C'est ce que l'on va faire, en Haskell tout d'abord. 

On introduit d'abord l'opérateur `Mu` comme un nouveau type de données prenant en paramètre un foncteur `f`. `Mu` a un seul
constructeur, `In` qui empaquette le foncteur `f` dans une boucle récursive, ce qui nous donne 2 fonctions permettant de naviguer
dans la "pile" de récursion: 

 - `In : f (Mu f) -> Mu f` (le constructeur, vu comme une fonction), 
 - `out : Mu f -> f (Mu f)` (l'accesseur de l'unique champ de la structure encapsulée par In).

~~~~~~~~~ {.haskell}
-- newtypes in Haskell are cheaps, they do not add any runtime overhead and serve
-- only for the compiler to distinguish types
newtype Mu f = In { out :: (f (Mu f)) }
~~~~~~~~~

Essayons maintenant de définir les entiers comme ci-dessus au moyen de `Mu` en évitant la récursion explicite et en définissant `Natural` comme un foncteur:

~~~~~~~~~ {.haskell}
-- le foncteur engendrant les entiers naturels
data Natf x = Zero  | Succ x 

-- le type (un simple alias) Natural comme point fixe d'un foncteur
type Natural = Mu Natf
~~~~~~~~~

Voici quelques objets de type `Natural` que l'on peut construire en utilisant directement les constructeurs de `Natf` sans se préoccuper de `Mu` pour l'instant:

```
*Main> let zero = Zero
*Main> let un = Succ Zero
*Main> :t un
un :: Natf (Natf x)
*Main> let deux = Succ un
*Main> :t deux
deux :: Natf (Natf (Natf x))
```

On peut constater que chaque "nombre" a un type différent, ce qui n'est pas très pratique. En utilisan Mu, on uniformise le type
d'où la naissance de Natural, un ensemble contenant des objets de type homogène: 

```
*Main> let zero = In Zero
*Main> :t zero
zero :: Mu Natf
*Main> let un = In (Succ zero)
*Main> :t un
un :: Mu Natf
*Main> let deux = In (Succ un)
*Main> :t deux
deux :: Mu Natf
```

Tous les nombres ont bien ici le type `Mu Natf` et l'on peut sans problème les combiner, par exemple pour définir l'addition:

~~~~~~~~~ {.haskell}
add :: Natural -> Natural -> Natural
add (In Zero) x = x
add x (In Zero) = x
add (In (Succ x)) (In (Succ x')) = In (Succ (In (Succ (add x x'))))
~~~~~~~~~

### Foncteur et F-Algèbre

Evidemment, c'est théoriquement très intéressant mais ce qu'on veut c'est manipuler des "vrais" nombres, pas de longues chaînes de
constructeurs, sauf dans les cas où l'on s'intéresse à la récursion explicite, évidemment. On voudrait donc pouvoir *transformer*
des objets de notre type Natural en un type plus commun, par exemple Int. Pour ce faire, notre type de base Natf manque d'un
ingrédient: la _fonctorialité_ (ou propriété d'être un foncteur). On a vu que ce qui définissait un foncteur, c'était le fait de
posséder une fonction `fmap` possédant quelques bonnes propriétés de compositionnalité. Dans le cas de Natf, cette définition est
simple: 

~~~~~~~~~ {.haskell}
instance Functor Natf where
  fmap f (Zero) = Zero
  fmap f (Succ x) = Succ (f x)
~~~~~~~~~

Dès que l'on a un foncteur `f`, alors pour tout type `a` on peut définir (entre autres myriades de choses) des fonctions de types
`h :: f a -> a` qui "déconstruisent" des éléments de `a` "transformés" par `f` en éléments de `a`: c'est comme si on enlevait une
couche d'une pelure d'oignon. Ce type de fonction est suffisamment courant pour avoir été nommé, on les appelle des
*f-algèbres*. Par exemple, on peut écrire une f-algèbre qui permet de transformer des objets de type `Natf Int` en objets de type
`Int` (nos gentils entiers habituels): 

~~~~~~~~~ {.haskell}
intalgebra :: Natf Int -> Int
intalgebra Zero     = 0
intalgebra (Succ x) = 1 + x
~~~~~~~~~

Cette fonction est très simple et non récursive, elle décrit simplement une correspondance univoque entre des opérations du type
de départ (les constructeurs de `Natf`) et des opérations du type d'arrivée (les fonctions `plus` et la constante `0`). Ce serait
encore plus explicite si l'on pouvait écrire ceci: 

~~~~~~~~~ {.haskell}
-- does not compile
intalgebra :: Natf Int -> Int
intalgebra Zero = 0
intalgebra Succ = (1+)
~~~~~~~~~

Mais une fois que l'on a cette fonction, on n'est guère avancé car de toute évidence, elle ne peut s'appliquer aux nombres de type
`Natural`. C'est ici qu'entre un jeu notre premier "récurseur" d'ordre supérieur: le **catamorphisme** (roulement de tambour) ! 

## Catamorphismes

Un *catamorphisme* est donc une _fonction d'ordre supérieure_ permettant de produire une valeur d'un type arbitraire en "repliant"
une structure, un type algébrique, récursivement, par application d'un opérateur quelconque sur une valeur initiale.  

Le catamorphisme "canonique" est l'opérateur `foldr` sur les listes:

~~~~~~~~~ {.haskell}
foldr :: (a -> b ->  b) -> b -> [a] -> b
foldr op x []     = x
foldr op x (y:ys) = y `op` (foldr op x ys)
~~~~~~~~~

Pour tout opérateur binaire ⊙ et toute valeur x, h = foldr ⊙ x, est un catamorphisme pour les listes de type `[a] -> b`. Le
parcours de la liste est imbriqué avec l'application de l'opérateur dans l'appel récursif à `foldr`. Par ailleurs, on a vu
ci-dessus que la récursion pouvait être rendue explicite au travers de la structure du type de données, par l'opérateur `Mu`, qui
produit un _point fixe_ d'un foncteur quelconque. On aimerait donc pouvoir distinguer, séparer, dans foldr et d'autres opérations
du même type qui transforment un type de données récursif en une valeur quleconque, deux entités distinctes:

* le traitement de chaque instance possible d'un foncteur, autrement dit une f-algèbre quelconque ;
* et la récursion. 

Ces deux contraintes peuvent s'exprimer dans le système de type, ce qui nous donne la signature suivante pour `cata`:

~~~~~~~~~ {.haskell}
cata :: Functor f => (f a -> a) -> (Mu f -> a)
~~~~~~~~~

`cata` est donc une fonction qui, à partir d'une f-algèbre, produit une fonction transformation un point fixe du foncteur `f` en
une valeur. Sa définition est la suivante et l'on voit bien que la récursion y est explicite: 
 
~~~~~~~~~ {.haskell}
cata h = h . fmap (cata h) . out
~~~~~~~~~

On est désormais équipé pour appliquer notre fonction `intalgebra` définie ci-dessus pour transformer les nombres algébriques en
entiers "sympathiques": 

~~~~~~~~~ {.haskell}
toInt :: Natural -> Int 
toInt = cata intalgebra
~~~~~~~~~

et l'on peut utiliser `toint` pour obtenir de "vrais" entiers:

```
*Main> toint (In Zero)
0
*Main> toint (In (Succ (In (Succ (In Zero)))))
2
*Main> 
```

[^1]: GHC 7.8.3 in our case
