---
title: Le XXIème sera typé ou ne sera pas
subtitle: ou Comment j'ai appris à aimer les types
author: Arnaud Bailly - @dr_c0d3
date: 2019-09-24
theme: serif-compact
transition: none
---

# Plan #

------

* Introduction
* Motivation
* Expédition
* Conclusion

# Introduction #

## Pourquoi ?

* Discussions sur le canal `#beginners` dans le slack `elm-lang`: Beaucoup du questions sur le type de `List a`...
* [Making Impossible States Impossible](https://www.youtube.com/watch?v=IcgmSRJHu_8)
* Lien entre types et formalisation de *domaines métiers*

## Objectif

* Faire découvrir la richesse des systèmes de types des langages fonctionnels
* Comprendre comment utiliser ces systèmes pour des programmes "normaux"
* Comprendre en quoi un système de types riche peut permettre de mieux concevoir des programmes

# Motivation #

# Expédition

----

## Fonctions pures

![](/images/picasso-demoiselles.jpg)

<div class="notes">

* Des *fonctions* sans effets de bord
* De préférences totales...
* De première classe dans le langage...
* Qui sont interprétées par le *runtime* qui encapsule l'ensemble des effets

</div>

## Fonctions pures

```idris
selectEntries : (Account -> Bool) -> Vect k Entries -> Balance

isLeapYear : Year -> Bool

toDate : String -> String -> String -> Either String Date
```

## Types de données algébriques

![](/images/mondrian-composition.png)


<div class="notes">

* Décrit les données comme engendrées par une structure *algébrique*
* Types construits par *composition* au moyen d'opérateurs *union* et *produit*
* Un type est une *fonction* dans le domaine des types, un *constructeur de types*
* Permet au compilateur de "garantir" la totalité d'une fonction

</div>

## Types de données algébriques

```idris
data Balance : Type where
  Zero : Balance

  Bal : (n : Amount) -> (d : Direction) -> Balance
```

## Types de données algébriques récursifs

```haskell
data List a = Nil | Cons a (List a)
```

## Interfaces

![](/images/pollock-untitled.png)

<div class="notes">
* Interface offerte par un type de données
* Permet d'encapsuler l'implémentation concrète
* Peut exprimer des relations complexes entre plusieurs types de données
* Plus que des interfaces à la java
</div>

## Interfaces

```idris
interface ToSExp a where
  toSExp : a -> SExp

ToSExp SExp where
  toSExp = id

ToSExp Unit where
  toSExp () = SList []

```

## Familles de types

![](/images/bacon-triptyque.jpg)

<div class="notes">
* Relation entre types
* Rend le domaine d'une fonction dépendant du codomaine
* Peut être *ouvert* (relation extensible) ou *fermé* (le codomaine est fini)
</div>

-----

```haskell

type family Answer q = a | a -> q where
  Answer QCM          = Int
  Answer Grade        = Double
  Answer OpenQuestion = Text
```


<div class="notes">
En Haskell, les familles des types sont une construction spéciales permettant de
définir des relations entre types
</div>

-----

```idris
Factors : UnitType -> Type
Factors Armored       = StdFactors
Factors Artillery     = Arty
Factors AntiTank      = Pak
Factors HQ            = Arty
Factors SupplyColumn  = Unit
```

<div class="notes">
* En Idris, les fonctions peuvent prendre des types et en retourner!
* la frontière entre types et valeurs est perméable, au moins syntaxiquement
</div>


## Types existentiels

![](/images/magritte-pipe.jpg)

<div class="notes">
* Encapsuler un ensemble de types dans un autre type de données
* Renforce la distinction entre type abstrait et concret en rendant l'implémentation inaccessible
* Garantit la *localité* des opérations sur un type: la variable de type est nécessairement locale
</div>

## Types existentiels

```haskell
data CmdREPL : (segment : GameSegment) -> Type where
  Cmd : (cmd : Command segment) -> CmdREPL segment
  Qry : (ToSExp res) => (qry : Query res) -> CmdREPL segment

```

<div class="notes">
On cache le type exact de `res` en n'exposant que son interface (`ToSExp`)
</div>

-----

```idris
data BookOfAccounts : Type where
  BookTransactions : (txs : Vect k Transaction) ->
                     BookOfAccounts
```

<div class="notes">
On cache le paramètre de type `k` qui indique la longueur du vecteur ce qui le rend inaccessible de "l'extérieur"
</div>

## GADTs

![](/images/kandinsky-comp-8.png)

<div class="notes">
* Applicable aux types paramétrés
* Chaque constructeur d'un type peut retourner une type différent
* Permet de spécialiser le paramètre en fonction du contexte dans lequel le type est appelé à s'insérer
</div>

## GADTs

```idris
data Query : (result : Type) -> Type where
  SupplyPath : (unitName : String)
             -> Query (Either QueryError (List Pos))

  TerrainMap : Query Map
```
<div class="notes">
En pattern-matchant sur le constructeur, on _sait_ quel est le type associé `result` ce qui permet de traiter ce résultat
</div>


## Hole-Driven Development

![](/images/duchamp-pissotière.png)

<div class="notes">
* Utiliser l'inférence de type pour compléter le code par la structure attendue par le compilateur
* Permet de s'assurer dés le départ qu'une fonction est totale ou a minima que tous les cas en input sont couverts
* Permet à l'éditeur de compléter automatiquement le code
* Guide l'implémentation et parfois permet de la déduire automatiquement
</div>

## Hole-Driven Development

```idris
Eq Balance where
  (==) b b' = ?hole

```

<div class="notes">
Pour définir l'égalité entre 2 `Balance` on part d'une équation minimale
</div>

----

```idris
- + Accounting.Core.hole [P]
 `--                     b : Balance
                        b' : Balance
     --------------------------------
      Accounting.Core.hole : Bool
```

<div class="notes">
* Au chargement le compilateur infère le type du "trou" en fonction du contexte dans
lequel il s'insère

* On peut ensuite remplir le trou petit à petit avec les différents cas
</div>

## Types dépendants

![](/images/calle-sleepers.jpg)

<div class="notes">
* Faire tomber la barrière entre les *types* et les *valeurs*
* Un type peut être définit par une fonction *dépendant*  de la valeur d'un paramètre
* Introduit une hiérarchie potentiellement infinie de types (quel est le type de `Type` ?)
* Le *vérificateur de types* utilise la même sémantique que le *runtime*
</div>

## Types dépendants

```idris
record GameUnit where
  constructor MkGameUnit
  nation : Nation
  unitType : UnitType
  name : String
  move : Nat
  hit : Bool
  combat : Factors unitType
```

<div class="notes">
Le type de `combat` est une _fonction_ de la valeur de `unitType`
</div>

## Type égalité

![](/images/klein-blue.png)

<div class="notes">
* Exprimer l'égalité entre deux _types_ comme un _type_
* Le type devient une *assertion* logique, une *proposition* qui doit être vérifiée par l'implémentation
* La seule valeur qui *habite* ce type est `Refl` qui est donc une *preuve* que l'égalité est vérifiée
</div>

## Type égalité

```idris
neighbours1_test : (neighbours (Hex 3 3) = [ Hex 2 3, Hex 3 2
                                           , Hex 4 3, Hex 4 4
                                           , Hex 3 4, Hex 2 4] )
neighbours1_test = Refl
```

<div class="notes">
* les types égalité peuvent servir à définir des "tests" exécutés par le compilateur
* en combinaison avec les "Trous", cela permet de faire du TDD dans le système de types!
</div>

## Type égalité

```idris
data Entries : Type where
  MkEntries : (entries : Vect n Entry) ->
              { auto need2Entries : LTE 2 n } ->
              { auto balanced : balance entries = Zero } -> Entries
```

<div class="notes">
* ils servent surtout à exprimer des propriétés, des _assertions_ qui sont vérifiées par le
compilateur
</div>

## Types = Proposition

![](/images/malevitch-white-on-white.png)

<div class="notes">
* Mise en oeuvre concrète de Curry-Howard
* Les types sont des *propositions*, les programmes des *preuves*
* Écrire un programme c'est démontrer que les types sont *habités*, que l'on peut effectivement construire des valeurs des types donnés
* De la conception de logiciels considérée comme l'énonciation de théorèmes...
</div>

## Types = Proposition

```idris
notPosZIsNotAbsZ : ((y = Pos 0) -> Void) -> ((absZ y = 0) -> Void)
notPosZIsNotAbsZ = contrapositive absZIsPosZ
```

<div class="notes">
Proof that if a number is not 0, its absolute value is not 0
This is the contrapositive proof to `absZIsPosZ`
@y: a Relative number which is not 0
</div>


## Types = Proposition


```idris
contrapositive : (a -> b) -> (Not b -> Not a)
contrapositive = flip (.)
```

<div class="notes">
Exprime la _contraposée_ d'une proposition: Si a implique b, alors non b implique non a
</div>

## Types = Proposition

```idris
absZIsPosZ : (absZ y = 0) -> (y = Pos 0)
absZIsPosZ {y = (Pos Z)}     Refl = Refl
absZIsPosZ {y = (Pos (S _))} Refl impossible
absZIsPosZ {y = (NegS _)}    Refl impossible
```

<div class="notes">
Proof that if the absolute value of a number is 0, then this number is 0
@y: a relative number which we take absolute value of
</div>

# Conclusion

----

![](/images/braque-gueridon.jpg)

## Conclusion

* Le système de type est un outil essentiel dans la *conception* du logiciel
* Les systèmes de types fonctionnels sont de plus en plus sophistiqués et permettent de modéliser des contraintes de plus en plus proches du métier
* Le XXIème sera typé !

## Pub

* 2 jours de formation **gratuite** sur l'_Architecture Fonctionnelle_
* S’approprier les fondamentaux de l’architecture fonctionnelle et apprendre à les mettre en pratique dans la conception d’un logiciel.
* Paris (lieu exact TBD), les 14 et 15 novembre 2019
* Un partenariat Aleryo / Palo-IT
* Contact: <a href="mailto:arnaud@aleryo.com">arnaud@aleryo.com</a>

# Colophon

## (Quelques) Références

* [Thinking With Types](https://leanpub.com/thinking-with-types), S.Maguire
* [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/), B.Pierce
* Advanced Types and Programming Languages, B.Pierce
* [Typing Haskell in Haskell](https://web.cecs.pdx.edu/~mpj/thih/thih.pdf)
* [Understanding types](http://lucacardelli.name/Papers/OnUnderstanding.A4.pdf) par Luca Cardelli, dans un contexte OO avec sous-typage
* [Familles de types en Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/type-families-and-pokemon)

## (Quelques) Références

* [Type-Driven Development with Idris](https://www.manning.com/books/type-driven-development-with-idris), E. Brady
* [Fun with Type Functions](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/typefun.pdf?from=http%3A%2F%2Fresearch.microsoft.com%2F%7Esimonpj%2Fpapers%2Fassoc-types%2Ffun-with-type-funs%2Ftypefun.pdf)
* [Types + Properties = Software - Mark Seemann on Vimeo](https://vimeo.com/162036084)
* [idris-tutorial.pdf](https://www.cs.ox.ac.uk/projects/utgp/school/idris-tutorial.pdf)
* [Type Theory and Functional Programming](https://www.cs.kent.ac.uk/people/staff/sjt/TTFP/ttfp.pdf)
* [Brutal {Meta}Introduction to Dependent Types in Agda](http://oxij.org/note/BrutalDepTypes/)

## Credits

* [Guéridon](http://www.georgesbraque.org/images/gallery/gueridon.jpg), G.Braque, 1913
* [Les demoiselles d'Avignon](https://c1.staticflickr.com/2/1305/563354141_dea564001f_b.jpg), P.Picasso, 1907
* [Composition II en rouge, bleu et jaune](https://en.wikipedia.org/wiki/Piet_Mondrian#/media/File:Piet_Mondriaan,_1930_-_Mondrian_Composition_II_in_Red,_Blue,_and_Yellow.jpg), P.Mondrian, 1930
* [Fontaine](https://d32dm0rphc51dk.cloudfront.net/ocgEy0HxvjqHsFXJO20PIw/larger.jpg), M.Duchamp, 1917
* [Second Version of Triptych 1944](https://static01.nyt.com/images/2015/12/04/arts/04BACON1/04BACON1-superJumbo-v2.jpg), F.Bacon, 1988
* [IKB 290](http://galeriegradiva.com/wp-content/uploads/2014/11/KLEIN_1.jpg), Y.Klein, 1959

## Credits

* [Orange et jaune](https://claudiabrigato.files.wordpress.com/2014/11/rothko-red.jpg), M.Rothko, 1956
* [Dormeurs](https://www.perrotin.com/images/2017/02/17/sophie_calle_the-sleepers_w1200xh630__003615.jpg), S.Calle, 1979
* [Ensemble](http://theforumist.com/wp-content/uploads/2016/12/5-Louise_Bourgeois_Together_20051.jpg), L.Bourgeois, 2005
* [Untitled](http://www.metmuseum.org/toah/works-of-art/1982.147.27/), J.Pollock, 1948
* [Composition VIII](https://www.ibiblio.org/wm/paint/auth/kandinsky/kandinsky.comp-8.jpg), W.Kandinsky, 1923
* [Suprematist Composition: White on White.](https://www.moma.org/m/tours/7/tour_stops/171?locale=en), K.Malevitch, 1918
* [La trahison des images](http://www.renemagritte.org/images/paintings/the-treachery-of-images.jpg), R.Magritte, 1929

## Feedback

### Questionnaire

[https://forms.gle/C1h63ymmoqdceeme7](https://forms.gle/C1h63ymmoqdceeme7)
