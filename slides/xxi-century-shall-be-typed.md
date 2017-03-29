------------
title: Le XXIème sera typé ou ne sera pas
subtitle: Comment j'ai appris à aimer les types
author: Arnaud Bailly - @dr_c0d3
date: 2017-03-28
theme: serif-compact
transition: none
------------ 

# Plan #

------

* Introduction
* Voyage au pays des types
* Travaux pratiques
* Conclusion

# Introduction #

## Motivation

* Discussions sur le canal `#beginners` dans le slack `elm-lang`: Beaucoup du questions sur le type de `List a`...
* [Robert "Uncle Bob" Martin ](http://blog.cleancoder.com/uncle-bob/2016/05/01/TypeWars.html) sur la couverture des tests et les types:

    > You don’t need static type checking if you have 100% unit test coverage.
    
* [Making Impossible States Impossible](https://www.youtube.com/watch?v=IcgmSRJHu_8)
* Lien entre types et formalisation de *domaines métiers*

## Objectif

* Faire découvrir la richesse des systèmes de types des langages fonctionnels
* Donner envie d'aller y voir de plus près
* Mieux comprendre et mieux articuler cette compréhension

# Voyage au pays des types

----

<iframe width="800" height="450" src="https://www.youtube.com/embed/y2R3FvS4xr4" frameborder="0" allowfullscreen></iframe>

# Elm 

## Fonctions pures

![](/images/picasso-demoiselles.jpg)

## Fonctions pures

* Des *fonctions* sans effets de bord
* De préférences totales...
* De première classe dans le langage...
* Qui sont interprétées par le *runtime* qui encapsule l'ensemble des effets

## Fonctions pures

```haskell
init : ( Model, Cmd Msg )

update : Msg -> Model -> ( Model, Cmd Msg )

view : Model -> H.Html Msg
```

## Types de données algébriques

![](/images/mondrian-composition.jpg)

## Types de données algébriques

* Décrit les données comme engendrées par une structure *algébrique*
* Types construits par *composition* au moyen d'opérateurs *union* et *produit*
* Un type est une *fonction* dans le domaine des types, un *constructeur de types*
* Permet au compilateur de "garantir" la totalité d'une fonction

## Types de données algébriques

```haskell
type Msg
    = SubmitResponse (Maybe String)
    | UpdateResponse String
    | NoOp
```

# Haskell

## Classes de types

![](/images/pollock-untitled.jpg) 

## Classes de types

* Interface offerte par un type de données
* Permet d'encapsuler l'implémentation concrète
* Peut exprimer des relations complexes entre plusieurs types de données

## Classes de types

```haskell
class (Eq (Answer q)) => Questionable q where
  type Answer q :: *

  question :: q -> Text
  expected :: q -> Answer q
  response :: q -> Maybe (Answer q)
  answered :: q -> Maybe (Answer q) -> q
  
  isCorrectAnswer :: q -> Bool
  isCorrectAnswer q = Just (expected q) == response q
```


## Familles de types

![](/images/bacon-triptyque.jpg)

## Familles de types

* Relation entre types
* Rend le domaine d'une fonction dépendant du codomaine
* Peut être *ouvert* (relation extensible) ou *fermé* (le codomaine est fini)

----- 

```haskell
type Answer q :: *

type Answer QCM = Int
type Answer Grade = Int
type Answer OpenQuestion = Text
```

## Types existentiels

![](/images/magritte-pipe.jpg)

## Types existentiels

* Encapsuler un ensemble de types dans un autre type de données
* Renforce la distinction entre type abstrait et concret en rendant l'implémentation inaccessible
* Garantit la *localité* des opérations sur un type: la variable de type est nécessairement locale

## Types existentiels

```haskell
data Question where
  Question :: (Questionable q) 
           => q 
           -> (Text -> Maybe (Answer q))
           -> Question
```

# Idris #


## Types abstraits de données généralisés

![](/images/kandinsky-comp-8.jpg) 

## Types abstraits de données généralisés

* Applicable aux types paramétrés
* Chaque constructeur d'un type peut retourner une type différent
* Permet de spécialiser le paramètre en fonction du contexte dans lequel le type est appelé à s'insérer

## Types abstraits de données généralisés

```idris
data Command : Type -> Type where   
  Prompt         : Question -> Command Input
  AnswerQuestion : String -> Command Bool
  Back           : Command ()
  Quit           : Command ()
```

## Hole-Driven Development

## Hole-Driven Development

![](/images/duchamp-pissotière.jpg)

## Hole-Driven Development

* Utiliser l'inférence de type pour compléter le code par la structure attendue par le compilateur
* Permet de s'assurer dés le départ qu'une fonction est totale
* Permet à l'éditeur de compléter automatiquement le code

## Hole-Driven Development

```idris
runQuizz : Quizz n -> IO ()
runQuizz quizz@(MkQuizz answered current next) = do
  (input, quizz') <- runCommand quizz (Prompt current)
  case input of 
     GoBack         => ?hole_1
     QuitGame       => ?hole_2
     (GiveAnswer x) => ?hole_3
     Garbage        => ?hole_4
```

## Types dépendants 

![](/images/calle-sleepers.jpg)

## Types dépendants 

* Faire tomber la barrière entre les *types* et les *valeurs*
* Un type peut être définit par une fonction *dépendant*  de la valeur d'un paramètre
<* Introduit une hiérarchie potentiellement infinie de types (quel est le type de `Type` ?)
* Le *vérificateur de types* utilise la même sémantique que le *runtime*

## Types dépendants 


```idris
data Quizz : (numQuestions : Nat) -> Type where
  MkQuizz :  (answered : Vect n Answered) ->
             (current  : Question) -> 
             (next : Vect m Question) -> 
             Quizz (n + m)
```

## Paires dépendantes


![](/images/bourgeois-together.jpg)

## Paires dépendantes

* Empaqueter une valeur et un type dépendant de cette valeur dans une paire
* Généralisation des types existentiels
* Permet de *conserver* de l'information pour le compilateur

## Paires dépendantes

```idris
data Answered : Type where 
  MkAnswered : (question ** Answer question) -> Answered
```

## Type égalité

![](/images/klein-blue.jpg)

## Type égalité

* Exprimer l'égalité entre deux types dans le système de types
* Le type devient une *assertion* logique, une *proposition* qui doit être vérifiée par l'implémentation
* La seule valeur qui *habite* ce type est `Refl` qui est donc une *preuve* que l'égalité est vérifiée

## Type égalité

```idris
plusOneCommutes :  (n : Nat) 
                -> (m : Nat)
                -> (n + S m = S n + m)
```
## Types = Proposition

![](/images/malevitch-white-on-white.jpg)

## Types = Proposition

* Mise en oeuvre concrète de Curry-Howard
* Les types sont des *propositions*, les programmes des *preuves*
* Écrire un programme c'est démontrer que les types sont *habités*, que l'on peut effectivement construire des valeurs des types donnés
* De la conception de logiciels considérée comme énonciation de théorèmes...

# Travaux Pratiques

---- 

![](/images/braque-gueridon.jpg)

# Conclusion

----

* Le système de type est un outil essentiel dans la *conception* du logiciel
* Les systèmes de types fonctionnels sont de plus en plus sophistiqués et permettent de modéliser des contraintes de plus en plus proches du métier
* Le XXIème sera typé !

# Colophon

## (Quelques) Références 

* [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/), B.Pierce
* Advanced Types and Programming Languages, B.Pierce
* [Typing Haskell in Haskell](https://web.cecs.pdx.edu/~mpj/thih/thih.pdf)
* [Understanding types](http://lucacardelli.name/Papers/OnUnderstanding.A4.pdf) par Luca Cardelli, dans un contexte OO avec sous-typage
* [Familles de types en Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/type-families-and-pokemon)

## (Quelques) Références 

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

[Questionnaire](https://goo.gl/forms/dePn6qVnXSEUVprD2)
