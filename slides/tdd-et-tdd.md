---
title: TDD & TDD sont dans un bateau
subtitle: TDD au delà des exemples
author: Arnaud Bailly - @dr_c0d3
institute: Input Output Global
date: 2022-10-21
theme: virgil-black-iohk
revealjs-url: /reveal.js
---

# Introduction

## Plan

* Pourquoi ?
* TDD par l'exemple
* Types et Tests de propriété
* Mutations
* Conclusion

## Présentation

* Actuellement architecte pour la blockchain Cardano
* Intéressé par le test depuis une thèse sur le sujet
* Pratiquant assidu d'_eXtreme Programming_
* Convaincu de l'intérêt du typage statique fort

# Pourquoi ?

## TDD

![](/images/tdd-kent-beck.png)

::: notes

* en tant que développeur, je me sens responsable de la qualité du code que je livre
* qualité = livrer non seulement un code correct mais aussi  qui répond au besoin de l'utilisateur
* _do the thing and do the right thing_

:::

## TDD

![](/images/tdd-is-dogma.png)

::: notes

* TDD permet d'atteindre ces deux objectifs en guidant la conception tout en se protégeant des évolutions futures
* mais on peut toujours faire mieux !

:::


# Développement Dirigé par les Tests

## Le problème

**N**uméro d'**I**nscription au **R**épertoire des Personnes Physiques

  =

[NIR](https://fr.wikipedia.org/wiki/Num%C3%A9ro_de_s%C3%A9curit%C3%A9_sociale_en_France)


::: notes

* Attribué à chaque enfant né en France
* Imaginé en 1940 pour les besoins de la Résistance, récupéré par Vichy, puis la IVème république
* Il a une sémantique bien précise => permet d'identifier des personnes => peu sûr
* ubiquitaire dans toutes les procédures administratives

:::

## Le problème {transition=none}

![](/images/nir-1.png)

## Le problème  {transition=none}

![](/images/nir-2.png)


::: notes

* des instructions de Vichy envisageaint d'utiliser le code pour ficher certaines catégories de population, ie. les juifs

:::

## Le problème  {transition=none}

![](/images/nir-3.png)

## Le problème  {transition=none}

![](/images/nir-4.png)

## Le problème  {transition=none}

![](/images/nir-5.png)

## Le problème  {transition=none}

![](/images/nir-6.png)

## Le problème  {transition=none}

![](/images/nir-7.png)

## Le problème  {transition=none}

![](/images/nir-8.png)

## Cycle TDD   {transition=none}

![](/images/tdd-1.png)

## Cycle TDD {transition=none}

![](/images/tdd-2.png)

## Cycle TDD {transition=none}

![](/images/tdd-3.png)

## Premier test

```haskell
validateINSEESpec = describe "Validate INSEE Number" $ do

    it "returns True given a valid INSEE Number" $
        validateINSEE "223115935012322" `shouldBe` True

```

::: notes

* framework de test BDD-style (hspec)
* demarrer par le test => définir l'interface
* on utilise

:::

## Une représentation _naïve_

```haskell
newtype INSEE1 = INSEE1 String
    deriving newtype (Eq, Show, IsString)

validateINSEE :: INSEE1 -> Bool
```

<div class="notes">
* simple encapsulation d'une chaîne de caractères
* un prédicat pour valider la chaîne
* simple, efficace, de bon goût
</div>

## Triangulation {transition=none}

```haskell
    it "must have the right length" $ do
        validateINSEE "2230" `shouldBe` False
        validateINSEE "2230159350123221" `shouldBe` False


```

::: notes

* on "réduit" l'espace des chaînes valides en identifiant des cas négatif
* c'est analogue à la _triangulation_ pour se repérer sur une carte
* on commence par une propriété de base de la chaîne

:::

## Triangulation {transition=none}

```haskell
    it "first character must be 1 or 2" $
        validateINSEE "323115935012322" `shouldBe` False



```

## Triangulation {transition=none}

```haskell
    it "characters at index 2 and 3 represent year" $
        validateINSEE "2ab115935012322" `shouldBe` False



```

::: notes

* le seul test négatif possible pour les années c'est d'avoir autre chose que des chiffres

:::

## Triangulation {transition=none}

```haskell
    it "characters at index 4 and 5 represent month" $ do
        validateINSEE "223ab5935012322" `shouldBe` False
        validateINSEE "223145935012322" `shouldBe` False
        validateINSEE "223005935012322" `shouldBe` False
```

::: notes

* le mois est plus intéressant: il faut supprimer plus de cas
* pointe un pb subtil dans notre approche: la chaîne n'est pas valide pour 2 raisons
  * celle explicite dans chaque cas testé
  * celle implicite dans le fait que la clé de contrôle serait invalide si elle était calculé

:::

## Triangulation {transition=none}

```haskell
    it "characters at index 6 and 7 represent department" $ do
        validateINSEE "22311xx35012322" `shouldBe` False
        validateINSEE "223119635012322" `shouldBe` False


```

## Triangulation {transition=none}

```haskell
    it "characters at index 6 and 7 contain 99 for a foreign-born person" $
        validateINSEE "200029923123486" `shouldBe` True



```

## Triangulation {transition=none}

```haskell
    it "characters 8, 9, and 10 represent city or country code" $
        validateINSEE "2231159zzz12322" `shouldBe` False



```

## Triangulation {transition=none}

```haskell
    it "characters 11, 12, and 13 represent an order" $
        validateINSEE "2231159123zzz22" `shouldBe` False



```

## Triangulation {transition=none}

```haskell
    it "characters 14 and 15 represent a control key" $ do
        validateINSEE "223115935012321" `shouldBe` False



```

## Le code du validateur

```haskell
validateINSEE (INSEE1 [gender, year1, year2, month1, month2, dept1, dept2, ...]) =
    validateGender gender
     && validateYear [year1, year2]
     && validateMonth [month1, month2]
     && validateDepartment [dept1, dept2]
```

::: notes

* on filtre la chaîne en capturant les différents caractères qui nous intéressent
* on a des fonctions individuelles de validatios pour chaque composant du NIR
* tous les cas ne sont pas encore implémentés

:::

----

```haskell
validateDepartment :: String -> Bool
validateDepartment dept =
    maybe False (\m -> m <= 95 && m > 0 || m == 99) (readNumber dept)

```

::: notes

* le code de validation du département est un peu plus complexe
* on traite le cas des personne nées à l'étranger
* pour des raisons de place on ne traite pas quelques cas particuliers:
  * les DOM/TOM
  * les personnes nées en Algérie/Tunisie/Maroc avant 1962

:::

## Après quelques cycles...

![](/images/tdd-nir-base-results.png)

----

![](/images/champagne.jpeg)

## Ou pas ?

* [Primitive Obsession](https://wiki.c2.com/?PrimitiveObsession) anti-pattern
* Nombre limité d'exemples utilisé pour trianguler
* On doit revalider à chaque réutilisation d'une valeur de type `INSEE`

::: notes

* Encapsuler une chaîne de caractères comme représentation interne c'est rapide mais peu robuste
* On ne représente pas les idées du domaine: genre, année et lieu de naissance, identification, clé

:::

# Un meilleur modèle

## De l'importance du domaine

![](/images/nir-hexagon.png)

::: notes

* séparer les concepts du domaine de leur représentation
* aka. architecture hexagonale
* aka. _Ports & Adapters_

:::

##

![](/images/use-the-types-luke.jpeg)

::: notes

* dans un langage statiquement typé on doit pouvoir représenter des exigences métiers dans le système de types
* un objet du domaine doit être structurellement valide pour exister
* aka. "Make impossible states impossible"

:::

## Use the Types, Luke

```haskell
data INSEE = INSEE
    { gender :: Gender
    , year :: Year
    , month :: Month
    , dept :: Department
    , commune :: Commune
    , order :: Order
    }
```

::: notes

* rendre explicite les choses, utiliser le langage du domaine
* détailler la structure valide  au moyen de champs aux noms et types descriptifs
:::

## Use the Types, Luke

La clé _ne fait pas partie_ du modèle, c'est une fonction dérivée

```haskell
computeINSEEKey :: INSEE -> Key
```

## Use the Types, Luke

```haskell
data Gender = M | F

data Month = Jan | Fev | Mar | Apr | Mai | Jun ...
```

::: notes

* contraindre les seules valeurs possibles au moyen "d'énumérations"
* par abus de langage, on parle de "types sommes"

:::

## Use the Types, Luke

```haskell
data Department
    = Dept (Zn 96)
    | Foreign
```

::: notes

* séparer explicitement les différents cas possibles d'un même type
* analogue au sous-typage dans les langages objets
* le langage permet de garantir la courverture de traitemetn des différents cas par "filtrage de motifs"

:::

## Use the Types, Luke

```haskell
newtype Year = Year (Zn 10)
    deriving (Eq, Show)

newtype Commune = Commune (Zn 1000)
    deriving (Eq, Show)

newtype Key = Key (Zn 100)
    deriving (Eq, Show)
```

::: notes

* expliciter les bornes d'un domaine numérique entier => notion de types dépendants
* `Zn` = groupe fini contenant `n` entiers
* par exemple, on a 100 valeurs différentes possibles pour une clé à 2 chiffres, 0 à 99
* il reste un codage résiduel, par exemple dans l'année sur 2 chiffres mais en l'absence d'autre information il est difficile de faire autrement
* les contraintes réelles pour le departement et la commune sont plus complexes
* => le bug dans Year est _volontaire_

:::

# Développement Dirigé par les Types

## Tester aux interfaces

![](/images/nir-hexagon.png)

::: notes

* le domaine est valide "par construction"
* on va s'intéresser aux entrées/sorties pour garantir aux clients/fournisseurs que les représentations produites sont correctes
* on devra quand même tester les fonctions internes au domaine bien sûr (calcul de la clé dans notre cas)

:::

## "Parse, Don't Validate"

©[Alexis King](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)

![](/images/nir-parse.png)

::: notes

* parse == analyser syntaxiquement == déserialiser
* aux frontières du domaine, on construit des valeurs de types valides par transformation d'une représentation externe
* on garantit à l'intérieur du domaine la cohérence des valeurs et leur correction
* on limite les erreurs d'utilisation potentielle -> limite la taille de l'ensemble des valeurs possibles d'un type

:::

## "Parse, Don't Validate"

```haskell
parse :: String -> Left ParseError INSEE
```

::: notes

* une chaîne de caractères quelconque peut ne pas être un identifiant INSEE valide, d'où le type de retour `Either`
* cette fonction a une propriété intéressante: elle est _totale_

:::

## Print = Parse$^{-1}$

![](/images/nir-pretty-print.png)

::: notes

* pretty-printing est l'inverse de parse == sérialiser
* utiliser pour transformer une valeur (par construction correcte) en une autre représentation (ici une chaîne de caractères)
* le consommateur pourra avoir sa propre représentation interne, ses propres types
* diminue le couplage tout en garantissant la correction de chaque domaine

:::

## Print = Parse$^{-1}$

```haskell
pretty :: INSEE -> String
```

::: notes

* pas besoin de traiter les cas d'erreur dans le codomaine de la fonction puisqu'une valeur `INSEE` est nécessairement correcte

:::

## Print = Parse$^{-1}$

> Étant donné un identifiant INSEE correct, on doit pouvoir reconstruire cet identifiant à partir d'une chaîne de caractères le représentant.

## Print = Parse$^{-1}$

```haskell
parseIsInverseToPrettyPrint insee =
    (parse . print) insee == Right insee &
       tabulate "Year" [yearRange (print insee)]

```

::: notes

* on définit une propriété d'isomorphisme: toute valeur
* utiliser la structure du type pour échantilloner l'ensemble des valeurs possibles => `Arbitrary`
* on compare la composition des deux fonctions `print` et `parse` avec la valeur arbitraire initiale
* `tabulate` calcule et affiche une distribution de valeurs

:::

## Générer des valeurs arbitraires

```haskell
instance Arbitrary Gender where
    arbitrary = elements [M, F]

instance Arbitrary Year where
    arbitrary = Year <$> someZn

instance Arbitrary Month where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Department where
    arbitrary = frequency [(9, Dept <$> someZn), (1, pure Foreign)]

```

::: notes

:::

## Exécution avec QuickCheck {transition=none}

![](/images/nir-prop-invalid.png)

## Exécution avec QuickCheck {transition=none}

![](/images/nir-prop-invalid2.png)

::: notes

* on constate une anomalie dans la distribution des années: on devrait aller de 0 à 99 mais on ne va que de 0 à 10
* _errare humanum est_ -> on a fait une erreur dans la définition du type `Year`

:::

## Exécution avec QuickCheck {transition=none}

```haskell
newtype Year = Year (Zn 10)
    deriving (Eq, Show)
```

devrait être

```haskell
newtype Year = Year (Zn 100)
    deriving (Eq, Show)
```

::: notes

* _errare humanum est_ -> on a fait une erreur dans la définition du type `Year`
* il est essentiel de valider que nos générateurs produisent bien les valeurs auxquelles on s'attend
* QuickCheck permet de calculer ces distributions et même de faire échouer le test si la couverture n'est pas suffisamment "bonne"

:::

----

![](/images/champagne.jpeg)

## Ou pas ?

* On n'a pas vraiment vérifié la fonction `computeKey`
* La propriété vérifie que toute chaîne valide produit une valeur INSEE correcte
* Mais qu'en-est-il de la contraposée : rejette-t'elle toute chaîne incorrecte ?

# Mutations

## Idée :tada:

* Partir d'une valeur `INSEE` correcte
* Lui appliquer une mutation _arbitraire_ la transformant en valeur incorrecte
* Vérifier que `parse` la rejette

## Propriété

```haskell
inseeValidatorKillsMutants =
    forAll arbitrary $ \insee ->
        forAll genMutant $ \mutation ->
            let mutant = mutation `mutate` insee
                parsedInsee = parse mutant
             in isLeft parsedInsee &
                 tabulate "Mutation"
                   [takeWhile (not . isSpace) $
                     show mutation]
```

## Les mutants

```haskell
data Mutation = MutateYear {position :: Int, character :: Char}
    deriving (Eq, Show)
```

```haskell
mutate :: Mutation -> INSEE -> String
```

::: notes

* il faudrait étendre le type `Mutation` pour traiter les différents champs, on ne considère ici que l'année
* chaque mutation agit en remplacant des caractères de la représentation sous forme de chaînes du NIR par d'autres caractères
* `mutate` applique une mutation sur une valeur et produit une chaîne supposée invalide

:::

## Générateur

```haskell
genMutant :: Gen Mutation
genMutant = do
  position <- choose (1, 2)
  character <- arbitraryPrintableChar
  pure $ MutateYear{position, character}
```

::: notes

* on génère des mutation affectant spécifiquement un champ du NIR, ici uniquement l'année
* le caractère remplacé est arbitraire

:::

## Résultat :bomb:

![](/images/nir-mutant-invalid.png)

::: notes

* QuickCheck a trouvé un jeu de valeurs invalidant la propriété
* il est tout à fait possible que deux NIR différant par l'année ait la même clé
* les mutations produisent des "individus" viables

:::

## Générateur amélioré

```haskell
genMutant = do
  position <- choose (1, 2)
  character <- arbitraryPrintableChar  `suchThat` (not . isDigit)
  pure $ MutateYear{position, character}
```

::: notes

* le caractère remplacé est arbitraire et ne doit pas  être un nombre
* on pourrait envisager d'avoir des mutations plus subtils qui remplacent un chiffre par un à condition de vérifier que la clé est différente
* mais on peut aussi muter la clé...

:::

## Un mutant pour les clés

```haskell
genMutantForKey :: Gen Mutation
genMutantForKey =
  MutateKey . Key <$> someZn
```

```haskell
genMutant =
  oneof [ genMutantForYear, genMutantForKey ]
```

::: notes

* le caractère remplacé est arbitraire et ne doit pas  être un nombre
* on pourrait envisager d'avoir des mutations plus subtils qui remplacent un chiffre par un à condition de vérifier que la clé est différente
* mais on peut aussi muter la clé...

:::

## Résultat :champagne:

![](/images/nir-mutant-valid.png)

##

![](/images/gatsby.jpeg)

# Conclusion

## Types + Tests = :rocket:

* Le typage fort s'intègre parfaitement dans le cycle TDD
* Le test de propriété évite de devoir énumérer des exemples explicitement
* L'utilisation de mutations améliore le processus de "triangulation" du domaine
* Utilisé dans la vraie vie:
  * [Cardano Ledger](https://github.com/input-output-hk/cardano-ledger)
  * [Hydra](https://github.com/input-output-hk/hydra-poc)
  * [Quviq](http://www.quviq.com/)

## Si je fais pas de Haskell ?

* Tous les langages ont désormais leur bibliothèque de tests de propriété
  * Python $\Rightarrow$ [hypothesis](https://hypothesis.readthedocs.io/en/latest/)
  * Rust  $\Rightarrow$ [quickcheck](https://crates.io/crates/quickcheck)
  * Java $\Rightarrow$ [jqwik](https://jqwik.net/)
  * ...
