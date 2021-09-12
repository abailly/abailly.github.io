------------
title: A Tale of Two TDDs
subtitle: In Praise of Fancy Haskell, or Why we Need Elaborate Type Systems
author: Arnaud Bailly
date: 2020-11-01
------------

This is the written and somewhat extended version of a talk I gave at [Agile Tour Bordeaux 2020](http://agiletourbordeaux.fr/), and a basis for a hands-on workshop I will give at [DDDEurope 2021](https://dddeurope.com/2021/handson-conference/). The goal of these sessions is to demonstrate that Test-Driven and Type-Driven Design are complementary techniques that play the same role

# A Simple Example: Modeling Quizz Questions

~~~~ {.haskell}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Presentation where

import Test.Hspec
import Data.String
import Test.QuickCheck
import Data.Char (toUpper, isUpper, isLower, toLower)

~~~~

~~~~ {.haskell}
data Question = Q {question :: String, expectedAnswer :: CaseInsensitive}
~~~~

~~~~ {.haskell}
verifyAnswerSpec :: Spec
verifyAnswerSpec = describe "Verify Answer" $ do
  let question1 = Q "Quel est la couleur du cheval blanc d'Henri IV?" "blanc"

  it "retourne True si la réponse donnée est égale à la réponse attendue" $ do
    verifieLaRéponse "blanc" question1 `shouldBe` True

  it "retourne False si la réponse donnée n'est pas égale à la réponse attendue" $ do
    verifieLaRéponse "noir" question1 `shouldBe` False

~~~~

~~~~ {.haskell}

verifieLaRéponse :: CaseInsensitive -> Question -> Bool
verifieLaRéponse proposition Q{reponseAttendue} =
  proposition == reponseAttendue
~~~~

~~~~ {.haskell}

-- un type représentant les chaînes de caractères insensibles à la casse
newtype CaseInsensitive = CaseInsensitive { sansCasse :: String }
  deriving (Show)

~~~~

~~~~ {.haskell}
instance IsString CaseInsensitive where
  fromString = CaseInsensitive

~~~~


~~~~ {.haskell}
egaliteCaseInsensitive :: CaseInsensitive -> Property
egaliteCaseInsensitive sc@(CaseInsensitive base) =
  collect (length base) $ CaseInsensitive (permuteCasse base) == sc

~~~~

~~~~ {.haskell}
permuteCasse :: String -> String
permuteCasse [] = []
permuteCasse (c:cs)
  | isUpper c =  toLower c : permuteCasse cs
  | isLower c =  toUpper c : permuteCasse cs
  | otherwise = c : permuteCasse cs
~~~~

~~~~ {.haskell}
instance Arbitrary CaseInsensitive where
  arbitrary = CaseInsensitive . getASCIIString <$> arbitrary
~~~~

~~~~ {.haskell}
instance Eq CaseInsensitive where
  CaseInsensitive sc1 == CaseInsensitive sc2 =
    fmap toLower sc1 == fmap toLower sc2
~~~~
