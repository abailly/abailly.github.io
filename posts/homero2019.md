------------
title: Homero 2019
author: Arnaud Bailly
date: 2019-01-02
------------

En écoutant [France inter](https://www.franceinter.fr/emissions/les-histoires-du-monde/les-histoires-du-monde-02-janvier-2019) ce matin j'ai eu immédiatement envie de partager cette lecture collective sur twitter de l'Iliade en français. J'ai donc immédiatement tweeté les premiers vers du premier chant de l'Iiliade, avant de me rendre compte que la contrainte exigeait de twitter _un chant par semaine_. Il est donc devenu urgent d'écrire un programme pour faire en sorte de pouvoir générer ces tweets automatiquement au lieu de devoir laborieusement les taper un par un. Voici le fruit, brut de décoffrage de cette micro-aventure...

Tout d'abord un peu de cérémonies pour que le fichier contenant le code soit interprétable comme un script:

~~~~haskell
#!/usr/bin/env stack
-- stack runhaskell --resolver lts-12.5 --
~~~~

Puis les inévitables extensions de langage et imports...

~~~~haskell
{-# LANGUAGE LambdaCase #-}
import           Control.Applicative
import           Data.Char
import qualified Data.List           as List
import           Data.Monoid
import           System.Directory
import           System.Environment
import           System.IO
~~~~

On va avoir envie de générer plus d'un tweet à la fois, et on va donc lire le nombre de tweets à générer en argument de la ligne de commande:

~~~~haskell
getNumTweets :: IO Int
getNumTweets = getArgs >>= \case
  [] -> pure 1
  (n:_) -> pure $ read n
~~~~

Le principe de ce programme, concentré dans la fonction `makeTweets` est extrêmement simple, on pourrait même dire brutal:

* On va itérer jusqu'à ce que le nombre de tweets à générer soit de 0:

~~~~haskell
makeTweets :: Int -> String -> IO ()
makeTweets 0 _ = pure ()
makeTweets numTweets content = do
~~~~

* On va lire le curseur depuis un fichier intitulé `.break` dans le répertoire courant, curseur qui stocke notre "état" sous la forme de l'index du premier mot du reste du texte. Si le fichier n'existe pas, on part tout simplement du début

~~~~haskell
  start <- readBreak <|> pure 0
~~~~

* On va travailler sur la liste des listes de mots du texte en entrée, où un mot est simplement une suite de lettres séparée par des espaces. Comme Haskell est un langage _paresseux_, cette liste ne va pas être construite explicitement si ce n'est pas nécessaire. Pour fluidifier la lecture on enlève du texte les renvois de notes qui apparaissent sous la forme de nombres

~~~~haskell
  let ws = List.inits . drop start . words . filter (not . isDigit) $ content
~~~~

* De cette liste de liste de mots, on va sélectionner la _dernière_ occurence qui:
    1. à une longueur inférieure à 260 lorsqu'on reconstitue une phrase à partir des mots
    2. se termnine par un signe de ponctuation

~~~~haskell
      sentence = lastWithPunctuation $ takeWhile ((< 260) . length . unwords) ws
~~~~

* Il ne reste plus qu'à mettre à jour notre "état" avec la longueur de la liste de mots sélectionnée, puis à imprimer la phrase ornée du hashtag `#homero2019` sur la sortie standard
* La récursion peut se poursuivre en réduisant le nombre de tweets et en conservant le même contenu

~~~~haskell
      end = start + length sentence
  putStrLn $ unwords sentence <> " #homero2019"
  writeBreak end
  makeTweets (numTweets - 1) content
~~~~

La fonction auxiliaire `lastWithPunctuation` est particulièrement brutale et dangereuse, mais néanmoins amusante:

* On inverse l'ensemble des listes, soit 3 niveaux de listes imbriquées jusqu'à identifier le dernier caractère de chaque "phrase" candidate
* On passe toutes les phrases qui ne se terminent pas par un signe de ponctuation
* et on retourne la première occurence qui se termine par un tel signe.

~~~~haskell
lastWithPunctuation :: [[ String ]] -> [String]
lastWithPunctuation = head . dropWhile (\ s -> not $ isPunctuation $ head $ head $ reverse <$> reverse s) . reverse
~~~~

Les deux fonctions `readBreak` et `writeBreak` servent simplement à mettre à jour le fichier `.break` contenant l'état du flux de tweets.

~~~~haskell
writeBreak :: Int -> IO ()
writeBreak = writeFile ".break" . show

readBreak :: IO Int
readBreak = do
  exist <- doesFileExist ".break"
  if exist
    then read <$> readFile ".break"
    else pure 0
~~~~

Il ne reste plus qu'à écrire le `main` dont le principal travail est d'appeler `makeTweets` avec le contenu lu depuis l'entrée standard:

~~~~haskell
main :: IO ()
main = do
  numTweets <- getNumTweets
  getContents >>= makeTweets numTweets
~~~~

En téléchargeant le Chant I depuis le [site officiel de l'éditeur](https://lesbelleslettresblog.com/2015/03/25/homere-iliade-chant-i-en-version-integrale-dans-la-traduction-de-paul-mazon/) et en le copiant/collant dans un fichier `chant1`, on peut lancer le programme `tweets.hs`:

```
$ cat chant1 | ./tweets.hs 3
Chante, déesse, la colère d’Achille, le fils de Pélée; détestable colère, qui aux Achéens valut des souffrances sans nombre et jeta en pâture à Hadès tant d’âmes fières de héros, #homero2019
tandis que de ces héros mêmes elle faisait la proie des chiens et de tous les oiseaux du ciel – pour l’achèvement du dessein de Zeus. Pars du jour où une querelle tout d’abord divisa le fils d’Atrée, protecteur de son peuple, et le divin Achille. #homero2019
Qui des dieux les mit donc aux prises en telle querelle et bataille ? Le fils de Létô et de Zeus. C’est lui qui, courroucé contre le roi, fit par toute l’armée grandir un mal cruel, dont les hommes allaient mourant; cela, #homero2019
```

j'aurais aimé pouvoir utiliser [tweet-hs](https://hackage.haskell.org/package/tweet-hs) pour pouvoir effectivement tweeté ces textes mais malheureusement, le processus pour obtenir des clés OAuth sur twitter est long et pénible et je ne sais pas si j'aurais l'énergie nécessaire pour ce faire...
