------------
title: Sur "Living Documentation"
author: Arnaud Bailly
date: 2021-05-05
------------

Durant 3 mois avec quelques personnes du Slack [Okiwi](https://okiwi.org/), notamment [Sébastien Rabiller](https://www.linkedin.com/in/sebastienrabiller) et [Frédéric Faure](https://www.linkedin.com/in/ffaure32/), nous avons lu [Living Documentation](https://www.amazon.com/Living-Documentation-Cyrille-Martraire/dp/0134689321), un livre écrit par [Cyrille Martraire](https://www.linkedin.com/in/martraire/). Ce billet se veut être une brève synthèse des [notes brutes](/living-documentation.html) prises durant ces sessions.

Quelques remarques sur la forme:

- le format ePub s'est avéré assez pénible à utiliser sur nos ordinateurs : images mal placées, manque de repères pour naviguer, code mal formatté... ;
- la version disponible sur [LeanPub](https://leanpub.com/livingdocumentation) n'a pas bénéficié du même travail d'édition que la version Addison-Wesley, cette dernière est à privilégier et c'est celle que nous avons utilisée.

Sur le contenu du livre :

- la structure est un peu confuse, il y a pas mal de répétitions et la structuration en chapitre n'est pas vraiment évidente ;
- les limites du sujet sont floues, le livre couvre des domaines aussi divers que la question de l'acquisition de connaissances, la techniques des annotations en Java, le langage Gherkin et donne parfois le sentiment que l'auteur a voulu dire beaucoup de choses, partager de nombreuses idées et une expérience riche en se servant de l'angle "Documentation" comme d'un prétexte ;
- le volume consacré au différents sujets n'est pas toujours équilibré : certains sujets sont traités en un ou deux paragraphes, par exemple la documentation au travers des messages écrits dans  le système de gestion de versions, qui auraient mérités un chapitre complet ; quand d'autres techniques sont répétées dans plusieurs chapitres, par exemple l'utilisation des _Annotations_ pour "documenter" le code, l'architecture ou les _patterns_ utilisés ;
- les parties "dialoguées" destinées à rendre plus vivant les processus de décisions et de production sous-jacents à tel ou tel artefact documentaire ne sont pas très convaincantes, même si l'intention d'ancrer le propos dans des expériences concrètes est louable ;
- les références technologiques ou méthodologiques nous ont semblé parfois un peu "datées" et exclusivement centrées sur le monde Java.

Ce qui nous a manqué :

- une synthèse plus précise et ... synthétique à la fin de chaque chapitre, dans le style _bullet-point_ fréquent dans les ouvrages techniques américains, avec des conseils de mise en oeuvre des notions et outils vus dans le chapitre.
- des "exercices" ou axes de réflexion pour "travailler" ces notions, là aussi fréquents dans la littérature technique anglophone et qui, même si on ne les fait pas tous, permettent d'ancrer le contenu théorique dans une pratique concrète. Par exemple, à partir de l'idée du nuage de mots, proposer un exercice en lien avec un outil existant ou peut-être un script ou du code, à faire sur son propre code.
- des références pour aller plus loin, articles, livres, liens hyper-textes.

Ce que nous avons apprécié :

- de nombreuses bonnes idées que l'on aimerait mettre en pratique : l'utilisation d'[OpenTracing](https://opentracing.io/) pour documenter l'architecture du système et les schémas d'interactions, les nuages de mots extraits du code, la génération d'exemples à partir des tests ;
- l'accent mis par l'auteur sur le fait de considérer la production de documentation comme partie intégrante de _chaque phase_ du processus de développement et non comme une étape subsidiaire, ou subalterne, ce qui donne tout son sens à l'expression _Living Documentation_.

En conclusion : un livre au sujet passionnant avec beaucoup d'idées mais qui pâtit d'une forme un peu brouillonne.
