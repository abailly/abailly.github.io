------------
title: Thérapeutique du code
author: Arnaud Bailly 
date: 2017-04-12
------------

Cela fait désormais de nombreuses années que j'écris du code qui a vocation à se retrouver dans des logiciels, eux-mêmes ayant vocation à répondre à des fonctions diverses et variées. Dans ma relation au code, je fait régulièrement l'expérience d'une véritable difficulté physique, d'une douleur : je suis en train de travailler sur un programme, un algorithme, un bout de truc pour déployer une application ; le temps passe et cela ne fonctionne pas correctement, je m'obstine et c'est comme si je poussais un rocher de plus en plus lourd ; le temps passant la lucidité s'émousse, la clairvoyance sur les moyens et les fins s'embrume par l'accumulation de détails apparemment absurdes, un dédale de pages *web* prodiguant des solutions à tel problème qui font naître d'autres problèmes de plus en plus éloignés du but recherché[^2] ; et dans mon cas se manifeste une douleur physique là, entre les omoplates, une tension musculaire dépassant le seuil de l'inconfort pour se transformer en brûlure, et c'est comme si j'avais dû traîner une lourde charge sur plusieurs centaines de mètres, un rocher de Sysiphe en miniature.

Cette sensation récurrente combinée à mon expérience du travail de développeur dans des organisations aussi diverses que des banques, des startups, des éditeurs de logiciels petits et gros ma fait prendre conscience de la nécessité et de la pertinence d'adopter une approche *thérapeutique* du code, d'où est née l'idée de créer [Dr.Code](http://drcode.io). Mais qu'est ce que ça veut dire une "approche thérapeutique" du code ? Voila ce que je voudrais expliciter ici...

Par *code* j'entends ici l'ensemble des artefacts qui constituent un logiciel ou un système logiciel, non seulement le code source de l'application mais aussi ses scripts, l'infrastructure de déploiement, le *build*, les outils de développement, bref les éléments, objets et systèmes techniques qui sont la matière, l'environnement autant que le fruit du travail des développeurs[^3]. Ces "êtres de la technique", pour reprendre les mots de [Bruno Latour](/posts/eme.html), ont une existence propre, un *mode d'existence* spécifique, pour reprendre là encore Latour et toute une tradition souterraine mais néanmoins vivace de la philosophie contemporaine qui, depuis au moins [Simondon](/posts/objets-techniques.html), cherche à comprendre le concept de technique et l'importance croissante qu'il a pris dans nos vies[^4].

Que le code me résiste est pour moi caractéristique : cette chose existe en dehors de moi, au point de pouvoir me faire mal, engendrer de la douleur mais aussi du plaisir. Et comme l'explique fort bien [Latour](http://modesofexistence.org/crossings//#/fr/tec-ref), il ne s'agit pas ici d'un déficit de connaissance ou de compétence mais bien d'une question d'ingéniosité, d'un tour de main, d'astuce ou de *metis* pour reprendre le beau mot grec. Le code est cet être technique qui demande de celui qui entre en relation avec lui de la metis, soit un mélange de sagesse, d'expérience, de connaissances, d'astuce - de rouerie aussi - qui permet de *trouver le bon angle* d'attaque, la meilleure approche, le *bon algo* ou la *bonne lib*... 

Quand je travaille le bois, ce qui m'arrive de temps à autre, il est clair que le bois a un *grain*, des propriétés qui dépendent de son essence - le chêne sera beaucoup plus dur à travailler que du pin, le frêne est plus rigide et "fonctionne" bien pour faire des étagères ou des grandes longueurs, de sa géométrie - on ne fait pas de plan de travail d'un seul tenant parce que sinon il va se vriller, il faut ajuster plusieurs planches les une aux autres, de l'usage que je compte en faire - ce n'est pas une bonne idée de faire un coffrage dans une pièce humide avec du pin... C'est évident parce que le bois est un matériau vivant, mais c'est aussi le cas pour du béton par exemple, de la pierre, du tissu ou n'importe quel autre matériau que les humains sont amenés à "travailler".

Mais c'est aussi le cas des outils : non seulement leur usage, leur destination,  mais aussi leurs propriétés spécifiques, se prêteront avec plus ou moins de bonne grâce aux desiderata de l'artisan qui les met en oeuvre. Pour atteindre son but, celui-ci devra donc mobiliser tous ces êtres en vue de l'atteinte dudit but, et cela se fera avec d'autant plus d'aisance qu'il suivra le *fil du bois*, le grain des choses, qu'il saura tirer parti des caractéristiques de chacun d'entre eux, non seulement générales - utiliser un marteau pour frapper un clou plutôt qu'un tournevis - mais aussi spécifiques - savoir que ce marteau est équilibré de telle manière pour ajuster la force de frappe et éviter de marquer le bois.

Approcher le code en thérapeute consiste simplement à considérer les dysfonctionnements de celui-ci - ses "bugs", ses plantages, sa "dette technique", ses branches mortes... - comme des *symptômes* d'une infélicité[^5] plus profonde dans l'existence de ce code. Et donc à rechercher au delà de ces symptômes les conditions de leur apparition, de la même manière qu'un médecin au chevet d'un malade 

dans la relation qu'il entretient avec son environnement, de la même manière qu'un toux ou une fièvre sont les symptômes qu'un corps émet Or qu'est ce que l'environnement du code ? Ce sont les humains **et** tous les autres êtres non techniques telles que les organisations, les êtres de fiction, les liens politiques avec lesquels il interagit ! 

* Ne plus parler du code, mais parler *au* code => thérapeutique de la parole
    * psychanalyse du code?
    * c'est "quoi" le code? 
* Le code a un "grain" comme du bois, issu des dépôts successifs de fonctionnalités, de l'interaction avec les développeurs précédents
    * Ne pas aller contre le grain du code permet d'éviter des frustrations
    * Impression de *pousser* dans le code, pénibilité, sensation physique de difficulté 
    * vs. sentiment de *flow* => congruence[^1]
* Loi de conway, congruence entre structure/architecture du code et application
* Code is Law 
* Soigner du code, qu'est ce que ça veut dire? c'est aussi prendre soin des codeurs -> le code n'est pas produit dans le vide...
* Le patient est le mieux placé pour améliorer sa santé! 
    * Ne pas le bourrer de médicaments
    * Contre l'approche mécaniste de la médecine "classique"
    * ex. maladie "invisible", diabète => hygiène de vie vs. traitement médicamenteux
* Faire émerger les solutions par la discussion => rubber ducking
* Pouvoir apporter une expertise *quand c'est nécessaire* pour ouvrir le champ des possibles, pas pour imposer une solution toute faite et pré-calibrée

> Par un autre détournement, les couches successives d’un programme, d’un compilateur, d’une puce, d’un radar parviennent à se compliquer et à s’aligner au point de remplacer le solide attelage qui attachait jusqu’ici les wagons d’un métro automatique pour finir par un « attelage immatériel » entièrement calculé. 
> http://modesofexistence.org/crossings//#/fr/tec-ref

[^1]: cf. Gerald Weinberg, vol.3, Quality Software Management

[^2]: L'expression consacrée est [tondre un yack](https://recher.wordpress.com/2016/02/28/tondre-un-yak/) même si la tonte de yack s'apparent plus souvent à la procrastination et n'est pas nécessairement source de frustrations.

[^3]: le terme de développeur étant ici aussi entendu au sens large et incluant tous ceux qui de près ou de loin ont affaire (ou à faire) au code : testeurs, chefs de projet, SRE, DevOps, DBAs, architectes et bien sûr le treillis de *managers* "nécessaires" à faire travailler ensemble ces différents métiers.

[^4]: Ce point devrait faire l'objet d'un article plus élaboré...

[^5]: Le terme est encore de Latour
