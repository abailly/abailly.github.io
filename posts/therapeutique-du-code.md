------------
title: Pour une thérapeutique du code
author: Arnaud Bailly 
date: 2017-04-14
------------

Cela fait désormais de nombreuses années que j'écris du code qui a vocation à se retrouver dans des logiciels, eux-mêmes ayant vocation à répondre à des fonctions diverses et variées. Dans ma relation au code, je fait régulièrement l'expérience d'une véritable difficulté physique, d'une douleur : je suis en train de travailler sur un programme, un algorithme, un bout de truc pour déployer une application ; le temps passe et cela ne fonctionne pas correctement, je m'obstine et c'est comme si je poussais un rocher de plus en plus lourd ; la lucidité s'émousse, la clairvoyance sur les moyens et les fins s'embrume par l'accumulation de détails apparemment absurdes et un dédale de pages *web* prodiguant des solutions à tel problème qui font naître d'autres problèmes de plus en plus éloignés du but recherché[^2] ; et dans mon cas se manifeste une douleur physique là, entre les omoplates, une tension musculaire dépassant le seuil de l'inconfort pour se transformer en brûlure, et c'est comme si j'avais dû traîner une lourde charge sur plusieurs centaines de mètres, un rocher de Sysiphe en miniature.

Cette sensation récurrente combinée à mon expérience du travail de développeur dans des organisations aussi diverses que des banques, des startups, des éditeurs de logiciels petits et gros ma fait prendre conscience de la nécessité et de la pertinence d'adopter une approche *thérapeutique* du code, d'où est née l'idée de créer [Dr.Code](http://drcode.io). Mais qu'est ce que ça veut dire une «approche thérapeutique» du code ? Voila ce que je voudrais expliciter ici...

Par *code* j'entends ici l'ensemble des artefacts qui constituent un logiciel ou un système logiciel : non seulement le code source de l'application mais aussi ses scripts, l'infrastructure de déploiement, le *build*, les outils de développement, bref les éléments, objets et systèmes techniques qui sont la matière, l'environnement autant que le fruit du travail des développeurs[^3]. Ces «êtres de la technique», pour reprendre les mots de [Bruno Latour](/posts/eme.html), ont une existence propre, un *mode d'existence* spécifique, pour reprendre, là encore non seulement Latour, mais aussi toute une tradition souterraine et néanmoins vivace de la philosophie contemporaine qui, depuis au moins [Simondon](/posts/objets-techniques.html), cherche à comprendre le concept de technique et l'importance croissante qu'il a pris dans nos vies[^4].

Que le code me résiste est pour moi caractéristique : cette chose existe en dehors de moi, au point de pouvoir me faire mal, engendrer de la douleur mais aussi du plaisir. Et comme l'explique fort bien [Latour](http://modesofexistence.org/crossings//#/fr/tec-ref), il ne s'agit pas ici d'un déficit de connaissance ou de compétence mais bien d'une question d'ingéniosité, d'un tour de main, d'astuce ou de *metis* pour reprendre le beau mot grec. Le code est cet être technique qui demande de celui qui entre en relation avec lui de la metis, soit un mélange de sagesse, d'expérience, de connaissances, d'astuce - de rouerie aussi - qui permet de *trouver le bon angle* d'attaque, la meilleure approche, le *bon algo* ou la *bonne lib*... 

> Par un autre détournement, les couches successives d’un programme, d’un compilateur, d’une puce, d’un radar parviennent à se compliquer et à s’aligner au point de remplacer le solide attelage qui attachait jusqu’ici les wagons d’un métro automatique pour finir par un « attelage immatériel » entièrement calculé. 
>
> B.Latour, [Enquête sur les modes d'existence](http://modesofexistence.org/crossings//#/fr/tec-ref)

Quand je travaille le bois, ce qui m'arrive de temps à autre, il est clair que le bois a un *grain*, des propriétés qui dépendent de son essence - le chêne sera beaucoup plus dur à travailler que du pin, le frêne est plus rigide et «fonctionne» bien pour faire des étagères ou des grandes longueurs, de sa géométrie - on ne fait pas de plan de travail d'un seul tenant parce que sinon il va se vriller, il faut ajuster plusieurs planches les une aux autres, de l'usage que je compte en faire - ce n'est pas une bonne idée de faire un coffrage dans une pièce humide avec du pin... C'est évident parce que le bois est un matériau vivant, mais c'est aussi vrai pour du béton par exemple, de la pierre, du tissu ou n'importe quel autre matériau que les humains sont amenés à «travailler».

Et c'est aussi le cas des outils : non seulement leur usage, leur destination, mais encore leurs propriétés spécifiques, se prêteront avec plus ou moins de bonne grâce aux *desiderata* de l'artisan qui les met en oeuvre. Pour atteindre son but, celui-ci devra donc mobiliser tous ces êtres en vue de l'atteinte dudit but, et cela se fera avec d'autant plus d'aisance qu'il suivra le *fil du bois*, le grain des choses, qu'il saura tirer parti des caractéristiques de chacun d'entre eux, non seulement générales - utiliser un marteau pour frapper un clou plutôt qu'un tournevis - mais aussi spécifiques - savoir que ce marteau est équilibré de telle manière pour ajuster la force de frappe et éviter de marquer le bois. Lorsque cela «fonctionne», l'artisan et le codeur peuvent atteindre cet état que l'on appelle le *flow* qui n'a rien de mystérieux ni de mystique - sauf à considérer que l'expérience mystique est elle-même caractérisée par un état de flow - mais est simplement la manifestation psychologique et physique d'une congruence plus ou moins parfaite entre tous les êtres qui participent du travail de l'artisan.

Approcher le code en thérapeute consiste simplement à considérer les dysfonctionnements de celui-ci - ses «bugs», ses plantages, sa «dette technique», ses branches mortes... - comme des *symptômes* d'une infélicité[^5] plus profonde dans l'existence de ce code. Et donc à rechercher au delà de ces symptômes les conditions de leur apparition, de la même manière qu'un médecin au chevet d'un malade cherchera au delà de la fièvre ou de l'apparition de boutons à identifier la maladie. 

Mais de même que le médecin, lorsqu'il fait le travail pour lequel il a prêté serment, va prendre en considération non seulement la maladie mais aussi la personne, son environnement, son mode de vie, son hygiène, son passé, la santé de ses ascendants, afin de proposer une thérapeutique qui ne «guérisse» pas simplement les symptômes mais surtout *améliore* la santé du patient ; de même est-il nécessaire de prendre en compte la relation du code avec son environnement si l'on veut espérer le «soigner» réellement. Or qu'est ce que l'environnement du code ? Ce sont les humains qui le manipule et le produisent, bien sûr, et tous les autres êtres non techniques telles que les organisations, les êtres de fiction, les liens politiques avec lesquels il interagit ! 

La [loi de Conway](http://www.melconway.com/Home/Conways_Law.html) qui nous montre comment l'existence du code est influencée par la structure de l'organisation au sein de laquelle il est produit, le [style](https://refuses.github.io/preprints/writing.pdf) d'écriture du code qui reflète la pensée des êtres l'ayant écrit sont des manifestations bien connues de ces relations.

Soigner le code, c'est donc aussi soigner les *codeurs* qui l'écrivent, le modifient, le manipulent ; *l'organisation* dans laquelle il s'insère ; les *croyances* et *idées politiques* qui circulent et constituent le substrat idéologique informant les conditions de production du code... Bref, c'est s'intéresser à tous les *actants* du *système* de code.

Dr.Code est la matérialisation de ces idées, somme toute assez simples, dont je tire quelques principes destinés à guider les *consultations*:

Le patient est le mieux placé pour identifier les remèdes à ses maux

: ... dans la mesure où il accepte de faire le travail d'analyse et d'enquête nécessaire. Cela signifie que le but de la consultation n'est pas de traiter les symptômes de manière mécaniste mais d'enquêter et de faire émerger au grand jour les problèmes que les douleurs ressenties par et dans le code manifestent[^7].

Le praticien a pour rôle d'aider le patient à faire ce travail

: ... et pas de lui faire appliquer telle ou telle technique, ou adopter tel ou tel outil, telle ou telle méthode. Sa position n'est pas celle d'un expert surplombant les problèmes porté par les ailes de son savoir encyclopédique. La connaissance et l'expertise sont conçues comme ouverture du champ des *possibles*, combustible pour la pensée, apport d'énergie pour accroître les potentialités du système. Il y a peu de cas où des solutions purement mécanistes s'avèrent pertinentes. 

Le dialogue *avec* et autour du code est le principal outil utilisé

: ... selon l'antique pratique de la maïeutique[^6]. Il s'agit de proposer une écoute active, participative et bienveillante de tous les acteurs concernés, dans la mesure du possible. *Active* car l'écoutant n'est pas une chose inerte qui ne serait que le simple reflet de ma conscience, une marionnette que je manipule, mais une entité autonome avec ses propres modalités d'existence qu'il est nécessaire de bien comprendre pour mieux la *traduire* ; *participative* car le dialogue est structuré par des questions permettant de préciser, relancer, ouvrir ; *bienveillante* car, jusqu`à preuve du contraire, sont présupposées la bonne foi et la bonne volonté de tous les *actants* du système.

Les solutions techniques s'inscrivent dans un contexte

: ... et ne sont jamais comprises comme des [balles en argent](http://worrydream.com/refs/Brooks-NoSilverBullet.pdf) capables de résoudre les problèmes à elles seules. Le *Développement Dirigé par les Tests* est une [technique puissante](/posts/tdd.html) mais qui ne résoudra pas les problèmes du code, elle ne fera que révéler des tensions, des blocages ou des ouvertures qu'il faudra résoudre ou exploiter. Les métriques de qualité du code ne prendront sens que si elles sont produites en vue d'une fin précise, par rapport à des hypothèses qu'elles doivent permettre de valider ou des contraintes qu'elles matérialisent.

On l'aura compris, approcher le code sous l'angle d'une thérapeutique n'est pas le chemin le plus facile et ne conviendra pas nécessairement à tous. Certains préféreront une thérapeutique mécaniste, symptomatique, à base de solutions technologiques ou organisationnelles formatées, certifiées et immédiatement reconnaissables ; des solutions que, reprenant la terminologie du modèle [Cynefin](https://en.wikipedia.org/wiki/Cynefin_framework), nous pourrions dire relever des domaines *Simple* ou *Compliqué*. Et dans nombre de cas, cette approche sera pertinente et efficace.

Pour les autres, dont la situation est de l'ordre du *Complexe*, le dialogue devra permettre d'interroger en profondeur l'ensemble du système et de ses acteurs, les habitudes, les relations entre différents modes d'existence afin de conduire à une meilleure hygiène de vie du code.

[^1]: cf. Gerald Weinberg, vol.3, Quality Software Management

[^2]: L'expression consacrée est [tondre un yack](https://recher.wordpress.com/2016/02/28/tondre-un-yak/) même si la tonte de yack s'apparent plus souvent à la procrastination et n'est pas nécessairement source de frustrations.

[^3]: le terme de développeur étant ici aussi entendu au sens large et incluant tous ceux qui de près ou de loin ont affaire (ou à faire) au code : testeurs, chefs de projet, SRE, DevOps, DBAs, architectes et bien sûr le treillis de *managers* «nécessaires» à faire travailler ensemble ces différents métiers.

[^4]: Ce point devrait faire l'objet d'un article plus élaboré...

[^5]: Le terme est encore de Latour

[^6]: Les développeurs ont l'habitude d'utiliser un canard en plastique comme interlocuteur, d'où l'expression *rubber ducking*.

[^7]: Pour le dire dans les termes de Spinoza, la compréhension est augmentation de ma puissance d'agir et principale source de *Joie*.
