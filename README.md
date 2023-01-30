Projet de Programmation Fonctionnelle Avancée
============================================

**M1 Informatique - Université Paris Cité**

### Enoncé : programmer par des exemples

[Le sujet](enonce.pdf)

[Quelques exemples](exemples)

### Consignes générales

  * Ce projet est à réaliser par groupe de 2 étudiants au plus
  * Date limite de rendu : A PRECISER (pas avant mai)
  * Soutenances de ce projet : A PRECISER (pas avant mai)
  * Pour réaliser ce projet et nous soumettre votre travail:
     - créez un "fork" du présent dépôt git `pfa-2022` par groupe
     - paramétrez votre dépôt pour qu'il soit privé
     - ajoutez `@letouzey` et `@geoffroy` parmi les membres (au moins reporter) de votre dépôt
     - réalisez votre programme dans le répertoire `projet` (détails ci-dessous)
     - utilisez régulièrement `git commit` et `git push` pour enregistrer votre travail.

### Consignes concernant votre programme

  * Votre projet devra utiliser l'outil `dune` pour la compilation et/ou fournir un fichier `Makefile` pour permettre la compilation via `make`. Détails à venir prochainement. 

  * Votre programme `genconcat` doit accepter les modes suivants de fonctionnement :

    - `genconcat <fichier>` doit afficher le programme Concat que vous avez produit à partir du contenu de `<fichier>`.
      Voir le répertoire `projet/exemples` pour des exemples de fichiers à traiter

    - `genconcat <fichier> <fichier>` doit créer un programme Concat à partir du contenu du premier fichier (contenant des lignes "input output") puis utiliser ce programme sur le second fichier. Celui-ci ne contiendra que des lignes "input", et vous devrez afficher chaque "output" calculé correspondant, un par ligne. Là encore, voir le répertoire `projet/exemples` pour des exemples de premiers et seconds fichiers à traiter.

  * Pour l'instant, la seule bibliothèque autorisée pour ce projet est la librairie standard d'OCaml. Vous pouvez éventuellement nous soumettre vos demandes (motivées) d'autorisation de bibliothèques externes, nous aviserons.

  * Le reste de l'organisation est libre (emplacement et découpage des fichiers sources par exemple).

  * La qualité du code produit (lisibilité, indentation, organisation, etc) sera évidemment prise en compte.
