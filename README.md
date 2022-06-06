## Rendu IN213 - "LifeSim"

Ce projet implémente un langage de programmation permettant de simuler le comportement d'entités interagissant avec d'autres entités.

Il se base sur un langage PCF amélioré supportant:
- des structs "entités", possédant des propriétés
- des listes, ainsi qu'un opérateur de somme
- un opérateur `rand` pour de l'aléatoire
- la description de comportements sous trois formes: autonome, externe et externe filtrée
  - `Planete x <=> _ do...` -> autonome
  - `Planete x <=> Planete list L do...` -> externe
  - `Planete x <=> Planete list L if function x do...` -> externe filtrée

Et s'articule autour d'un moteur d'exécution, ou simulateur, permettant d'exécuter ces comportements et de générer un rendu graphique.