## Rendu IN213 - "LifeSim"

Ce projet implémente un langage de programmation interprété permettant de simuler le comportement d'entités interagissant avec d'autres entités.

Il se base sur un langage PCF amélioré supportant:
- des structs "entités", possédant des propriétés
- des listes, ainsi qu'un opérateur de somme
- un opérateur `rand` pour de l'aléatoire
- la description de comportements sous trois formes: autonome, externe et externe filtrée
  - `Planete x <=> _ do...` -> autonome
  - `Planete x <=> Planete list L do...` -> externe
  - `Planete x <=> Planete list L if function x do...` -> externe filtrée

Et s'articule autour d'un moteur d'exécution, ou simulateur, permettant d'exécuter ces comportements et de générer un rendu graphique.

## Execution

Le projet fournit un script `run.sh` permettant de:
- choisir l'un des 3 exemples de programmes fournis
- l'exécuter avec `dune exec LifeSim <prog>`
- convertir son résultat en une vidéo `out.mp4`, en utilisant `ffmpeg`

## Programmes de test fournis

### Gravitation

Un simple programme de test implémentant des planêtes de masse aléatoire qui rebondissent sur les bords du domaine, attirées par les forces de gravitation.

![](examples/gravitation.mp4)
