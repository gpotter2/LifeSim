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

### Installation

Pour installer les dépendances (owl), utiliser la commande suivante à la racine du projet:
```sh
opam install . --deps-only
```

Le script `run.sh` utilise ffmpeg pour générer les vidéos, il faut donc également l'installer:
```
sudo apt install ffmpeg
```

## Execution

Le projet fournit un script `run.sh` permettant de:
- choisir l'un des 3 exemples de programmes fournis
- l'exécuter avec `dune exec LifeSim <prog>`
- convertir son résultat en une vidéo `out.mp4`, en utilisant `ffmpeg`

### Description du langage

Un fichier de programme doit comporter 3 éléments:
1. une description de structures, sous la forme d'une définition de structs. Ces structures sont des "entités" qui intéragissent entre elles

e.g.
```rs
// Définition de l'entité
struct Grain {
    vitesse: rand [1., 5.];
    x: rand [-200., 200.];
    y: rand [-200., 200.];
}
```

2. une description de comportements, sous l'une de ses 3 formes (autonome, externe ou externe filtrée). Il peut y avoir de nombreux comportements entre les entités que l'on veut. Un comportement définit une fonction qui modifie une entité, en fonction des autres entités (externe) possiblement filtrées (externe filtrées)

e.g.
```rs
// Comportement autonome
Grain x <=> _ do {
    let y = x.y - x.vitesse in
    let my = if y < -200. then y + 400. else y in  // modulo
    Grain {
        vitesse: x.vitesse;
        x: x.x;
        y: my;
    }
}
```

3. un élément `init` qui définie le nombre d'entités utilisées pour l'instanciation, point d'entrée du programme

e.g.
```rs
init {
    Grain: 100;
};
```

En exécutant ce programme, on obtient le résultat suivant:

https://user-images.githubusercontent.com/10530980/172335906-7c7afbf4-8cfd-40cb-8867-8dd3ed89aca7.mp4

Ce programme est extrèmement simple car les grains n'ont pas d'interactions entre eux. Les programmes de test fournis par la suite utilisent à l'inverse extensivement les interactions inter-entités.

## Programmes de test fournis

### Gravitation

Un simple programme de test implémentant 3 comportements, pour faire des planêtes de masse aléatoire qui rebondissent sur les bords du domaine, attirées par les forces de gravitation.

https://user-images.githubusercontent.com/10530980/172315766-9bdda0de-acee-47af-bbe3-a67a0d99029e.mp4

### Poule renard vipère

Un programme un peu plus compliqué implémentant 9 comportements pour décrire des groupes de poules (en rouge), renards (en vert) et vipères (en bleu).

https://user-images.githubusercontent.com/10530980/172316201-3ad3d3a6-1d24-4c42-8edd-e3dc43896a7f.mp4

### Vol d'oiseaux

Ce programme implémente 4 comportements nettement plus compliqués (alignement, séparation, cohésion), afin de répliquer l'exemple des [**Boids**](https://fr.wikipedia.org/wiki/Boids)), un exemple classique de simulation du comportement d'une nuée d'oiseaux.

https://user-images.githubusercontent.com/10530980/172316232-a8ad007f-bbf6-4767-943e-40ba46520445.mp4

