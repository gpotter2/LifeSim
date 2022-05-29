## Essain d'oiseaux

Programmer un essain d'oiseaux

- lexer
- parseur
- sémantique opérationelle

```
struct Oiseau {
    // Attributs par défaut: x et y
    vx: 0;
    vy: 0;
}

let oiseauPres a b = (distance a b) < 10 in
Oiseau x <=> Oiseau L if oiseauPres x L do {
    (Oiseau {}, Oiseau {})
    // Actions à effectuer, modifier le comportement.
    // L contient la liste des 2 plus proches oiseaux
}

init {
    Oiseau: <nombre>;
}
```