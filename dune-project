(lang dune 3.0)

(name Vol)

(generate_opam_files true)

(authors "Gabriel Potter")

(maintainers "Gabriel Potter")

(license LICENSE)

(package
 (name Vol)
 (synopsis "Vol")
 (description
  "Un langage de description et de simulation de comportements de groupes d'entitees")
 (depends ocaml dune owl owl-plplot))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
