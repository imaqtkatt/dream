(lang dune 3.10)

(name dream)

(generate_opam_files true)

(using menhir 2.1)

(package
 (name dream)
 (depends
  (ocaml (= 5.1.0))
  (dune (>= 3.10))
  (ppx_deriving (= 5.2.1))
  menhir))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
