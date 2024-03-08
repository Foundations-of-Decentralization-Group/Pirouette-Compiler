(lang dune 3.14)
(using menhir 3.0)

(name pirouette)

(generate_opam_files true)

(source
 (github Foundations-of-Decentralization-Group/Pirouette-Compiler))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name pirouette)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune ocamllex menhir ounit2 yojson)
 (tags
  (topics "to describe" your project))
 (allow_empty))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project