(lang dune 3.6)

(name Pirouette)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name Pirouette)
 (synopsis "A compiler for Pirouette code")
 (description "A compiler for Pirouette code")
 (depends ocaml dune menhir ocamllex ounit2 ppx_deriving))

(using menhir 2.0)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
