(lang dune 3.21)

(name Mobil)

(generate_opam_files true)

(source
 (github Mr-Oregano/Mobil))

(authors Mr-Oregano)

(maintainers Mr-Oregano)

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 3.0)

(package
 (name Mobil)
 (synopsis "Coeffects for Code Mobility")
 (description "Using coeffects to model a type system for code mobility")
 (depends
  (ocaml
   (>= 5.3.0))
  (alcotest :with-test))
 (tags
  ("coeffects" "code mobility")))
