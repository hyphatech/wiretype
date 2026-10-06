(lang dune 3.18)
(name wiretype)

(generate_opam_files true)

(maintainers "Hypha Technologies Ltd <dev@hyphatech.io>")

(authors "Hypha Technologies Ltd")

(license MIT)

(maintenance_intent "(latest)")

(source
 (github hyphatech/wiretype))

(package
 (name wiretype)
 (synopsis
  "JSON contracts for OCaml: one description validates, writes, and prints JSON Schema and zod")
 (description
  "One value describes a JSON shape: it reads a document in one pass with every problem found, writes a value, checks the bounds it states, and prints itself as JSON Schema 2020-12 and as zod, so a server, a browser and a model read one contract. Depends on nothing.")
 (depends
  (ocaml
   (>= 5.4))
  (alcotest
   (and
    :with-test
    (>= 1.9.1)))
  (odoc
   (and
    :with-dev-setup
    (>= 3.2.1)))
  (merlin
   (and
    :with-dev-setup
    (>= 5.6-504)))
  (ocaml-lsp-server
   (and
    :with-dev-setup
    (>= 1.24.0)))
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.29.0)))))

(package
 (name ppx_wiretype)
 (synopsis "[@@deriving wiretype]: a type's wiretype description, derived")
 (description
  "Writes a type's description from records, enums, tagged unions, polymorphic variants, type parameters and recursive types, with bounds, defaults, docs and examples as attributes. What it writes is wiretype's public combinators.")
 (depends
  (ocaml
   (>= 5.4))
  (wiretype
   (= :version))
  (ppxlib
   (>= 0.38.0))
  (yojson
   (and
    :with-test
    (>= 3.0.0)))
  (alcotest
   (and
    :with-test
    (>= 1.9.1)))
  (qcheck-core
   (and
    :with-test
    (>= 0.91)))
  (qcheck-alcotest
   (and
    :with-test
    (>= 0.91)))))
