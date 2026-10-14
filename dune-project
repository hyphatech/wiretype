(lang dune 3.18)
(name wiretype)

(implicit_transitive_deps false)

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
  "JSON contracts for OCaml: one description validates, writes, and prints JSON Schema and Zod")
 (description
  "One value describes a JSON shape: it reads a document in one pass with every problem found, writes a value, checks the bounds it states, and prints itself as JSON Schema 2020-12 and as Zod, so a server, a browser and a model read one contract. Depends on Ptime, Mtime and Uuidm alone.")
 (depends
  (ocaml
   (>= 5.5))
  (ptime
   (>= 1.2.0))
  (mtime
   (>= 2.1.0))
  (uuidm
   (>= 0.9.10))
  (alcotest
   (and
    :with-test
    (>= 1.9.1)))))

(package
 (name ppx_wiretype)
 (synopsis "[@@deriving wiretype]: a type's wiretype description, derived")
 (description
  "Writes a type's description from records, enums, tagged unions, polymorphic variants, type parameters and recursive types, with bounds, defaults, docs and examples as attributes. What it writes is wiretype's public combinators.")
 (depends
  (ocaml
   (>= 5.5))
  (wiretype
   (= :version))
  (ppxlib
   (>= 0.38.0))
  (ptime
   (and
    :with-test
    (>= 1.2.0)))
  (mtime
   (and
    :with-test
    (>= 2.1.0)))
  (uuidm
   (and
    :with-test
    (>= 0.9.10)))
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
