(lang dune 3.8)

(name kompreni)
(source (github genos/kompreni))
(license MIT)
(authors "Graham Enos <graham.enos@gmail.com>")
(maintainers "Graham Enos <graham.enos@gmail.com>")
(generate_opam_files true)

(package
 (name kompreni)
 (synopsis "Understanding OCaml with abstract algebra examples")
 (depends
   (ocaml (>= 5.0.0))
   (ppx_deriving (and :with-test (>= 5.2.1)))
   (ppx_deriving_popper (and :with-test (>= 0.1.1)))
   (zarith (and :with-test (>= 1.12)))))
