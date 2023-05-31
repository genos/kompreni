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
   (alcotest (and :with-test (>= 1.7.0)))
   (qcheck (and :with-test (>= 0.21.1)))
   (qcheck-alcotest (and :with-test (>= 0.21.1)))
   (zarith (and :with-test (>= 1.12)))))
