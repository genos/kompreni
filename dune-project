(lang dune 3.15)

(name kompreni)
(source (github genos/kompreni))
(license MIT)
(authors "Graham Enos <graham.enos@gmail.com>")
(maintainers "Graham Enos <graham.enos@gmail.com>")

(package
 (name kompreni)
 (synopsis "Understanding OCaml with abstract algebra examples")
 (depends
   (ocaml (and (>= 5.2.0) (< 6)))
   (popper (and :with-test (>= 0.1.1) (< 0.2)))
   (ppx_deriving (and :with-test (>= 6.0.2) (< 7)))
   (ppx_deriving_popper (and :with-test (>= 0.1.1) (< 0.2)))
   (zarith (and :with-test (>= 1.13) (< 2)))))
