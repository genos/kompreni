(lang dune 3.7)

(name kompreni)
(maintainers "Graham Enos <graham.enos@gmail.com>")

(generate_opam_files true)

(package
 (name kompreni)
 (depends
   (ocaml (>= 5.0.0))
   (odoc (and :with-doc (>= 2.2.0)))
   (alcotest (and :with-test (>= 1.7.0)))
   (qcheck (and :with-test (>= 0.20)))
   (qcheck-alcotest (and :with-test (>= 0.20)))))
