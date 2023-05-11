open Scaffolding

module Gen = struct
  type t = int

  let gen = QCheck2.Gen.int
end

module Sum = struct
  type t = int

  let ( +& ) = ( + )
  let zero = 0
end

module Prod = struct
  type t = int

  let ( +& ) = ( * )
  let zero = 1
end

module Max = struct
  type t = int

  let ( +& ) = max
  let zero = min_int
end

module Min = struct
  type t = int

  let ( +& ) = min
  let zero = max_int
end

module Integers = struct
  type t = int

  let ( +& ) = ( + )
  let zero = 0
  let ( *& ) = ( * )
  let one = 1
end

let () =
  let module Sum = CommutativeMonoidLaws (Gen) (Sum) in
  let module Prod = CommutativeMonoidLaws (Gen) (Prod) in
  let module Max = CommutativeMonoidLaws (Gen) (Max) in
  let module Min = CommutativeMonoidLaws (Gen) (Min) in
  let module Integers = SemiringLaws (Gen) (Integers) in
  Alcotest.run "Variious Int Law Examples"
    [
      ("Sum", Sum.tests);
      ("Prod", Prod.tests);
      ("Max", Max.tests);
      ("Min", Min.tests);
      ("Integers", Integers.tests);
    ]
