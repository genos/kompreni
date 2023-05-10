open Scaffolding

module Gen = struct
  type t = int

  let gen = QCheck2.Gen.int
end

let () =
  let module Sum = CommutativeMonoidLaws (Gen) (Instances.Sum) in
  let module Prod = CommutativeMonoidLaws (Gen) (Instances.Prod) in
  let module Max = CommutativeMonoidLaws (Gen) (Instances.Max) in
  let module Min = CommutativeMonoidLaws (Gen) (Instances.Min) in
  let module Integers = SemiringLaws (Gen) (Instances.Integers) in
  Alcotest.run "Laws Int Examples"
    [
      ("Sum", Sum.tests);
      ("Prod", Prod.tests);
      ("Max", Max.tests);
      ("Min", Min.tests);
      ("Integers", Integers.tests);
    ]
