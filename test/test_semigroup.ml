open Kompreni

module LawTest (S : Semigroup.Signature with type t = int) = struct
  include Semigroup.Laws (S)

  let tests =
    [
      QCheck_alcotest.to_alcotest
        (QCheck.Test.make ~count:1000 ~name:"associative"
           (QCheck.triple QCheck.int QCheck.int QCheck.int) (fun (x, y, z) ->
             associative x y z));
    ]
end

let () =
  let module Sum = LawTest (Instances.Sum) in
  let module Prod = LawTest (Instances.Prod) in
  let module Max = LawTest (Instances.Max) in
  let module Min = LawTest (Instances.Min) in
  Alcotest.run "Semigroup Laws Int Examples"
    [
      ("sum", Sum.tests);
      ("prod", Prod.tests);
      ("max", Max.tests);
      ("min", Min.tests);
    ]
