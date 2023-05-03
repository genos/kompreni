open Kompreni

module LawTest (M : Monoid.Signature with type t = int) = struct
  include Monoid.Laws (M)

  let tests =
    List.map
      (fun (name, prop) ->
        QCheck_alcotest.to_alcotest
          (QCheck.Test.make ~count:1000 ~name QCheck.int prop))
      [ ("left id", left_id); ("right id", right_id) ]
end

let () =
  let module Sum = LawTest (Instances.Sum) in
  let module Prod = LawTest (Instances.Prod) in
  let module Max = LawTest (Instances.Max) in
  let module Min = LawTest (Instances.Min) in
  Alcotest.run "Monoid Laws Int Examples"
    [
      ("sum", Sum.tests);
      ("prod", Prod.tests);
      ("max", Max.tests);
      ("min", Min.tests);
    ]
