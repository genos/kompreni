open Kompreni

module LawTest (M : Monoid.Signature with type t = int) = struct
  include Monoid.Laws (M)

  let tests name =
    List.map
      (fun (n, p) ->
        QCheck_alcotest.to_alcotest
          (QCheck.Test.make ~count:1000 ~name:(name ^ " " ^ n) QCheck.int p))
      [ ("left id", left_id); ("right id", right_id) ]
end

let () =
  let module Sum = LawTest (struct
    type t = int

    let ( <+> ) = ( + )
    let empty = 0
  end) in
  let module Prod = LawTest (struct
    type t = int

    let ( <+> ) = ( * )
    let empty = 1
  end) in
  let module Max = LawTest (struct
    type t = int

    let ( <+> ) = max
    let empty = min_int
  end) in
  let module Min = LawTest (struct
    type t = int

    let ( <+> ) = min
    let empty = max_int
  end) in
  Alcotest.run "Monoid Laws, int examples"
    [
      ("sum", Sum.tests "sum");
      ("prod", Prod.tests "prod");
      ("max", Max.tests "max");
      ("min", Min.tests "min");
    ]
