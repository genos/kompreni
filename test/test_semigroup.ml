open Kompreni

module LawTest (S : Semigroup.Signature with type t = int) = struct
  include Semigroup.Laws (S)

  let tests name =
    [
      QCheck_alcotest.to_alcotest
        (QCheck.Test.make ~count:1000
           ~name:(name ^ " " ^ "assoc")
           (QCheck.triple QCheck.int QCheck.int QCheck.int)
           (fun (x, y, z) -> assoc x y z));
    ]
end

let () =
  let module Sum = LawTest (struct
    type t = int

    let ( <+> ) = ( + )
  end) in
  let module Prod = LawTest (struct
    type t = int

    let ( <+> ) = ( * )
  end) in
  let module Max = LawTest (struct
    type t = int

    let ( <+> ) = max
  end) in
  let module Min = LawTest (struct
    type t = int

    let ( <+> ) = min
  end) in
  Alcotest.run "Semigroup Laws, int examples"
    [
      ("sum", Sum.tests "sum");
      ("prod", Prod.tests "prod");
      ("max", Max.tests "max");
      ("min", Min.tests "min");
    ]
