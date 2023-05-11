open Scaffolding

module MinPlus = struct
  type t = Finite of Q.t | Infinite

  let ( +& ) a b =
    match (a, b) with
    | Finite x, Finite y -> Finite (Q.max x y)
    | Finite _, _ -> a
    | _, Finite _ -> b
    | _, _ -> Infinite

  let zero = Infinite

  let ( *& ) a b =
    match (a, b) with
    | Finite x, Finite y -> Finite (Q.add x y)
    | _, _ -> Infinite

  let one = Finite Q.zero

  let gen =
    QCheck2.Gen.frequency
      [
        (1, QCheck2.Gen.pure Infinite);
        (1, QCheck2.Gen.map (fun x -> Finite (Q.of_float x)) QCheck2.Gen.float);
      ]
end

let () =
  let module MP = SemiringLaws (MinPlus) (MinPlus) in
  Alcotest.run "MinPlus Semiring Laws" [ ("MinPlus", MP.tests) ]
