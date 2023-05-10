open Scaffolding

module MinPlus = struct
  type t = Finite of float | Infinite

  let ( +& ) a b =
    match (a, b) with
    | Finite x, Finite y -> Finite (Float.max x y)
    | Finite _, _ -> a
    | _, Finite _ -> b
    | _, _ -> Infinite

  let zero = Infinite

  let ( *& ) a b =
    match (a, b) with Finite x, Finite y -> Finite (x +. y) | _, _ -> Infinite

  let one = Finite 0.0

  let gen =
    QCheck2.Gen.frequency
      [
        (1, QCheck2.Gen.pure Infinite);
        (1, QCheck2.Gen.map (fun x -> Finite x) QCheck2.Gen.float);
      ]
end

let () =
  let module MP = SemiringLaws (MinPlus) (MinPlus) in
  Alcotest.run "Laws MinPlus Examples" [ ("MinPlus", MP.tests) ]
