open Scaffolding

module Gen = struct
  open Instances

  type t = MinPlus.t

  let gen =
    QCheck2.Gen.frequency
      [
        (1, QCheck2.Gen.pure MinPlus.Infinite);
        (1, QCheck2.Gen.map (fun x -> MinPlus.Finite x) QCheck2.Gen.float);
      ]
end

let () =
  let module MP = SemiringLaws (Gen) (Instances.MinPlus) in
  Alcotest.run "Laws MinPlus Examples" [ ("MinPlus", MP.tests) ]
