open Scaffolding
open Popper

module MinPlus = struct
  type q = Q.t

  let pp_q = Q.pp_print
  let compare_q = Q.compare

  let q_sample =
    Sample.map (uncurry2 Q.of_ints)
      (Sample.both Sample.int
         (Sample.map (fun d -> if d == 0 then 1 else d) Sample.int))

  type t = Finite of q | Infinite [@@deriving show, ord, popper]

  let ( +: ) a b =
    match (a, b) with
    | Finite x, Finite y -> Finite (Q.max x y)
    | Finite _, _ -> a
    | _, Finite _ -> b
    | _, _ -> Infinite

  let zero = Infinite

  let ( *: ) a b =
    match (a, b) with
    | Finite x, Finite y -> Finite (Q.add x y)
    | _, _ -> Infinite

  let one = Finite Q.zero
  let gen = sample
end

let () =
  let module MP = SemiringLaws (MinPlus) (MinPlus) in
  run MP.tests
