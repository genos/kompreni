module type Signature = sig
  type t

  val ( +: ) : t -> t -> t
end

module Utils (S : Signature) = struct
  open S

  let rec stimes (n : int) (x : S.t) : S.t option =
    match n with
    | n when n < 1 -> None
    | 1 -> Some x
    | _ ->
        let x' = stimes (n lsr 1) (x +: x) in
        if n land 1 = 0 then x' else Option.map (( +: ) x) x'
end

module Laws (S : Signature) = struct
  open S
  open Utils (S)

  let associative x y z = x +: (y +: z) = x +: y +: z

  let stimes_ok n x =
    let nx = stimes n x in
    Option.is_some nx = (n > 0)
end
