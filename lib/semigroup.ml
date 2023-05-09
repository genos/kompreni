module type Signature = sig
  type t

  val ( +& ) : t -> t -> t
end

module Utils (S : Signature) = struct
  open S

  let rec stimes n x =
    match n with
    | n when n < 1 -> None
    | 1 -> Some x
    | _ ->
        let x' = stimes (n / 2) (x +& x) in
        if n mod 2 = 0 then x' else Option.map (( +& ) x) x'
end

module Laws (S : Signature) = struct
  open S

  let associative x y z = x +& (y +& z) = (x +& y +& z)
end
