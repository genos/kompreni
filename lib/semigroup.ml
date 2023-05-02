module type Signature = sig
  type t

  val ( <+> ) : t -> t -> t
end

module Utils (S : Signature) = struct
  open S

  let rec stimes n x =
    match n with
    | n when n < 1 -> None
    | 1 -> Some x
    | _ -> (
        let n' = n / 2 in
        let x' = stimes n' (x <+> x) in
        match n mod 2 with 0 -> x' | _ -> Option.map (( <+> ) x) x')
end

module Laws (S : Signature) = struct
  open S

  let assoc x y z = x <+> (y <+> z) = (x <+> y <+> z)
end
