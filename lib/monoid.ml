module type Signature = sig
  include Semigroup.Signature

  val zero : t
end

module Utils (M : Signature) = struct
  open M
  include Semigroup.Utils (M)

  let mtimes (n : int) (x : M.t) : M.t =
    match n with n when n < 1 -> zero | _ -> Option.get (stimes n x)
end

module Laws (M : Signature) = struct
  open M
  open Utils (M)
  include Semigroup.Laws (M)

  let left_id x = zero +: x = x
  let right_id x = x +: zero = x
end
