module type Signature = sig
  include Semigroup.Signature

  val empty : t
end

module Utils (M : Signature) = struct
  open M
  include Semigroup.Utils (M)

  let mtimes (n : int) (x : M.t) : M.t option =
    match n with n when n < 0 -> None | 0 -> Some empty | _ -> stimes n x
end

module Laws (M : Signature) = struct
  open M
  include Semigroup.Laws (M)

  let left_id x = empty <+> x = x
  let right_id x = x <+> empty = x
end
