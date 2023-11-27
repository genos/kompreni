module type Signature = sig
  include Semigroup.Signature
end

module Utils (S : Signature) = struct
  include Semigroup.Utils (S)
end

module Laws (S : Signature) = struct
  open S
  include Semigroup.Laws (S)

  let commutative x y = x +: y = y +: x
end
