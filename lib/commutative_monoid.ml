module type Signature = sig
  include Commutative_semigroup.Signature
  include Monoid.Signature
end

module Utils (M : Signature) = struct
  include Monoid.Utils (M)
end

module Laws (M : Signature) = struct
  include Monoid.Laws (M)
  include Commutative_semigroup.Laws (M)
end
