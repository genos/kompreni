module type Signature = sig
  include Commutative_semigroup.Signature
  include Monoid.Signature
end


module Laws (M: Signature) = struct
  include Monoid.Laws(M)
  include Commutative_semigroup.Laws(M)
end
