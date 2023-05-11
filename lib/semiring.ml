module type Signature = sig
  include Commutative_monoid.Signature

  val one : t
  val ( *& ) : t -> t -> t
end

module Utils (S : Signature) = struct
  include Commutative_monoid.Utils (S)
end

module Laws (S : Signature) = struct
  open S
  include Commutative_monoid.Laws (S)

  (* (S, 1, *&) is also a Monoid *)
  let times_associative x y z = x *& (y *& z) = x *& y *& z
  let times_left_one x = one *& x = x
  let times_right_one x = x *& one = x

  (* Zero annihilates *)
  let zero_annihilates_left x = zero *& x = zero
  let zero_annihilates_right x = x *& zero = zero

  (* Distributivity *)
  let left_distributive x y z = x *& (y +& z) = (x *& y) +& (x *& z)
  let right_distributive x y z = (x +& y) *& z = (x *& z) +& (y *& z)
end
