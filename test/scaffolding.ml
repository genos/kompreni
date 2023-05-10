open Kompreni

let uncurry2 f (x, y) = f x y
let uncurry3 f (x, y, z) = f x y z

let make_test gen name prop =
  QCheck_alcotest.to_alcotest (QCheck2.Test.make ~count:1000 ~name gen prop)

module type Testable = sig
  type t

  val gen : t QCheck2.Gen.t
end

module SemigroupLaws (X : Testable) (S : Semigroup.Signature with type t = X.t) =
struct
  include Semigroup.Laws (S)

  let tests =
    [
      make_test
        QCheck2.Gen.(triple X.gen X.gen X.gen)
        "associative" (uncurry3 associative);
    ]
end

module CommutativeSemigroupLaws
    (X : Testable)
    (S : Commutative_semigroup.Signature with type t = X.t) =
struct
  include Commutative_semigroup.Laws (S)

  let tests =
    let module SL = SemigroupLaws (X) (S) in
    List.cons
      (make_test
         QCheck2.Gen.(pair X.gen X.gen)
         "commutative" (uncurry2 commutative))
      SL.tests
end

module MonoidLaws (X : Testable) (M : Monoid.Signature with type t = X.t) =
struct
  include Monoid.Laws (M)

  let tests =
    let module SL = SemigroupLaws (X) (M) in
    List.map
      (uncurry2 (make_test X.gen))
      [ ("left id", left_id); ("right id", right_id) ]
    @ SL.tests
end

module CommutativeMonoidLaws
    (X : Testable)
    (M : Commutative_monoid.Signature with type t = X.t) =
struct
  include Commutative_monoid.Laws (M)

  let tests =
    let module ML = MonoidLaws (X) (M) in
    let module CSL = CommutativeSemigroupLaws (X) (M) in
    ML.tests @ CSL.tests
end

module SemiringLaws (X : Testable) (S : Semiring.Signature with type t = X.t) =
struct
  include Semiring.Laws (S)

  let tests =
    let module CML = CommutativeMonoidLaws (X) (S) in
    CML.tests
    @ List.map
        (uncurry2 (make_test X.gen))
        [
          ("times_left_one", times_left_one);
          ("times_right_one", times_right_one);
          ("zero_annihilates_left", zero_annihilates_left);
          ("zero_annihilates_right", zero_annihilates_right);
        ]
    @ List.map
        (uncurry2 (make_test QCheck2.Gen.(triple X.gen X.gen X.gen)))
        [
          ("times_associative", uncurry3 times_associative);
          ("left_distributive", uncurry3 left_distributive);
          ("right_distributive", uncurry3 right_distributive);
        ]
end
