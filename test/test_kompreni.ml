open Kompreni

module Base = struct
  let uncurry2 f (x, y) = f x y
  let uncurry3 f (x, y, z) = f x y z
  let count = 1000

  let make_test gen name prop =
    QCheck_alcotest.to_alcotest (QCheck.Test.make ~count ~name gen prop)
end

module SemigroupLaws (S : Semigroup.Signature with type t = int) = struct
  include Base
  include Semigroup.Laws (S)

  let tests =
    [
      make_test
        (QCheck.triple QCheck.int QCheck.int QCheck.int)
        "associative" (uncurry3 associative);
    ]
end

module CommutativeSemigroupLaws
    (S : Commutative_semigroup.Signature with type t = int) =
struct
  include Base
  include Commutative_semigroup.Laws (S)

  let tests =
    let module SL = SemigroupLaws (S) in
    List.cons
      (make_test
         (QCheck.pair QCheck.int QCheck.int)
         "commutative" (uncurry2 commutative))
      SL.tests
end

module MonoidLaws (M : Monoid.Signature with type t = int) = struct
  include Base
  include Monoid.Laws (M)

  let tests =
    let module SL = SemigroupLaws (M) in
    List.map
      (uncurry2 (make_test QCheck.int))
      [ ("left id", left_id); ("right id", right_id) ]
    @ SL.tests
end

module CommutativeMonoidLaws
    (M : Commutative_monoid.Signature with type t = int) =
struct
  include Base
  include Commutative_monoid.Laws (M)

  let tests =
    let module ML = MonoidLaws (M) in
    let module CSL = CommutativeSemigroupLaws (M) in
    ML.tests @ CSL.tests
end

let () =
  let module Sum = CommutativeMonoidLaws (Instances.Sum) in
  let module Prod = CommutativeMonoidLaws (Instances.Prod) in
  let module Max = CommutativeMonoidLaws (Instances.Max) in
  let module Min = CommutativeMonoidLaws (Instances.Min) in
  Alcotest.run "Laws Int Examples"
    [
      ("Sum", Sum.tests);
      ("Prod", Prod.tests);
      ("Max", Max.tests);
      ("Min", Min.tests);
    ]
