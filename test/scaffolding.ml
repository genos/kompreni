open Kompreni
open Popper
open Sample.Syntax

let uncurry2 f (x, y) = f x y
let uncurry3 f (x, y, z) = f x y z
let pp2 pp_x pp_y out (x, y) = Format.fprintf out "(%a, %a)" pp_x x pp_y y

let pp3 pp_x pp_y pp_z out (x, y, z) =
  Format.fprintf out "(%a, %a, %a)" pp_x x pp_y y pp_z z

let make_test gen pp name prop =
  ( name,
    test @@ fun () ->
    let* e = Sample.(with_log name pp gen) in
    is_true (prop e) )

module type Testable = sig
  type t

  val gen : t Sample.t
  val pp : Format.formatter -> t -> unit
end

module SemigroupLaws (X : Testable) (S : Semigroup.Signature with type t = X.t) =
struct
  include Semigroup.Laws (S)

  let tests =
    suite
      [
        make_test
          Sample.Tuple.(tripple X.gen X.gen X.gen)
          (pp3 X.pp X.pp X.pp) "associative" (uncurry3 associative);
        make_test
          Sample.Tuple.(pair Sample.int X.gen)
          (pp2 Format.pp_print_int X.pp)
          "stimes ok" (uncurry2 stimes_ok);
      ]
end

module CommutativeSemigroupLaws
    (X : Testable)
    (S : Commutative_semigroup.Signature with type t = X.t) =
struct
  include Commutative_semigroup.Laws (S)

  let tests =
    let module SL = SemigroupLaws (X) (S) in
    suite
      [
        make_test
          Sample.Tuple.(pair X.gen X.gen)
          (pp2 X.pp X.pp) "commutative" (uncurry2 commutative);
        ("semigroup", SL.tests);
      ]
end

module MonoidLaws (X : Testable) (M : Monoid.Signature with type t = X.t) =
struct
  include Monoid.Laws (M)

  let tests =
    let module SL = SemigroupLaws (X) (M) in
    suite
      [
        make_test X.gen X.pp "left id" left_id;
        make_test X.gen X.pp "right id" right_id;
        ("semigroup", SL.tests);
      ]
end

module CommutativeMonoidLaws
    (X : Testable)
    (M : Commutative_monoid.Signature with type t = X.t) =
struct
  include Commutative_monoid.Laws (M)

  let tests =
    let module ML = MonoidLaws (X) (M) in
    let module CSL = CommutativeSemigroupLaws (X) (M) in
    suite [ ("monoid", ML.tests); ("commutative semigroup", CSL.tests) ]
end

module SemiringLaws (X : Testable) (S : Semiring.Signature with type t = X.t) =
struct
  include Semiring.Laws (S)

  let tests =
    let module CML = CommutativeMonoidLaws (X) (S) in
    suite
    @@ [ ("commutative monoid", CML.tests) ]
    @ List.map
        (uncurry2 (make_test X.gen X.pp))
        [
          ("times left one", times_left_one);
          ("times right one", times_right_one);
          ("zero annihilates left", zero_annihilates_left);
          ("zero annihilates right", zero_annihilates_right);
        ]
    @ List.map
        (uncurry2
           (make_test
              Sample.Tuple.(tripple X.gen X.gen X.gen)
              (pp3 X.pp X.pp X.pp)))
        [
          ("times associative", uncurry3 times_associative);
          ("left distributive", uncurry3 left_distributive);
          ("right distributive", uncurry3 right_distributive);
        ]
end
