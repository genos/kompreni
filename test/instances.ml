module Sum = struct
  type t = int

  let ( +& ) = ( + )
  let zero = 0
end

module Prod = struct
  type t = int

  let ( +& ) = ( * )
  let zero = 1
end

module Max = struct
  type t = int

  let ( +& ) = max
  let zero = min_int
end

module Min = struct
  type t = int

  let ( +& ) = min
  let zero = max_int
end

module Integers = struct
  type t = int

  let ( +& ) = ( + )
  let zero = 0
  let ( *& ) = ( * )
  let one = 1
end

module MinPlus = struct
  type t = Finite of float | Infinite

  let ( +& ) a b = function
    | Finite x, Finite y -> Finite (Float.max x y)
    | Finite _, _ -> a
    | _, Finite _ -> b
    | _, _ -> Infinite

  let zero = Infinite

  let ( *& ) = function
    | Finite x, Finite y -> Finite (x +. y)
    | _, _ -> Infinite

  let one = Finite 0.0
end
