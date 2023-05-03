module Sum = struct
  type t = int

  let ( <+> ) = ( + )
  let empty = 0
end

module Prod = struct
  type t = int

  let ( <+> ) = ( * )
  let empty = 1
end

module Max = struct
  type t = int

  let ( <+> ) = max
  let empty = min_int
end

module Min = struct
  type t = int

  let ( <+> ) = min
  let empty = max_int
end
