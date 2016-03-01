module type Setoid = sig
  type t
  val eq : t -> t -> bool
end

