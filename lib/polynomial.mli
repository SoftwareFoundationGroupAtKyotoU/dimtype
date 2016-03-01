open Algebra

type t

val of_list : (Vect.Num.t * Num.num) list -> t
val to_list : t -> (Vect.Num.t * Num.num) list

val vars : t -> Id.t list
val eq : t -> t -> bool
val add : t -> t -> t
val sub : t -> t -> t
val subst : t -> Id.t -> t -> t

val pp : Format.formatter -> t -> unit
val to_string : t -> string
