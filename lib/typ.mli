include (module type of Algebra.Vect.Num)

val of_vect : Algebra.Vect.Num.t -> t
val to_vect : t -> Algebra.Vect.Num.t

val pp : ?logarithm:bool -> Format.formatter -> t -> unit
