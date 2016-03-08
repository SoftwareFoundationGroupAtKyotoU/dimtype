(** Dimtype is a main module. *)

open Algebra

module Tenv : sig
  include Algebra.Environment.S

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end

(** [constr] is a constraint on polynomials.
    - [Eq (x, p)] expresses that [p] should be dimensionally consistent
      and the type of [x] should be equal to the type of [p]
    - [Po p] expresses that [p] should be dimensionally consistent *)
type constr =
  | Eq of Id.t * Polynomial.t
  | Po of Polynomial.t

(** [Infer.infer cs] infers the type environment consistent with [cs].
    This function returns two environments [tenv] and [ctenv], where [tenv]
    is a normal type environment and [ctenv] is a type environment for
    constants. *)
val infer : constr list -> Tenv.t * Typ.t list
