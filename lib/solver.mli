(** Solver is a main module. *)

open Algebra

module Tenv : sig
  include Algebra.Environment.S with type dom = Id.t and type cod = Typ.t

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end

(** [Solver.constr] is a constraint on polynomials.
    - [Solver.Eq (x, p)] expresses that [p] should be dimensionally
      consistent and the type of [x] should be equal to the type of [p]
    - [Solver.Po p] expresses that [p] should be dimensionally
      consistent *)
type constr =
  | Eq of Id.t * Polynomial.t
  | Po of Polynomial.t

(** [Solver.infer ~heuristic cs] infers the type environment
    consistent with [cs].  This function returns two environments
    [tenv] and [ctenv], where [tenv] is a normal type environment and
    [ctenv] is a type environment for constants.  If [heuristic] is
    true this function applies a heuristic which tries to type program
    variables as non-dimensionless as possible (see comments in
    solver.ml for more details).  [heuristic] is true by default. *)
val infer : ?heuristic:bool -> constr list -> Tenv.t * Typ.t list

(** [Solver.enum_powersets ~max_degree (tenv, ctenv) typ] enumerates
    the powersets whose types are same as [typ] under the type
    environment [tenv] and the constant type environment [ctenv].
    The degrees of powersets don't exceed [max_degree]. *)
val enum_powersets : max_degree:int
                     -> (Tenv.t * Typ.t list)
                     -> Typ.t
                     -> Powerset.t list
