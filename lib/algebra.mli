(** Algebraic modules. *)

(** [Setoid] is a module equipped with an equivalence relation. *)
module type Setoid = sig
  type t
  val eq : t -> t -> bool
end

(** [Environment] is a basic module which stores mappings from [dom] values to
    [cod] values. *)
module Environment : sig
  module type S = sig
    type dom
    type cod
    type t

    val doms : t -> dom list
    val cods : t -> cod list
    val find : dom -> t -> cod
    val find_opt : dom -> t -> cod option
    val remove : dom -> t -> t
    val add_overwrite : dom -> cod -> t -> t
    val mapv : (cod -> cod) -> t -> t
    val filter : (dom * cod -> bool) -> t -> t
    val filterv : (cod -> bool) -> t -> t
    val empty : t
    val is_empty : t -> bool
    val to_list : t -> (dom * cod) list
    val of_list : (dom * cod) list -> t
    val eq : t -> t -> bool
    val size : t -> int

    val pp : pp_sep:(Format.formatter -> unit -> unit)
             -> pp_pair:(Format.formatter -> (dom * cod) -> unit)
             -> Format.formatter
             -> t
             -> unit
  end

  module Make (Dom : Setoid) (Cod : Setoid) : S with type dom = Dom.t
                                                 and type cod = Cod.t
end

(** [Field] is a mathematical structure equipped with +, *, -, inverse, 0, 1,
    but this signature don't require inverse function and the properties on
    these components. *)
module type Field = sig
  include Setoid
  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
end

(** [NumField] is the field of [Num.num]. *)
module NumField : Field with type t = Num.num

(** [Vect] is a module expresses a vector space. *)
module Vect : sig
  module type S = sig
    type basis
    type coeff
    type t

    val to_list : t -> (basis * coeff) list
    val of_list : (basis * coeff) list -> t
    val bases : t -> basis list
    val unit : basis -> t
    val is_empty : t -> bool
    val empty : t
    val eq : t -> t -> bool
    val size : t -> int
    val normalize : t -> t
    val coeff : basis -> t -> coeff
    val remove : basis -> t -> t
    val scalar : coeff -> t -> t
    val inv : t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val subst : t -> basis -> t -> t
    val mapv : (coeff -> coeff) -> t -> t
    val filter : (basis * coeff -> bool) -> t -> t
    val filterv : (coeff -> bool) -> t -> t

    (** [Algebra.Vect.eval f v] evaluates [v] using [f].

        {[
        open Algebra

        let f b =
          match Id.of_string b with
          | "x" -> Num.num_of_int 1
          | "y" -> Num.num_of_int 2
          | _   -> failwith "not_found"

        let v = Vect.of_list [(Id.of_string "x", 2); (Id.of_string "y", -3)]

        let () = assert (Vect.eval f v = Num.num_of_int (-4))
        ]}
    *)
    val eval : (basis -> coeff) -> t -> coeff
    val pp : pp_empty:(Format.formatter -> unit -> unit)
      -> pp_sep:(Format.formatter -> unit -> unit)
      -> pp_pair:(Format.formatter -> (basis * coeff) -> unit)
      -> Format.formatter
      -> t
      -> unit
  end

  module Make (Basis : Setoid) (C : Field) : S with type basis = Basis.t
                                                and type coeff = C.t

  module Num : S with type basis = Id.t
                  and type coeff = NumField.t

  module Int : S with type basis = Id.t
                  and type coeff = int
end

module Powerset : sig
  type t

  val to_list : t -> (Id.t * int) list
  val of_list : (Id.t * int) list -> t

  val vars : t -> Id.t list
  val zero : t
  val unit : Id.t -> t
  val mul : t -> t -> t
  val normalize : t -> t
  val remove : Id.t -> t -> t
  val eq : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end

module Polynomial : sig
  type t

  val of_list : (Powerset.t * Num.num) list -> t
  val to_list : t -> (Powerset.t * Num.num) list

  (** [Polynomial.make l] constructs the polynomial from [l].

      For example:
      {[
        (* 2xy^2 + x^2y *)
        Polynomial.make [ [("x", 1); ("y", 2)], Num.num_of_int 2
                        ; [("x", 2); ("y", 1)], Num.num_of_int 1
                        ]
      ]}
  *)
  val make : ((string * int) list * Num.num) list -> t

  (** [Polynomial.inspect p] returns the list expression of [p].
      This function is an inverse of [Polynomial.make]. *)
  val inspect : t -> ((string * int) list * Num.num) list

  val map : (Powerset.t * Num.num -> Powerset.t * Num.num) -> t -> t
  val vars : t -> Id.t list
  val bases : t -> Powerset.t list
  val eq : t -> t -> bool
  val normalize : t -> t
  val zero : t
  val const : Num.num -> t
  val coeff : Powerset.t -> t -> Num.num
  val unit : Powerset.t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val pow : t -> int -> t
  val subst : t -> Id.t -> t -> t

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end
