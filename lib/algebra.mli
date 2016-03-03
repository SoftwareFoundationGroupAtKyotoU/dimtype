(** Setoid is a type equipped with an equivalence relation. *)
module type Setoid = sig
  type t
  val eq : t -> t -> bool
end

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

module type Field = sig
  include Setoid
  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
end

module NumField : Field with type t = Num.num

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
    (** [subst v k v'] substitute k to v' in v.
        (ex. subst (x + 2 * y) y (x + z) = 3 * x + 2 * z) *)
    val subst : t -> basis -> t -> t
    val mapv : (coeff -> coeff) -> t -> t
    val filter : (basis * coeff -> bool) -> t -> t
    val filterv : (coeff -> bool) -> t -> t
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

  val of_list : (Vect.Int.t * Num.num) list -> t
  val to_list : t -> (Vect.Int.t * Num.num) list

  val vars : t -> Id.t list
  val bases : t -> Vect.Int.t list
  val eq : t -> t -> bool
  val normalize : t -> t
  val zero : t
  val const : Num.num -> t
  val coeff : Vect.Int.t -> t -> Num.num
  val unit : Powerset.t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val pow : t -> int -> t
  val subst : t -> Id.t -> t -> t

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end
