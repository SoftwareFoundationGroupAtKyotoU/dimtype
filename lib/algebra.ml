module type Setoid = sig
  type t
  val eq : t -> t -> bool
end

module Environment = struct
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

  (* The equivalence relation on elements of Codomain is required to
     compare environments. *)
  module Make (Dom : Setoid) (Cod : Setoid) = struct
    type dom = Dom.t
    type cod = Cod.t
    type t = (dom * cod) list

    let doms e = List.map fst e

    let cods e = List.map snd e

    let find k e = Util.assoc ~eq:Dom.eq k e

    let find_opt k e = Util.assoc_opt ~eq:Dom.eq k e

    let remove k e = Util.remove_assoc ~eq:Dom.eq k e

    let add_overwrite k v e =
      if Util.is_some (find_opt k e)
      then (k, v) :: (remove k e)
      else (k, v) :: e

    let mapv f e = List.map (fun (x, y) -> (x, f y)) e

    let filter f e = List.filter (fun (x, y) -> f (x, y)) e

    let filterv f e = List.filter (fun (_, y) -> f y) e

    let empty = []

    let is_empty = function
      | [] -> true
      | _  -> false

    let to_list e = e

    let of_list e = e

    let eq e1 e2 =
      let cmp d1 d2 =
        if Dom.eq d1 d2
        then 0
        else compare d1 d2
      in
      let open List in
      doms e1 @ doms e2
      |> sort_uniq cmp
      |> for_all (fun k -> Cod.eq (assoc k e1) (assoc k e2))

    let size = List.length

    let pp ~pp_sep ~pp_pair fmt e =
      Format.fprintf fmt "@[<2>%a@]" (Format.pp_print_list ~pp_sep pp_pair) e
  end

end

module type Field = sig
  include Setoid
  val zero : t
  val one  : t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t    
end

module NumField = struct
  open Num
  type t = num
  let zero = num_of_int 0
  let one = num_of_int 1
  let ( + ) = ( +/ )
  let ( - ) = ( -/ )
  let ( * ) = ( */ )
  let eq = eq_num
end

module Vect = struct
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
    val pp : pp_empty:(Format.formatter -> unit -> unit)
      -> pp_sep:(Format.formatter -> unit -> unit)
      -> pp_pair:(Format.formatter -> (basis * coeff) -> unit)
      -> Format.formatter
      -> t
      -> unit
  end

  module Make (Basis : Setoid) (Coeff : Field) = struct
    type basis = Basis.t
    type coeff = Coeff.t
    module E = Environment.Make(Basis)(Coeff)
    type t = E.t

    let to_list = E.to_list
    let of_list = E.of_list

    let bases v = E.doms v

    let unit b = E.(add_overwrite b Coeff.one empty)

    let is_empty = E.is_empty

    let empty = E.empty

    let eq v1 v2 = E.eq v1 v2

    let size = E.size

    let normalize v = E.filterv (fun c -> not Coeff.(eq c zero)) v

    let coeff b v =
      match E.find_opt b v with
      | Some c -> c
      | None   -> Coeff.zero

    let remove b v = E.remove b v

    let scalar n v = E.mapv (fun c -> Coeff.(c * n)) v |> normalize

    let inv v = E.mapv (fun c -> Coeff.(zero - c)) v

    let add v1 v2 =
      let vs = bases v1 @ bases v2 |> List.sort_uniq compare in
      List.map
        (fun v ->
           let c =
             match E.find_opt v v1, E.find_opt v v2 with
             | Some c1, Some c2 -> Coeff.(c1 + c2)
             | Some c1, None    -> c1
             | None   , Some c2 -> c2
             | None   , None    -> failwith "binary_operation"
           in
           v, c)
        vs
      |> E.of_list
      |> normalize

    let sub v1 v2 = add v1 (inv v2)

    let subst v b v' = add (remove b v) (scalar (coeff b v) v')

    let pp ~pp_empty ~pp_sep ~pp_pair fmt v =
      if E.is_empty v
      then pp_empty fmt ()
      else E.pp ~pp_sep ~pp_pair fmt v
  end

  module Num = Make(Id)(NumField)

  module Int = Make(Id)(struct
      type t = int
      let zero = 0
      let one = 1
      let ( + ) = ( + )
      let ( - ) = ( - )
      let ( * ) = ( * )
      let eq x y = compare x y = 0
    end)
end
