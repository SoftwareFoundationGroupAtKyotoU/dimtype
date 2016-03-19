open Util

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
      doms e1 @ doms e2
      |> Util.remove_duplicates ~eq:Dom.eq
      |> List.for_all
          (fun k ->
            try  Cod.eq (find k e1) (find k e2)
            with Not_found -> false)

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
    val mapv : (coeff -> coeff) -> t -> t
    val filter : (basis * coeff -> bool) -> t -> t
    val filterv : (coeff -> bool) -> t -> t
    val eval : (basis -> coeff) -> t -> coeff
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

    let bases v = E.doms v

    let unit b = E.(add_overwrite b Coeff.one empty)

    let is_empty = E.is_empty

    let empty = E.empty

    let size = E.size

    let normalize v = E.filterv (fun c -> not Coeff.(eq c zero)) v

    let to_list = E.to_list
    let of_list l = E.of_list l |> normalize

    let eq v1 v2 = E.eq (normalize v1) (normalize v2)

    let coeff b v =
      match E.find_opt b v with
      | Some c -> c
      | None   -> Coeff.zero

    let remove b v = E.remove b v

    let scalar n v = E.mapv (fun c -> Coeff.(c * n)) v |> normalize

    let inv v = E.mapv (fun c -> Coeff.(zero - c)) v

    let add v1 v2 =
      bases v1 @ bases v2
      |> Util.remove_duplicates ~eq:Basis.eq
      |> List.map
          (fun b ->
            let c =
              match E.find_opt b v1, E.find_opt b v2 with
              | Some c1, Some c2 -> Coeff.(c1 + c2)
              | Some c1, None    -> c1
              | None   , Some c2 -> c2
              | None   , None    -> failwith "Vect.add"
            in
            b, c)
      |> E.of_list
      |> normalize

    let sub v1 v2 = add v1 (inv v2)

    let subst v b v' = add (remove b v) (scalar (coeff b v) v')

    let mapv = E.mapv
    let filter = E.filter
    let filterv = E.filterv

    let eval f v =
      List.fold_left (fun acc (x, c) -> Coeff.(acc + f x * c))
        Coeff.zero
        v

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
      let eq = ( = )
    end)
end

module Powerset = struct
  type t = Vect.Int.t

  let to_list   = Vect.Int.to_list
  let of_list   = Vect.Int.of_list
  let vars      = Vect.Int.bases
  let zero      = Vect.Int.empty
  let unit      = Vect.Int.unit
  let exponent  = Vect.Int.coeff
  let mul       = Vect.Int.add
  let normalize = Vect.Int.normalize
  let remove    = Vect.Int.remove
  let eq        = Vect.Int.eq
  let pp fmt ps =
    Vect.Int.pp
      ~pp_empty:(fun fmt () -> Format.fprintf fmt "1")
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "*")
      ~pp_pair:(fun fmt (v, p) ->
        if p = 1
        then Format.fprintf fmt "%a" Id.pp v
        else Format.fprintf fmt "%a^%a" Id.pp v pp_int p)
      fmt ps

  let to_string = string_of pp
end

module Polynomial = struct
  module P = Vect.Make(Powerset)(NumField)

  type t = P.t

  let of_list l = l
           |> List.map (fun (v, n) -> (Powerset.normalize v, n))
           |> P.of_list
           |> P.normalize

  let to_list = P.to_list

  let make l =
    let powersets =
      List.map (fun (p, c) ->
        (Powerset.of_list (List.map (fun (x, i) -> (Id.of_string x, i)) p), c))
        l
    in
    of_list powersets

  let inspect (p : t) =
    List.map (fun (ps, c) ->
      List.map (fun (x, i) -> Id.to_string x, i) (Powerset.to_list ps), c)
      (to_list p)

  let map f p = List.map f (of_list p) |> to_list |> P.normalize

  let vars p = List.map Powerset.vars (P.bases p)
           |> List.flatten
           |> List.sort_uniq compare

  let bases = P.bases
  let eq = P.eq
  let normalize = P.normalize
  let zero = P.empty
  let const c = P.of_list [Powerset.zero, c]
  let coeff = P.coeff
  let unit = P.unit
  let scalar = P.scalar
  let add = P.add
  let sub = P.sub

  let mul p1 p2 =
    let bs1 = P.bases p1
    and bs2 = P.bases p2
    in
    let all_pairs = List.(flatten (map (fun b1 -> map (fun b2 -> (b1, b2)) bs2) bs1))
    in
    List.fold_left
      (fun acc (b1, b2) ->
        let b = Powerset.mul b1 b2
        and c = Num.(P.coeff b1 p1 */ P.coeff b2 p2)
        in
        P.add (P.of_list [b, c]) acc)
      zero
      all_pairs
    |> normalize
      
  let pow p n =
    let rec loop acc = function
      | 0 -> acc
      | n -> loop (mul p acc) (n - 1)
    in
    loop (const (Num.num_of_int 1)) n

  let subst p x p' =
    let powerset_subst ps x q =
      let c = Powerset.exponent x ps in
      mul (unit (Powerset.remove x ps)) (pow q c)
    in
    let p = to_list p in
    let l = List.map (fun (ps, c) -> powerset_subst ps x p', c) p in
    List.fold_left (fun acc (p, c) -> add acc (scalar c p)) zero l

  let pp fmt v =
    P.pp
      ~pp_empty:(fun fmt () -> Format.fprintf fmt "0")
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ + ")
      ~pp_pair:(fun fmt (ps, c) -> Format.fprintf fmt "%a%a" pp_num c Powerset.pp ps)
      fmt
      v

  let to_string v = string_of pp v
end
