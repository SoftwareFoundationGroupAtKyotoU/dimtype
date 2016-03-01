open Util

(* module Monomial = struct *)
(*   type t = Algebra.Vect.t *)

(* end *)

open Algebra

module P = Vect.Make(Vect.Num)(NumField)

type t = P.t

let of_list l = l
                |> List.map (fun (v, n) -> (Vect.Num.normalize v, n))
                |> P.of_list

let to_list = P.to_list

let vars p = List.map Vect.Num.bases (P.bases p)
             |> List.flatten
             |> List.sort_uniq compare
let eq = P.eq
let add = P.add
let sub = P.sub
let subst _ = assert false

let pp fmt v =
  let pp_powerset fmt v =
    Algebra.Vect.Num.pp
      ~pp_empty:(fun fmt () -> Format.fprintf fmt "")
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "")
      ~pp_pair:(fun fmt (v, p) -> Format.fprintf fmt "%a^%a" Id.pp v pp_num p)
      fmt v
  in
  P.pp
    ~pp_empty:(fun fmt () -> Format.fprintf fmt "0")
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ + ")
    ~pp_pair:(fun fmt (ps, c) -> Format.fprintf fmt "%a%a" pp_num c pp_powerset ps)
    fmt
    v

let to_string v = string_of pp v
