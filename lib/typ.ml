open Util

include Algebra.Vect.Num

let pp ?(logarithm=false) fmt typ =
  if logarithm
  then pp
         ~pp_empty:(fun fmt () -> Format.fprintf fmt "0")
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " +@ ")
         ~pp_pair:(fun fmt (k, v) ->
           Format.fprintf fmt "@[<2>%a@ * %a@]" Id.pp k pp_num v)
         fmt
         typ
  else pp
         ~pp_empty:(fun fmt () -> Format.fprintf fmt "1")
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " *@ ")
         ~pp_pair:(fun fmt (k, v) ->
           Format.fprintf fmt "%a^%a" Id.pp k pp_num v)
         fmt
         typ
