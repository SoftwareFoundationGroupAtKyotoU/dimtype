open Util
open Algebra

module Tenv = struct
  include Environment.Make(Id)(Typ)

  let pp = pp
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
    ~pp_pair:(fun fmt (x, typ) ->
      Format.fprintf fmt "%a : %a" Id.pp x (Typ.pp ~logarithm:false) typ)

  let to_string = string_of pp
end

type constr =
  | Eq of Id.t * Polynomial.t
  | Po of Polynomial.t

type tyconstr = Vect.Num.t

let tyconstr_of_constr (cs : constr list) (tenv : Tenv.t) : tyconstr list =

  let tyconstr_of_polynomial (p : Polynomial.t) : tyconstr list =
    let typ_of_powerset (ps : Powerset.t) : Typ.t =
      List.fold_left
        (fun acc (x, c) -> Typ.(add acc (scalar (ni c) (Tenv.find x tenv))))
        Typ.empty
        (Powerset.to_list ps)
    in
    let typs =
      List.map
        (fun (ps, _) -> typ_of_powerset ps)
        (Polynomial.to_list p)
    in
    match typs with
    | [] | [_] -> []
    | x :: rest -> List.map (fun typ -> Typ.(to_vect (sub typ x))) rest
  in

  cs
  |> List.map
      (function
      | Eq (x, p) -> tyconstr_of_polynomial Polynomial.(add (unit (Powerset.unit x)) p)
      | Po p      -> tyconstr_of_polynomial p)
  |> List.flatten

let solve_tyconstr (eqns : tyconstr list) : (Id.t * Vect.Num.t) list =
  let rec solve sol eqns =
    match eqns with
    | []          -> sol
    | eqn :: rest ->
      begin
        match Vect.Num.to_list eqn with
        | []          -> solve sol rest
        | (x, c) :: v ->
          let v = Vect.Num.(scalar Num.(ni (-1) // c) (of_list v)) in
          let sol = List.map (fun (y, v') -> y, Vect.Num.subst v' x v) sol in
          let rest = List.map (fun v' -> Vect.Num.subst v' x v) rest in
          solve ((x, v) :: sol) rest
      end
  in
  solve [] eqns

let infer (cs : constr list) : Tenv.t =

  let cs =
    (* auxiliary variables used only in type inference *)
    let avar_id = Id.unique ~prefix:"inf" in
    let append_aux_tvars p =
      Polynomial.map
        (fun (ps, c) -> (Powerset.(mul ps (avar_id () |> unit)), c))
        p
    in
    List.map (function
    | Eq (x, p) -> Eq (x, append_aux_tvars p)
    | Po p      -> Po (append_aux_tvars p)
    ) cs
  in

  let vars =
    List.(map (function Eq (_, p) | Po p -> Polynomial.vars p) cs |> flatten)
  in
  let tvar_id = Id.unique ~prefix:"ty" in
  let tenv = vars
             |> List.map (fun v -> (v, Typ.unit @@ tvar_id ()))
             |> Tenv.of_list
  in
  let sol = solve_tyconstr (tyconstr_of_constr cs tenv) in

  (* List.iter *)
  (*   (fun (x, v) -> *)
  (*     let open Format in *)
  (*     fprintf std_formatter "%a : %a\n" *)
  (*       Id.pp x *)
  (*       (Vect.Num.pp *)
  (*          ~pp_empty:(fun fmt () -> fprintf fmt "1") *)
  (*          ~pp_sep:(fun fmt () -> fprintf fmt "*") *)
  (*          ~pp_pair:(fun fmt (x, c) -> fprintf fmt "%a^%a" Id.pp x pp_num c)) *)
  (*       v) *)
  (*   sol; *)

  Tenv.mapv
    (fun typ ->
      List.fold_left
        (fun acc (x, v) -> Vect.Num.subst acc x v)
        (Typ.to_vect typ)
        sol
      |> Typ.of_vect)
    tenv

let () =
  let x = Id.of_string "x"
  and y = Id.of_string "y"
  and z = Id.of_string "z"
  in
  (* x^2y + xz^2 *)
  let p1 = Polynomial.of_list
    [ Powerset.of_list [ (x, 2); (y, 1) ], ni 2
    ; Powerset.of_list [ (x, 1); (z, 2) ], ni 3
    ]
  in
  print_endline (Tenv.to_string (infer [Po p1]))

(*
open Imp.Syntax
open Normalize
open Util

let uid = unique_name "inf"

let to_list aexp =
  let rec loop acc = function
    | BinOp ((Add | Sub), a1, a2) -> loop (a2 :: acc) a1
    | a -> a :: acc
  in
  loop [] aexp

let normalize a =
  expand_aexp a
  |> map_tree (fun m -> BinOp (Mul, Var (Id.of_string @@ uid ()), m))
  |> opt_zero
  |> opt_one

(* Normalize and multiply parameters to each monomial. *)
let rec preproc = function
  | Subst substs      ->
    Subst (List.map (fun (v, p) -> (v, normalize p)) substs)
  | Seq   (s1, s2)    -> Seq (preproc s1, preproc s2)
  | If    (p, s1, s2) ->
    let p =
      match p with
      | EqZ  p -> EqZ (normalize p)
      | NEqZ p -> NEqZ (normalize p)
      | NDet   -> NDet
    in
    If (p, preproc s1, preproc s2)
  | While c           -> While (preproc c)
  | Skip              -> Skip

let equation_of_aexp tenv a =
  let rec loop tenv = function
    | Var x              -> Tenv.find x tenv, []
    | (Int _ | Float _)  -> Typ.empty, []
    | BinOp (op, a1, a2) ->
      let (t1, c1) = loop tenv a1
      and (t2, c2) = loop tenv a2
      in
      (match op with
       | (Add | Sub) -> t1, Typ.sub t1 t2 :: c1 @ c2
       | Mul -> Typ.add t1 t2, c1 @ c2
       | Div -> Typ.sub t1 t2, c1 @ c2
       | Pow ->
         (match a2 with
          | Float f -> Typ.pown t1 (num_of_float f), c1 @ c2
          | Int   i -> Typ.powi t1 i, c1 @ c2
          | (Var _ | BinOp _) -> failwith "equation_of_aexp"))
  in
  snd (loop tenv a)

let rec equation_of_command tenv = function
  | Subst substs      ->
    let open List in
    filter (fun (_, p) -> not (iszero p)) substs
    |> map (fun (v, p) -> equation_of_aexp tenv (BinOp (Sub, p, Var v)))
    |> concat
  | Seq   (s1, s2)    -> equation_of_command tenv s1 @ equation_of_command tenv s2
  | If    (p, s1, s2) ->
    let p =
      match p with
      | EqZ  p -> equation_of_aexp tenv p
      | NEqZ p -> equation_of_aexp tenv p
      | NDet   -> []
    in
    p @
    equation_of_command tenv s1 @
    equation_of_command tenv s2
  | While c           -> equation_of_command tenv c
  | Skip              -> []

(* A type variable is insignificant iff even if it is substituted
   for dimensionless, any program variable is still not dimensionless.

   In the following type environment, A or B are insignificant:

   x : A * B, y : A * B

   In the following type environment, no type variables are insignificant:

   x : A, y : B * C, z : B
*)
let crush_insignificant_tvars1 tenv vars =
  let insignificant tenv tv =
    List.for_all
      (fun v -> Typ.remove tv (Tenv.find v tenv) <> Typ.empty)
      vars
  in
  let tvars =
    List.(map Typ.collect_tvars (Tenv.typs tenv)
          |> concat
          |> sort_uniq compare)
  in
  List.fold_left
    (fun tenv tv ->
      if insignificant tenv tv
      then Tenv.remove_tv tv tenv
      else tenv)
    tenv
    tvars

let crush_insignificant_tvars2 tenv vars =
  (* Return whether it is ok to substitute [dimvar] by [ty]. *)
  let insignificant tenv dimvar ty =
    let tenv' = Tenv.subst_ty dimvar ty tenv in
    if List.for_all (fun v -> Tenv.find v tenv' <> Typ.empty) vars
    then Some tenv'
    else None
  in
  let rec loop tenv = function
    | []         -> tenv
    | ty :: rest ->
       match Vect.to_list (Typ.to_vect ty) with
       | []          -> loop tenv rest
       | (v, d) :: r ->
          let ty' = Vect.(scalar Num.(ni (-1) // d) (of_list r)) |> Typ.of_vect in
          match insignificant tenv v ty' with
          | Some tenv' -> loop tenv' (List.map (fun t -> Typ.subst t v ty') rest)
          | None       -> loop tenv rest
  in
  loop tenv (Tenv.typs tenv)

let infer c =
  let uid = unique_name "ty" in
  let program_vars = collect_symbols c in
  let c = preproc c in
  let vars = collect_symbols c in
  let aux_vars = List.filter (fun v -> not (List.mem v program_vars)) vars in
  let tenv =
    List.map (fun v -> (v, Typ.unit (Id.of_string (uid ())))) vars
    |> Tenv.of_list
  in

  let eqns = equation_of_command tenv c in
  let sol =
    Linear_alg.solve (List.map Typ.to_vect eqns)
    |> List.map (fun (v, t) -> (v, Typ.of_vect t))
  in
  let tenv =
    Tenv.mapv
      (fun t -> List.fold_left (fun acc (v, t') -> Typ.subst acc v t') t sol)
      tenv
  in

  let tenv = crush_insignificant_tvars1 tenv program_vars in
  let tenv = crush_insignificant_tvars2 tenv program_vars in

  (* Substitute auxiliary variables by 1. *)
  let c =
    List.fold_left (fun c v -> subst_command v (Int 1) c) c aux_vars
  in
  let (vtenv, ctenv) =
    let aux_vars_typs =
      Tenv.(filter (fun (v, _) -> List.mem v aux_vars) tenv
            |> typs)
    in
    let ctyps = remove_duplicates aux_vars_typs
                |> List.filter (fun ty -> not (Typ.is_empty ty))
    in
    (Tenv.filter (fun (v, _) -> not (List.mem v aux_vars)) tenv,
     Tenv.of_list (List.map (fun t -> (Num.num_of_int 1, t)) ctyps))
  in

  (vtenv, ctenv, map_aexp opt_one c)
*)

