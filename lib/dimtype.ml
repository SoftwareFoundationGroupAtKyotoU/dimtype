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

let vars_of_constraints cs =
  let vars_of_constr = function
    | Eq (x, p) -> x :: Polynomial.vars p
    | Po p      -> Polynomial.vars p
  in
  List.map vars_of_constr cs
  |> List.flatten
  |> remove_duplicates ~eq:Id.eq

(* [tyconstr] is a constraint on types.  The inhabitants of [tyconstr] should
   be dimensionless. *)
type tyconstr = Typ.t

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
    | x :: rest -> List.map (fun typ -> Typ.sub typ x) rest
  in

  cs
  |> List.map
      (function
      | Eq (x, p) -> tyconstr_of_polynomial Polynomial.(add (unit (Powerset.unit x)) p)
      | Po p      -> tyconstr_of_polynomial p)
  |> List.flatten

(* The core solving function. *)
let solve_tyconstr (eqns : tyconstr list) : (Id.t * Typ.t) list =
  let rec solve sol eqns =
    match eqns with
    | []          -> sol
    | eqn :: rest ->
      begin
        match Typ.to_list eqn with
        | []          -> solve sol rest
        | (x, c) :: v ->
          let v = Typ.(scalar Num.(ni (-1) // c) (of_list v)) in
          let sol = List.map (fun (y, v') -> y, Typ.subst v' x v) sol in
          let rest = List.map (fun v' -> Typ.subst v' x v) rest in
          solve ((x, v) :: sol) rest
      end
  in
  solve [] eqns

let dump_sol sol =
  List.iter
    (fun (x, v) ->
      Format.fprintf Format.std_formatter "%a : %a\n"
        Id.pp x
        (Typ.pp ~logarithm:true) v)
    sol

(* Introducing auxiliary program variables causes to lose the original relations
   between the program variables, and it results in weak templates.

   An example:

   Our strategy to introduce auxiliary program variables translates the following
   program

     x, v, t := x + v * dt, v + a * dt, t + dt

   into

     x, v, t := aux0 * x + aux1 * v * dt,
                aux2 * v + aux3 * a * dt,
                aux4 * t + aux 5 * dt.

   and from this program the following type environment is inferred:

     aux0 : 1
     aux1 : ty3^-1 * ty2^-1 * ty4
     aux2 : 1
     aux3 : ty5^-1 * ty2^-1 * ty3
     aux4 : 1
     aux5 : ty2^-1 * ty10                                         ... (E1)
     dt : ty2
     v : ty3
     x : ty4
     a : ty5
     t : ty10

   The powersets whose type is equal to ty3^2 are

     v^2, v * aux3 * a * dt, a^2 * aux3^2 * dt^2                  ... (1)

   which is fewer than the powersets calculated with the type environment
   inferred from the original program:

     a^2 * dt^2, a^2 * dt * t, a^2 * t^2, a * dt * v, a * t * v   ... (2)

   The template generated from (1) misses the powersets needed to find
   the law of conservation of energy, which can be found when using the
   template generated from (2).

   In order to address this problem, we eliminate the unnecessary type
   variables.  A type variable is unnecessary if when the type variable
   is substituted by a type, all the original program variables are
   still not dimensionless.  In this implementation, we attempt to
   substitute by dimensionless (crush_unnecessary_tvars1) first,
   and then substitute by the type such that a auxiliary variable is
   dimensionless (crush_unnecessary_tvars2).
   
   Examples:

   In the following type environment, it is safe to substitute B by
   dimensionless:

     x : A * B, y : A

   In the following type environment, it is safe to substitute either
   A or B by dimensionless:

     x : A * B, y : A * B

   In the following type environment, all type variables are necessary:

     x : A, y : B * C, z : B
*)

let crush_unnecessary_tvars1 tenv vars =
  let unnecessary tenv tv =
    List.for_all
      (fun v -> Typ.remove tv (Tenv.find v tenv) <> Typ.empty)
      vars
  in
  let tvars =
    List.(map Typ.bases (Tenv.cods tenv)
          |> concat
          |> remove_duplicates ~eq:Id.eq)
  in
  List.fold_left
    (fun tenv tv ->
      if unnecessary tenv tv
      then Tenv.mapv (Typ.remove tv) tenv
      else tenv)
    tenv
    tvars

let crush_unnecessary_tvars2 tenv vars =
  (* Return whether it is ok to substitute [tvar] by [typ]. *)
  let unnecessary tenv tvar typ =
    let tenv' = Tenv.mapv (fun typ' -> Typ.subst typ' tvar typ) tenv in
    if List.for_all (fun v -> Tenv.find v tenv' <> Typ.empty) vars
    then Some tenv'
    else None
  in
  let rec loop tenv = function
    | []          -> tenv
    | typ :: rest ->
       match Typ.to_list typ with
       | []          -> loop tenv rest
       | (v, d) :: r ->
          let typ' = Typ.(scalar Num.(ni (-1) // d) (of_list r)) in
          match unnecessary tenv v typ' with
          | Some tenv' -> loop tenv' (List.map (fun t -> Typ.subst t v typ') rest)
          | None       -> loop tenv rest
  in
  loop tenv (Tenv.cods tenv)

let infer (cs : constr list) : (Tenv.t * Typ.t list) =

  let program_vars = vars_of_constraints cs in

  (* multiply each monomial of polynomial in `cs` by auxiliary variables *)
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

  (* [vars] includes auxiliary variables *)
  let vars = vars_of_constraints cs in

  (* create a fresh type environment *)
  let tenv =
    let tvar_id = Id.unique ~prefix:"ty" in
    vars
    |> List.map (fun v -> (v, Typ.unit @@ tvar_id ()))
    |> Tenv.of_list
  in

  let sol = solve_tyconstr (tyconstr_of_constr cs tenv) in

  (* substitute tenv by sol *)
  let tenv =
    Tenv.mapv
      (fun typ -> List.fold_left (fun acc (x, v) -> Typ.subst acc x v) typ sol)
      tenv
  in

  let tenv = crush_unnecessary_tvars1 tenv program_vars in
  let tenv = crush_unnecessary_tvars2 tenv program_vars in

  let avars = List.filter (fun v -> not (mem ~eq:Id.eq v program_vars)) vars
  in

  (* eliminate auxiliary variables
     - if it is dimensionless, simply removed
     - if it is not dimensionless, add to the constant type environment *)
  List.fold_left
    (fun (tenv, ctenv) v ->
      let typ = Tenv.find v tenv in
      Tenv.remove v tenv,
      if Typ.is_empty typ || mem ~eq:Typ.eq typ ctenv then ctenv else typ :: ctenv)
    (tenv, [])
    avars

