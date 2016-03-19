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

(* [tyconstr] is a constraint on types.  This constraints expresses
   that the inhabitants of [tyconstr] should be dimensionless. *)
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

(* The dimension type system by Kennedy has a flaw and we cannot apply
   this type system directly.  This dimension type system gives
   dimensionless to the constants, but this may causes all program
   variables be inferred dimensionless.  The dimensionless environment
   has no hint to make an efficient template, it is needed to avoid
   such a situation.

   Example:

   The following program is petter2, which calculates the sum of the
   squares of natural numbers below N:

   1:  x := 0;
   2:  y := 0;
   3:  while y != N do
   4:    x := x + y^2;
   5:    y := y + 1
   6:  end

   The value of y is incremented by 1 at the line 5, but 1 is
   dimensionless so y is also dimensionless.  Since there is
   comparison between N and y and addition of x and y^2, N and x are
   also dimensionless.

   If we modify the line 5 to y := y + k, the dimension type system
   can infer a non-trivial environment from the modified version of
   petter2 and we can generate a template from the environment which
   can be used to calculate an useful invariant efficiently.

   The expression like y^2 + y also spoil the dimensional meaning
   because the only dimension satisfy the constraints calculated from
   this expression is dimensionless.

   An easy solution to this problem is to regard the monomials in a
   given program as be multiplied by 1 and to give non-dimensionless
   types to constants. To regard the monomials are multiplied by 1
   makes the expressions such as y^2 + y are typed as
   non-dimensionless.

   The main function [infer] multiplies fresh variables to each
   monomial in the given program and infers dimension types as usual,
   then remove the fresh variables, collect their types, and return
   the types as the environment of constants.  This implementation has
   an advantage that it enables the above solution without changing
   the existing dimension type inferrence algorithm.

   The above solution, however, causes to lose the original relations
   between the program variables, and it results in weak templates.
   We explain this problem by using an example.  Note that in the
   following explanation we use auxiliary variables as [infer] does
   since it makes the explanation easy to understand, but to introduce
   auxiliary variables is not essential and this problem is inevitable
   to the solution.

   Example:

   Our strategy to introduce auxiliary program variables translates
   the following program

   x, v, t := x + v * dt, v + a * dt, t + dt

   into

   x, v, t := aux0 * x + aux1 * v * dt,
   aux2 * v + aux3 * a * dt,
   aux4 * t + aux 5 * dt.

   and the following type environment is inferred from this program:

   aux0 : 1
   aux1 : ty3^-1 * ty2^-1 * ty4
   aux2 : 1
   aux3 : ty5^-1 * ty2^-1 * ty3
   aux4 : 1
   aux5 : ty2^-1 * ty10                                       ... (E1)
   dt : ty2
   v : ty3
   x : ty4
   a : ty5
   t : ty10

   The powersets whose types are equal to ty3^2 (= the type of v^2)
   are

   v^2, v * aux3 * a * dt, a^2 * aux3^2 * dt^2                ... (1)

   which is fewer than the powersets calculated with the type
   environment inferred from the original program:

   a^2 * dt^2, a^2 * dt * t, a^2 * t^2, a * dt * v,
   a * t * v, v^2, a * x                                      ... (2)

   The template generated from (1) misses the powersets needed to find
   the law of conservation of energy, which can be found when using
   the template generated from (2).

   In order to address this problem, we eliminate the unnecessary type
   variables.  A type variable is unnecessary if when the type
   variable is substituted by a type, all the original program
   variables are still not dimensionless.  In this implementation, we
   attempt to substitute by dimensionless (crush_unnecessary_tvars1)
   first, and then substitute by the type such that a auxiliary
   variable is dimensionless (crush_unnecessary_tvars2).
   
   Example:

   In the following type environment, it is safe to substitute B by
   dimensionless:

   x : A * B, y : A

   In the following type environment, it is safe to substitute either
   A or B by dimensionless:

   x : A * B, y : A * B

   In the following type environment, all type variables are
   necessary:

   x : A, y : B * C, z : B

   Example:

   crush_unnecessary_tvars2 eliminates aux1, aux3, and aux5 from E1
   and translates E1 into the following environment:

   (aux0 .. aux5 are all dimensionless, hence we can ignore them)
   dt : ty10
   v : ty10^-1 * ty4
   x : ty4                                                   ... (E1')
   a : ty10^-2 * ty4
   t : ty10

   (E1') has usual physical meaning if we think ty10 as the dimension
   of time and ty4 as the dimension of length and the law of
   conservation of energy can be calculated from a template generated
   from the powersets whose type is equal to the type of v^2 in (E1').
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

let infer ?(heuristic=true) (cs : constr list) : (Tenv.t * Typ.t list) =

  let program_vars = vars_of_constraints cs in

  (* multiply each monomial of polynomial in [cs] by auxiliary variables *)
  let cs =
    if not heuristic
    then cs
    else
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

  let tenv =
    if not heuristic
    then tenv
    else
      crush_unnecessary_tvars2
        (crush_unnecessary_tvars1 tenv program_vars)
        program_vars
  in

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

exception No_solution

(* This version of [solve_tyconstr] can solve equations which include
   constants (bad implementation). *)
let solve_tyconstr2 (eqns : tyconstr list) : (Id.t * Typ.t) list =
  let rec solve sol eqns =
    match eqns with
    | []          -> sol
    | eqn :: rest ->
      begin
        match Typ.to_list eqn with
        | []          -> solve sol rest
        | [x, c] when Id.to_string x = "const" -> raise No_solution
        | (x, c) :: v ->
           if Id.to_string x = "const"
           then
             solve sol ((Typ.of_list (v @ [x, c])) :: rest)
           else
             let v = Typ.(scalar Num.(ni (-1) // c) (of_list v)) in
             let sol = List.map (fun (y, v') -> y, Typ.subst v' x v) sol in
             let rest = List.map (fun v' -> Typ.subst v' x v) rest in
             solve ((x, v) :: sol) rest
      end
  in
  solve [] eqns

exception Last_combination

(* Enumerate all integer elements in [lo,hi]^n. *)
let enum_ranges lo hi n =
  let rec next_combination = function
    | []        -> raise Last_combination
    | x :: rest ->
       if x = hi
       then lo :: next_combination rest
       else (x + 1) :: rest
  in
  let rec loop last acc =
    try
      let next = next_combination last in
      loop next (last :: acc)
    with
      Last_combination -> last :: acc
  in
  loop (ntimes (fun _ -> 0) n) []

let enum_powersets ~max_degree (tenv, ctenv) typ =
  (* Fake constants as variables. *)
  let constants =
    let f = Id.unique ~prefix:"constant" in
    List.map (fun _ -> f ()) ctenv
  in
  let tenv =
    List.fold_left
      (fun acc (c, typ) -> Tenv.add_overwrite c typ acc)
      tenv
      (List.combine constants ctenv)
  in

  let vars     = Tenv.doms tenv in
  let const_id = Id.of_string "const" in

  (* Construct constraints.  Use the program variable names as the
     parameters of constraints. *)
  let constraints =
    List.map
      (fun (d, c) ->
        (const_id, Num.(ni 0 -/ c))
        :: (List.map (fun v -> (v, Typ.coeff d (Tenv.find v tenv))) vars)
        |> Typ.of_list)
      (Typ.to_list typ)
  in

  try

    (* Solve the constraint and split them into dependent parameters and
       independent parameters. *)
    let sol = solve_tyconstr2 constraints in
    let dependent_params = List.map fst sol in
    let independent_params =
      List.filter (fun v -> not (mem ~eq:Id.eq v dependent_params)) vars
    in

    (* All the possible values of [independent_params]. *)
    let assigns =
      List.map
        (List.map ni)
        (enum_ranges 0 max_degree (List.length independent_params))
    in

    (* Calculate the values of [dependent_params] by assigning [assign]
       values. *)
    let substitute (assign : (Id.t * Num.num) list) =
      let valuate d =
        if Id.to_string d = "const"
        then ni 1
        else assoc ~eq:Id.eq d assign
      in
      List.map
        (fun (v, typ) -> (v, Typ.eval valuate typ))
        sol
    in

    (* Calculate the values of [dependent_params] and return the values if
       they are in the range of 0 to max_degree. *)
    let try_to_assign assigns : (Id.t * Num.num) list list =
      let rec loop acc = function
        | []             -> acc
        | assign :: rest ->
           let assign_pairs = List.combine independent_params assign in
           let substed      = substitute assign_pairs in
           let all_positive = List.for_all (fun (_, d) -> Num.(ni 0 <= d)) substed in
           let degree       =
             List.fold_left
               (fun acc (_, d) -> Num.(acc +/ d)) (ni 0)
               (substed @ assign_pairs)
           in
           if all_positive && Num.(degree <= ni max_degree)
           then loop ((substed @ assign_pairs) :: acc) rest
           else loop acc rest
      in
      loop [] assigns
    in

    let is_constant x =
      let x = Id.to_string x in
      String.length x >= 8 && String.sub x 0 8 = "constant"
    in

    try_to_assign assigns

    (* remove constants *)
    |> List.map (List.filter (fun (x, _) -> not (is_constant x)))

    (* convert into powersets.  WARN that [Num.int_of_num] can lose
       accurate information. *)
    |> List.map (List.map (fun (x, v) -> x, Num.int_of_num v))
    |> List.map Powerset.of_list
    |> remove_duplicates ~eq:Powerset.eq

  with

    No_solution -> []
