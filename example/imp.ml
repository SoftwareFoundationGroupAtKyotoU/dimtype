open Util
open Algebra

(* Imp: an example imperative language. *)

(* Definition of Imp. *)

type id = string

type aexp =
  | Const of int
  | Var of id
  | Add of aexp * aexp
  | Mul of aexp * aexp

type bexp =
  | Eq of aexp * aexp
  | Ne of aexp * aexp

type comm =
  | Assign of (id * aexp) list
  | If of bexp * comm * comm
  | While of bexp * comm
  | Seq of comm * comm

(* Printers for Imp. *)

let rec pp_aexp fmt =
  let open Format in function
    | Const i -> fprintf fmt "%d" i
    | Var x -> fprintf fmt "%s" x
    | Add (a1, a2) -> fprintf fmt "@[<2>%a +@ %a@]" pp_aexp a1 pp_aexp a2
    | Mul (a1, a2) -> fprintf fmt "@[<2>(%a) *@ (%a)@]" pp_aexp a1 pp_aexp a2

let pp_bexp fmt = function
  | Eq (a1, a2) -> Format.fprintf fmt "@[<2>%a =@ %a@]" pp_aexp a1 pp_aexp a2
  | Ne (a1, a2) -> Format.fprintf fmt "@[<2>%a !=@ %a@]" pp_aexp a1 pp_aexp a2

let rec pp_comm fmt =
  let open Format in function
    | Assign assigns ->
       let xs = List.map fst assigns
       and aexps = List.map snd assigns
       in
       fprintf fmt "@[<2>(@[<2>%a@]) :=@ (@[<2>%a@])@]"
         (pp_print_list
            ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
            pp_print_string)
         xs
         (pp_print_list
            ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
            pp_aexp)
         aexps
    | If (b, c1, c2) ->
       fprintf fmt "@[<v 0>if %a@;@[<v 2>then@;%a@]@;@[<v 2>else@;%a@]@]"
         pp_bexp b
         pp_comm c1
         pp_comm c2
    | While (b, c) ->
       fprintf fmt "@[<v 0>@[<v 2>while %a do@;%a@]@;end@]"
         pp_bexp b
         pp_comm c
    | Seq (c1, c2) ->
       fprintf fmt "@[<v 0>%a;@;%a@]" pp_comm c1 pp_comm c2

(* Translate Imp components into polynomials. *)

let rec polynomial_of_aexp =
  let open Polynomial in function
    | Const i      -> const (Num.num_of_int i)
    | Var x        -> unit (Powerset.unit (Id.of_string x))
    | Add (a1, a2) -> add (polynomial_of_aexp a1) (polynomial_of_aexp a2)
    | Mul (a1, a2) -> mul (polynomial_of_aexp a1) (polynomial_of_aexp a2)

let polynomial_of_bexp (Eq (a1, a2) | Ne (a1, a2)) =
  Polynomial.add (polynomial_of_aexp a1) (polynomial_of_aexp a2)

(* Extract dimensional constraints from Imp program using translators
   defined above. *)

let rec extract_constr =
  let open Solver in function
    | Assign xs ->
       List.map
         (fun (x, a) -> Eq (Id.of_string x, polynomial_of_aexp a))
         xs
    | If (b, c1, c2) ->
       Po (polynomial_of_bexp b)
       :: extract_constr c1
        @ extract_constr c2
    | While (b, c) -> Po (polynomial_of_bexp b) :: extract_constr c
    | Seq (c1, c2) -> extract_constr c1 @ extract_constr c2

let pp_typlist fmt typs =
  let open Format in
  fprintf fmt "@[<2>[%a]@]"
    (pp_print_list
       ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
       (Typ.pp ~logarithm:true))
    typs

let dump_envs program tenv ctenv =
  Format.printf
    "program:@.@[<v 0>%a@]@.@.tenv: %a@.ctenv: %a@.@."
    pp_comm program
    Solver.Tenv.pp tenv
    pp_typlist ctenv

(* Some tests to infer dimension type environments of imp programs. *)

(* x, v, t := x + v * dt, v + a * dt, t + dt *)
let accel =
  Assign [ ("x", Add (Var "x", Mul (Var "v", Var "dt")))
         ; ("v", Add (Var "v", Mul (Var "a", Var "dt")))
         ; ("t", Add (Var "t", Var "dt"))
         ]

let () =
  let (tenv, ctenv) = Solver.infer (extract_constr accel) in

  dump_envs accel tenv ctenv;

  let typ_of_v  = Solver.Tenv.find (Id.of_string "v") tenv in
  let typ_of_v2 = Typ.scalar (ni 2) typ_of_v in
  let res = Solver.enum_powersets ~max_degree:4 (tenv, ctenv) typ_of_v2 in

  let open Format in
  printf
    "monomials whose types are same as the type of v^2:@.%a@.@."
    (pp_print_list
       ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
       Powerset.pp)
    res

(* A program to calculate quotient and remainder of two natural numbers.
   Subtraction is abstracted to addition. *)
let mannadiv =
  Seq ( Assign [ ("y1", Const 0)
               ; ("y2", Const 0)
               ; ("y3", Var "x1")
               ]
      , (While (Ne (Var "y3", Const 0)
               , If ( Eq (Add (Var "x2", Add (Var "y2", Const 1)), Const 0)
                    , Assign [ ("y1", Add (Var "y1", Const 1))
                             ; ("y2", Const 0)
                             ; ("y3", Add (Var "y3", Const 1)) ]
                    , Assign [ ("y2", Add (Var "y2", Const 1))
                             ; ("y3", Add (Var "y3", Const 1))
                             ]))))

let () =
  let (tenv, ctenv) = Solver.infer (extract_constr mannadiv) in

  dump_envs mannadiv tenv ctenv;

  let typ_of_y1 = Solver.Tenv.find (Id.of_string "y1") tenv in
  let typ_of_x2 = Solver.Tenv.find (Id.of_string "x2") tenv in
  let typ_of_y3 = Solver.Tenv.find (Id.of_string "y3") tenv in
  let typ = Typ.(add typ_of_y1 (add typ_of_x2 typ_of_y3)) in
  let res = Solver.enum_powersets ~max_degree:3 (tenv, ctenv) typ in

  let open Format in
  printf
    "monomials whose types are same as the type of y1*x2*y3:@.%a@.@."
    (pp_print_list
       ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
       Powerset.pp)
    res
