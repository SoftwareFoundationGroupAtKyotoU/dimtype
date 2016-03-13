open Util
open Algebra
open Solver

let pp_typlist fmt typs =
  let open Format in
  fprintf fmt "@[<2>[%a]@]"
    (pp_print_list
       ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
       (Typ.pp ~logarithm:true))
    typs

(* TODO: make the output readable *)
let execute () =
  print_endline "-- begin --";

  let x = Id.of_string "x"
  and y = Id.of_string "y"
  and z = Id.of_string "z"
  and v = Id.of_string "v"
  and dt = Id.of_string "dt"
  and t = Id.of_string "t"
  and a = Id.of_string "a"
  in
  (* x^2y + xz^2 *)
  let p1 = Polynomial.of_list
    [ Powerset.of_list [ (x, 2); (y, 1) ], ni 2
    ; Powerset.of_list [ (x, 1); (z, 2) ], ni 3
    ]
  in
  let tenv, _ = infer [Po p1] in
  print_endline ((Tenv.to_string tenv) ^ "\n");

  let cs =
    [ Eq (x, Polynomial.of_list [ Powerset.of_list [ (x, 1) ], ni 1
                                ; Powerset.of_list [ (v, 1); (dt, 1) ], ni 1 ])
    ; Eq (v, Polynomial.of_list [ Powerset.of_list [ (v, 1) ], ni 1
                                ; Powerset.of_list [ (a, 1); (dt, 1) ], ni 1 ])
    ; Eq (t, Polynomial.of_list [ Powerset.of_list [ (t, 1) ], ni 1
                                ; Powerset.of_list [ (dt, 1) ], ni 1 ])
    ]
  in
  let tenv, _ = infer cs in
  print_endline (Tenv.to_string tenv);

  let aux n = Id.of_string ("aux" ^ string_of_int n) in
  let cs =
    [ Eq (x, Polynomial.of_list [ Powerset.of_list [ (aux 0, 1); (x, 1) ], ni 1
                                ; Powerset.of_list [ (aux 1, 1); (v, 1); (dt, 1) ], ni 1 ])
    ; Eq (v, Polynomial.of_list [ Powerset.of_list [ (aux 2, 1); (v, 1) ], ni 1
                                ; Powerset.of_list [ (aux 3, 1); (a, 1); (dt, 1) ], ni 1 ])
    ; Eq (t, Polynomial.of_list [ Powerset.of_list [ (aux 4, 1); (t, 1) ], ni 1
                                ; Powerset.of_list [ (aux 5, 1); (dt, 1) ], ni 1 ])
    ]
  in
  let tenv, _ = infer cs in
  print_endline (Tenv.to_string tenv);

  let cs = [ Po (Polynomial.of_list [ Powerset.of_list [ (x, 1) ], ni 1
                                    ; Powerset.of_list [ (x, 2) ], ni 1 ]) ]
  in
  let tenv, ctenv = infer cs in
  Format.(
    fprintf std_formatter "ctenv: %a@.tenv: %a@."
      (pp_print_list
         ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
         (Typ.pp ~logarithm:false))
      ctenv
      Tenv.pp
      tenv);

  print_endline "-- end --"
