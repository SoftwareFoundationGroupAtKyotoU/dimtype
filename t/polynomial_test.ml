open Util
open Polynomial

let x = Id.of_string "x"
let y = Id.of_string "y"
let z = Id.of_string "z"
          
let powerset_of_string str =
  let extract_power var =
    let regexp = Str.regexp (var ^ "\\(\\^\\([0-9]+\\)\\)?") in
    try ignore (Str.search_forward regexp str 0);
      try Str.matched_group 2 str |> int_of_string
      with Not_found -> 1
    with Not_found -> 0
  in
  let alphabets = "abcdefghijklmnopqrstuvwxyz" |> Str.(split (regexp "")) in
  List.fold_left
    (fun acc v -> (Id.of_string v, ni (extract_power v)) :: acc)
    []
    alphabets
  |> Algebra.Vect.Num.of_list

let monomial_of_string str =
  let coeff =
    try
      (Str.string_match (Str.regexp "[0-9]*") str 0 |> ignore;
       Str.matched_string str)
      |> int_of_string
    with
      Failure "int_of_string" -> 1
  in
  (coeff, powerset_of_string str)

let polynomial_of_string str =
  let open Str in
  let str =
    str
    |> global_replace (regexp " ") ""
    |> full_split (regexp "+\\|-")
  in
  let rec to_pairs = function
    | Delim d :: Text t :: tl -> (d, t) :: to_pairs tl
    | [] -> []
    | _ -> failwith "pos: to_pairs"
  in
  let pairs =
    match str with
    | Delim d :: Text t :: tl -> (d, t) :: to_pairs tl
    | Text t :: tl -> ("+", t) :: to_pairs tl
    | _ -> failwith "pos: pairs"
  in
  List.fold_left
    (fun acc (d, t) ->
       let c, ps = monomial_of_string t in
       if d = "+"
       then (ps, ni c) :: acc
       else (ps, ni (-c)) :: acc)
    []
    pairs
  |> of_list

let pos = polynomial_of_string

let p1 = pos "3x^2y +  xz^2"
let p2 = pos " x^2y - 2xz^2 + yz"
let p3 = pos "4x^2y -  xz^2 + yz"
let p4 = pos "x + y"
let p5 = pos "xz^2 + 3x^2y + 6xy^2 + 3y^3"

let () =
  print_endline (to_string p1)

let test_vars () =
  Printf.eprintf "test_vars\n";
  assert (vars p1 = List.sort_uniq compare [x; y; z]);
  assert (vars p2 = List.sort_uniq compare [x; y; z]);
  assert (vars p3 = List.sort_uniq compare [x; y; z]);
  assert (vars p4 = List.sort_uniq compare [x; y])

let () =
  print_endline (to_string (pos "xy+yx"));
  print_endline (to_string (pos "2xy"))

let test_eq () =
  Printf.eprintf "test_eq\n";
  assert (eq (pos "xy") (pos "yx"));
  assert (eq (pos "xy+yx") (pos "2xy"))

let test_add () =
  Printf.eprintf "test_add\n";
  assert (Printexc.print (eq (add p1 p2)) p3);
  assert (eq (add p2 p1) p3)

let test_sub () =
  Printf.eprintf "test_sub\n";
  assert (eq (sub p3 p2) p1);
  assert (eq (sub p3 p1) p2)

let test_subst () =
  Printf.eprintf "test_subst\n";
  assert (eq (subst p1 x p4) p5)

let suite =
  [ test_vars
  ; test_eq
  ; test_add
  ; test_sub
  ; test_subst
  ]

let execute () =
  List.iter (fun t -> t ()) suite;
  Printf.eprintf "Polynomial ok\n"
