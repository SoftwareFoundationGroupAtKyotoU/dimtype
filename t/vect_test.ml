open Util

(* Test for the module of Vect with integer coefficients. *)
module Test_int = struct

  include Algebra.Vect.Int

  let x = Id.of_string "x"
  let y = Id.of_string "y"
  let z = Id.of_string "z"
  let v1 = of_list [(x, 1); (y, -2)]
  let v1' = of_list [(x, 1); (y, -2)]
  let v2 = of_list [(z, 3); (x, 4)]
  let v3 = of_list [(z, 6); (x, 8)]

  let test_eq () =
    assert (eq v1 v1');
    assert (not (eq v1 v2))

  let test_size () =
    assert (size v1 = 2);
    assert (size v2 = 2)

  let test_coeff () =
    assert (coeff x v1 = 1);
    assert (coeff y v1 = -2);
    assert (coeff x v2 = 4);
    assert (coeff z v2 = 3)

  let test_scalar () =
    assert (eq (scalar 2 v2) v3)

  let test_add () =
    let expected = of_list [(x, 5); (y, -2); (z, 3)] in
    assert (eq (add v1 v2) expected)

  let test_subst () =
    assert (eq (subst v1 y v2) (of_list [(x, -7); (z, -6)]))

  let suite =
    [ test_eq
    ; test_size
    ; test_coeff
    ; test_scalar
    ; test_add
    ; test_subst
    ]
end

let execute () =
  List.iter (fun t -> t ()) Test_int.suite;
  Printf.eprintf "Vect ok\n"
