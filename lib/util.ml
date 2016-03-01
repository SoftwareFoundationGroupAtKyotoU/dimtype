(* Option *)

let is_some = function
  | Some _ -> true
  | None   -> false

let is_none x = not (is_some x)

let option_value = function
  | None   -> failwith "option_value"
  | Some x -> x

(* List *)

let rec remove_first a = function
  | []     -> []
  | x :: l -> if x = a then l else x :: (remove_first a l)

let rec mem ~eq a = function
  | []     -> false
  | x :: l -> if eq x a then true else mem ~eq a l

let remove_duplicates ~eq l =
  let rec loop acc = function
    | []        -> acc
    | x :: rest -> loop (if mem ~eq x acc then acc else x :: acc) rest
  in
  List.rev (loop [] l)

(* Association lists *)

let rec assoc ~eq a = function
  | []          -> raise Not_found
  | (x, y) :: l -> if eq x a then y else assoc ~eq a l

let assoc_opt ~eq a l =
  try
    Some (assoc ~eq a l)
  with
    Not_found -> None

let remove_assoc ~eq a l =
  let rec loop acc = function
    | []          -> acc
    | (x, y) :: l -> if eq x a then loop acc l else loop ((x, y) :: acc) l
  in
  List.rev (loop [] l)

(* File input *)

let read_all ch =
  let rec loop acc =
    try
      let s = input_line ch in loop (s :: acc)
    with
      End_of_file -> String.concat "\n" (List.rev acc)
  in
  loop []

(*
let measure_time ?(times=50) f =
  let total = ref 0. in
  for i = 1 to times do
    let (fd_in, fd_out) = Unix.pipe () in
    match Unix.fork () with
    | 0 -> (Unix.close fd_in;
            let s = Unix.gettimeofday () in
            f ();
            let e = Unix.gettimeofday () in
            output_bytes
              (Unix.out_channel_of_descr fd_out)
              (string_of_float (e -. s) ^ "\n");
            exit 0)
    | _ -> (Unix.close fd_out;
            match Unix.wait () with
            | (pid, Unix.WEXITED _) ->
               let b = input_line (Unix.in_channel_of_descr fd_in) in
               total := !total +. float_of_string b
            | (pid, _)         ->
               failwith "measure_time")
  done;
  !total /. float_of_int times
*)

let ni = Num.num_of_int

let num_of_float f =
  let open Num in
  let rec loop acc base f count =
    if count > 40
    then acc
    else
      let f' = ni (int_of_float (floor f)) in
      loop (acc +/ f' */ base) (base // ni 10) ((f -. floor f) *. 10.) (count + 1)
  in
  loop (ni 0) (ni 1) f 0

let pp_num fmt n = Format.fprintf fmt "%s" (Num.string_of_num n)

(* printer *)

let string_of pp e = Format.(pp str_formatter e; flush_str_formatter ())

(* parsing *)

(* exception Syntax_error of string *)

(* let parse_with p s = *)
(*   let open MParser in *)
(*   match parse_string p s () with *)
(*   | Success r        -> r *)
(*   | Failed  (msg, _) -> raise (Syntax_error msg) *)

(* project specific *)

let unique_name ~prefix =
  let counter = ref (-1) in
  fun () ->
    incr counter;
    prefix ^ string_of_int !counter
