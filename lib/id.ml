type t = string

let pp fmt v = Format.fprintf fmt "%s" v
let print v = pp Format.std_formatter v
let to_string v = v
let of_string v = v
let eq x y = x = y
(* let id_str s = MParser_RE.(regexp (make_regexp "[A-Za-z_][A-Za-z0-9_]*")) s *)
(* let parse s = MParser.(id_str >>= fun id -> return id) s *)
