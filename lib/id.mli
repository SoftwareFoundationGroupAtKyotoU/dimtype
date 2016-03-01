type t

val pp : Format.formatter -> t -> unit
val print : t -> unit
val to_string : t -> string
val of_string : string -> t
val eq : t -> t -> bool
(* val parse : 'a MParser.state -> (t, 'a) MParser.reply *)
