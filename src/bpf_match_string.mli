type t

val (@>) : t -> t -> t
(** combine two programs sequencially: [a %> b] will execute [a] then execute [b] *)

val skip : int -> t
(** [skip n] generates instructions to skip [n] chars or exit false if reaching EOS *)

val match_string : string -> t
(** [match_string "hello"] tries to match current postion with "hello" or exit false *)

val check_eos : [`Eos | `NotEos ] -> bool -> t
(**
   [check_eos `Eos b] will exit with [b] if current position is end of string 
   [check_eos `NotEos b] will exit with [b] if current position is not end of string 
*)

val skip_to_char : char -> t
(** [skip_to_char c] generates instructions to move just after the first occurence of c exit false if reaching EOS *)

val assemble : t -> string
(* exemple :

    assemble begin 
      skip 4 @>
      skip_to_char '%' @>
      match_string "hello world" @>
      check_eos `NotEos false
    end
*)
