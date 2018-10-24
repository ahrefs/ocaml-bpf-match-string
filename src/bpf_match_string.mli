type t

type code =
  | Skip of int
  | MatchString of string
  | MismatchChars of char list
  | CheckEOS of bool
  | SkipToChar of char

val (@>) : t -> t -> t
(** combine two programs sequentially: [a %> b] will execute [a] then execute [b] *)

val concat : t list -> t
(** combine list of programs sequentially *)

val skip : int -> t
(** [skip n] generates instructions to skip [n] chars or exit false if reaching EOS *)

val match_string : string -> t
(** [match_string "hello"] tries to match current postion with "hello" or exit false *)

val mismatch_chars : char list -> t
(** [mismatch_chars l] will exit false if char at current position is one of the list [l]; does not advance position *)

val check_eos : bool -> t
(**
   [check_eos false] will exit false if current position is end of string
   [check_eos true] will exit false if current position is not end of string
*)

val skip_to_char : char -> t
(** [skip_to_char c] generates instructions to move just after the first occurence of c exit false if reaching EOS *)

val string_of_code : code -> string

val make : code list -> t

val assemble : t -> string
(* example :

    assemble begin
      skip 4 @>
      skip_to_char '%' @>
      match_string "hello world" @>
      check_eos true
    end
*)
