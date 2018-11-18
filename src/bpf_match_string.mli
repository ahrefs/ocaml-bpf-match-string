type t

type code =
  | Skip of int
    (** [Skip n] generates instructions to skip [n] chars or exit false if reaching EOS *)
  | MatchString of string
    (** [Match_string "hello"] tries to match current postion with "hello" or exit false *)
  | MatchChars of char list
    (** [Match_chars l] will exit true if char at current position is one of the list [l]; does not advance position *)
  | MismatchChars of char list
    (** [Mismatch_chars l] will exit false if char at current position is one of the list [l]; does not advance position *)
  | CheckEOS of bool
    (**
       [Check_eos false] will exit false if current position is end of string
       [Check_eos true] will exit false if current position is not end of string
    *)
  | SkipToChar of char
    (** [Skip_to_char c] generates instructions to move just after the first occurence of c exit false if reaching EOS *)

val string_of_code : code -> string

val make : code list -> t

val assemble : t -> string

(* example :
    assemble @@ make [
      Skip 4;
      Skip_to_char '%';
      Match_string "hello world";
      Check_eos true;
    ]
*)
