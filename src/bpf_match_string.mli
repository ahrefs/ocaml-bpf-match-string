type t

type code =
  | Skip of int
    (** [Skip n] skips [n] chars or
       returns false if EOS would be reached *)
  | MatchString of string
    (** [MatchString s] matches current postion with string [s] or
       returns false if there is no match *)
  | MatchChars of char list
    (** [MatchChars l] matches current position with one of characters in [l] or
        returns false if there is no match; does not advance the current position *)
  | MismatchChars of char list
    (** [MismatchChars l] ensures current position does not match one of characters in [l] or
        returns false if there is a match; does not advance the current position *)
  | CheckEOS of bool
    (**
       [CheckEOS true] checks if current position is end of string, returns false on no EOS
       [CheckEOS false] checks if current position is not end of string, returns false on EOS
    *)
  | SkipToChar of char
  (** [SkipToChar c] moves current position just after the first occurence of [c] or
      returns false if EOS is reached in the process *)
  | And of code list list
  (** [And [prog1; prog2; ...]] ensures all of [prog1, prog2, ...] match, returns false otherwise;
      does not advance the current position *)
  | Or of code list list
  (** [Or [prog1; prog2; ...]] ensures one of [prog1, prog2, ...] match, returns false otherwise;
     DOES advance the current position *)
  | Not of code list
  (** [Not prog] ensures prog does not match; returns false if it does. *)

val string_of_code : code -> string

val make : code list -> t

val assemble : t -> string

(* example :
    assemble @@ make [
      Skip 4;
      SkipToChar '%';
      MatchString "hello world";
      CheckEOS true;
    ]
*)
