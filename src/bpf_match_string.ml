open Printf
open EBPF

type label = int

type t = label EBPF.insn list

type code =
  | Skip of int
  | MatchString of string
  | MatchChars of char list
  | MismatchChars of char list
  | CheckEOS of bool
  | SkipToChar of char
  | And of code list list
  | Or of code list list
  | Not of code list

(* buffer global context : R1 = ptr  R2 = len *)
let ptr = R1
let len = R2

(* stack pointers : R10 = frame  R9 = stack *)
let frame = R10
let stack = R9

type context = {
  true_ : label;
  false_ : label;
  next : label;
  cur : label;
  stack : bool;
}

let label lbl l = label lbl :: l

let jump lbl l = jump lbl :: l

let jump_or_skip lbl next l = if lbl = next then l else jump lbl l

let jump_true { true_; next; _ } l = jump_or_skip true_ next l

let jump_false { false_; next; _ } l = jump_or_skip false_ next l

let push reg l = stx DW (stack, -8) reg :: subi stack 8 :: l

let pop reg l = ldx DW reg (stack, 0) :: addi stack 8 :: l

let push_state (ctx, l) = ctx, push len l

let pop_state l =
  pop R3 @@
  sub len R3 :: (* delta = len - old_len *)
  add ptr len :: (* ptr += delta *)
  mov len R3 :: (* len = old_len *)
  l

let drop_state l =
  addi stack 8 ::
  l

let with_backtrack what f ({ true_; false_; cur; next; _ }, l) =
  let (pop_true, pop_false) =
    match what with
    | `True -> pop_state, drop_state
    | `False -> drop_state, pop_state
    | `Both -> pop_state, pop_state
  in
  let ctx = { true_ = cur; false_ = cur + 1; cur = cur + 2; next = cur; stack = true; } in
  push_state @@
  f begin
    ctx,
    label cur @@
      pop_true @@
      jump true_ @@
    label (cur + 1) @@
      pop_false @@
      jump_or_skip false_ next l
  end

let bound_check ctx l = jmpi ctx.false_ len `EQ 0 :: l

let skip n (ctx, l) =
  ctx,
  movi R5 n ::
  jmp ctx.false_ R5 `GT len ::
  subi len n ::
  addi ptr n ::
  jump_true ctx l

(* XXX slow could be unrolled / vectorized *)
let skip_to_char c ({ cur; _ } as ctx, l) =
  { ctx with cur = cur + 1; },
  label cur @@
    bound_check ctx @@
    ldx B R3 (ptr, 0) ::
    subi len 1 ::
    addi ptr 1 ::
    jmpi cur R3 `NE (Char.code c) ::
  jump_true ctx l

let check_eos cond (ctx, l) =
  ctx,
  jmpi ctx.false_ len (if cond then `NE else `EQ) 0 :: jump_true ctx l

let match_string str ({ false_; _ } as ctx, l) =
  let extract_int idx n str =
    let rec loop res = function
      | n when n < 0 -> res
      | n ->
      let c = Char.code str.[idx + n] in
      loop Int64.(logor (shift_left res 8) (of_int c)) (n - 1)
    in
    let n = match n with B -> 1 | H -> 2 | W -> 4 | DW -> 8 in
    loop 0L (n - 1)
  in
  let jmp_out i = jmpi false_ R3 `NE (Int64.to_int i) in
  let ldx size at = ldx size R3 (ptr, at) in
  let cmp size idx str l = ldx size idx :: jmp_out (extract_int idx size str) :: l in
  let rec unrolled str idx l =
    let remaining = String.length str - idx in
    match remaining with
    | 0 -> l
    | 1 -> cmp B idx str l
    | 2 | 3 -> cmp H idx str (unrolled str (idx + 2) l)
    | 4 | 5 | 6 | 7 -> cmp W idx str (unrolled str (idx + 4) l)
    | _ ->
    let c = extract_int idx DW str in
    ldx DW idx ::
    lddw R4 c ::
    jmp false_ R3 `NE R4 ::
    unrolled str (idx + 8) l
  in
  let length = String.length str in
  ctx,
  movi R5 length ::
  jmp false_ R5 `GT len ::
  unrolled str 0 begin
    subi len length ::
    addi ptr length ::
    jump_true ctx l
  end

let match_chars set (ctx, l) =
  ctx,
  bound_check ctx @@
  let l = List.fold_left (fun l c -> jmpi ctx.true_ R3 `EQ (Char.code c) :: l) (jump_false ctx l) set in
  if set = [] then l else ldx B R3 (ptr, 0) :: l

let mismatch_chars set (ctx, l) =
  ctx,
  bound_check ctx @@
  let l = List.fold_left (fun l c -> jmpi ctx.false_ R3 `EQ (Char.code c) :: l) (jump_true ctx l) set in
  if set = [] then l else ldx B R3 (ptr, 0) :: l

let exec_with_backtrack exec what prog (ctx, l) =
  let need_backtrack = function MatchChars _ | MismatchChars _ | CheckEOS _ | And _ | Not _ -> false | _ -> true in
  let need_backtrack_true l = List.exists need_backtrack l in
  let rec need_backtrack_false = function
    | [] | [ Skip _ | MatchString _ | Or _; ] -> false
    | hd :: tl -> need_backtrack hd || need_backtrack_false tl
  in
  let need_backtrack =
    match what with
    | `False -> need_backtrack_false
    | `True | `Both -> need_backtrack_true
  in
  match need_backtrack prog with
  | true -> with_backtrack what (exec prog) (ctx, l)
  | false -> exec prog (ctx, l)

let rec map_code code =
  match code with
  | Skip i -> skip i
  | MatchString s -> match_string s
  | MatchChars l -> match_chars l
  | MismatchChars l -> mismatch_chars l
  | CheckEOS b -> check_eos b
  | SkipToChar c -> skip_to_char c
  | And l -> and_ l
  | Or l -> or_ l
  | Not l -> not_ l
and continue { false_; _ } ({ cur; stack; _ }, l) =
  { true_ = cur; false_; next = cur; cur = cur + 1; stack; }, label cur l
and retry { true_; _ } ({ cur; stack; _ }, l) =
  { true_; false_ = cur; next = cur; cur = cur + 1; stack; }, label cur l
and exec prog (ctx, l) =
  List.rev prog |>
  List.fold_left begin fun (ctx, l) code ->
    map_code code (ctx, l) |>
    continue ctx
  end (ctx, l)
and and_ prog (ctx, l) =
  List.rev prog |>
  List.fold_left begin fun (ctx, l) prog ->
    exec_with_backtrack exec `Both prog (ctx, l) |>
    continue ctx
  end (ctx, l)
and or_ prog (ctx, l) =
  List.rev prog |>
  List.fold_left begin fun (ctx, l) prog ->
    exec_with_backtrack exec `False prog (ctx, l) |>
    retry ctx
  end (ctx, l)
and not_ prog ({ true_; false_; _ } as ctx, l) =
  exec_with_backtrack exec `Both prog ({ ctx with true_ = false_; false_ = true_; }, l)

let str_list f l = "[" ^ String.concat ";" (List.map f l) ^ "]"

let rec string_of_code = function
  | Skip i -> sprintf "Skip %d" i
  | MatchString s -> sprintf "MatchString %S" s
  | MatchChars l -> sprintf "MatchChars %s" (str_list (sprintf "%C") l)
  | MismatchChars l -> sprintf "MismatchChars %s" (str_list (sprintf "%C") l)
  | CheckEOS b -> sprintf "CheckEOS %b" b
  | SkipToChar c -> sprintf "SkipToChar %C" c
  | And l -> sprintf "And %s" (str_list (str_list string_of_code) l)
  | Or l -> sprintf "Or %s" (str_list (str_list string_of_code) l)
  | Not l -> sprintf "Not %s" (str_list string_of_code l)

let make prog =
  let exit value l = label value @@ movi R0 value :: ret :: l in
  let init_stack (ctx, l) = if ctx.stack then mov stack frame :: l else l in
  exec prog ({ true_ = 1; false_ = 0; next = 1; cur = 2; stack = false; }, exit 1 @@ exit 0 []) |>
  init_stack

let assemble n = EBPF.assemble ~options:({ default with jump_back = true }) n

