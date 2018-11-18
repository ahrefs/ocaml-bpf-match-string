open Prelude
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

(* stack pointer : R10 *)
let stack = R10

type context = {
  true_ : label;
  false_ : label;
  next : label;
  cur : label;
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
  addi R10 8 ::
  l

let with_backtrack what f ({ true_; false_; cur; next; }, l) =
  let (pop_true, pop_false) =
    match what with
    | `True -> pop_state, drop_state
    | `False -> drop_state, pop_state
    | `Both -> pop_state, pop_state
  in
  let ctx = { true_ = cur; false_ = cur + 1; cur = cur + 2; next = cur; } in
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

let load_dw r1 r2 i l =
  movi r1 Int64.(logand 0xFFFFFFFFL i |> to_int) ::
  movi r2 Int64.(logand 0xFFFFFFFFL (shift_right_logical i 32) |> to_int) ::
  lshi r2 32 ::
  or_ r1 r2 ::
  l

let match_string str ({ false_; _ } as ctx, l) =
  let ldx' size at = EBPF.ldx size R3 (ptr, at) in
  let extract_int count str =
    match count with
    | 1 -> CCString.Sub.get str 0 |> Char.code |> Int64.of_int
    | 2 | 4 | 8 ->
        let r = ref 0L in
        for idx = 0 to count - 1 do
          let c = CCString.Sub.get str idx |> Char.code |> Int64.of_int in
          r := Int64.(logor !r (shift_left c (8 * idx)))
        done;
        !r
    | n -> Exn.fail "unsupported unrolling size %d" n
  in
  let jmp_out i = jmpi false_ R3 `NE (Int64.to_int i) in
  let rec unrolled idx str =
    let remaining = CCString.Sub.length str in
    match remaining with
    | 0 -> []
    | 1 -> [ldx' B idx; jmp_out (extract_int 1 str) ]
    | 2 -> [ldx' H idx; jmp_out (extract_int 2 str) ]
    | 4 -> [ldx' W idx; jmp_out (extract_int 4 str)]
    | n ->
        if n < 8 then begin
          let (acc, str, n, idx) =
            if n > 4 then begin
              let acc = [ldx' W idx; jmp_out (extract_int 4 str)] in
              (acc, CCString.Sub.sub str 4 (n - 4),  n - 4, idx + 4)
            end else
            ([], str, n, idx)
          in
          let tail =
            CCList.init n id |>
            List.map (fun i ->
              let str = CCString.Sub.sub str i 1 in
              [ ldx' B (idx + i); jmp_out (extract_int 1 str) ]
            ) |>
            List.flatten
          in
          acc @ tail
        end else begin
          let c = extract_int 8 str in
          let acc =
            ldx' DW idx ::
            load_dw R4 R5 c [
              jmp false_ R3 `NE R4;
            ]
          in
          acc @ unrolled (idx + 8) (CCString.Sub.sub str 8 (CCString.Sub.length str - 8))
        end
  in
  let length = String.length str in
  ctx,
  movi R5 length ::
  jmp false_ R5 `GT R2 ::
  unrolled 0 (CCString.Sub.make str 0 ~len:(String.length str)) @
  begin
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
and continue { false_; _ } ({ cur; _ }, l) =
  { true_ = cur; false_; next = cur; cur = cur + 1; }, label cur l
and retry { true_; _ } ({ cur; _ }, l) =
  { true_; false_ = cur; next = cur; cur = cur + 1; }, label cur l
and exec prog (ctx, l) =
  List.rev prog |>
  List.fold_left begin fun (ctx, l) code ->
    map_code code (ctx, l) |>
    continue ctx
  end (ctx, l)
and and_ prog (ctx, l) =
  List.rev prog |>
  List.fold_left begin fun (ctx, l) prog ->
    with_backtrack `Both (exec prog) (ctx, l) |>
    continue ctx
  end (ctx, l)
and or_ prog (ctx, l) =
  List.rev prog |>
  List.fold_left begin fun (ctx, l) prog ->
    with_backtrack `False (exec prog) (ctx, l) |>
    retry ctx
  end (ctx, l)
and not_ prog ({ true_; false_; _ } as ctx, l) =
  with_backtrack `Both (exec prog) ({ ctx with true_ = false_; false_ = true_; }, l)

let rec string_of_code = function
  | Skip i -> sprintf "Skip %d" i
  | MatchString s -> sprintf "MatchString %S" s
  | MatchChars l -> sprintf "MatchChars %s" (Stre.list (sprintf "%C") l)
  | MismatchChars l -> sprintf "MismatchChars %s" (Stre.list (sprintf "%C") l)
  | CheckEOS b -> sprintf "CheckEOS %b" b
  | SkipToChar c -> sprintf "SkipToChar %C" c
  | And l -> sprintf "And %s" (Stre.list (Stre.list string_of_code) l)
  | Or l -> sprintf "Or %s" (Stre.list (Stre.list string_of_code) l)
  | Not l -> sprintf "Not %s" (Stre.list string_of_code l)

let make prog =
  let exit value l = label value @@ movi R0 value :: ret :: l in
  exec prog ({ true_ = 1; false_ = 0; next = 1; cur = 2; }, exit 1 @@ exit 0 []) |>
  snd

let assemble n = EBPF.assemble ~options:({ default with jump_back = true }) n

