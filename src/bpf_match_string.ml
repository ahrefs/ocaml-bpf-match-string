open Prelude
open EBPF

type t = string EBPF.insn list

let (@>) p1 p2 = p1 @ p2

(* buffer global context : R1 = ptr  R2 = len *)
let ptr = R1
let len = R2

let exit_prog value =
  [
    movi R0 (if value then 1 else 0);
    ret
  ]

let exit_true' = 
  [
    label "exit_true"
  ] @ exit_prog true

let exit_false' = 
  [
    label "exit_false"
  ] @ exit_prog false

let skip n =
  [
    movi R5 n;
    jmp "exit_false" R5 `GT len;
    subi len n;
    addi ptr n;
  ]

let bound_check =
  [
    jmpi "exit_false" len `EQ 0;
  ]


(* XXX slow could be unrolled / vectorized *)
let skip_to_char c =
  bound_check @ [
    ldx B R3 (ptr, 0);
    subi len 1;
    addi ptr 1;
    jmpi_ (-5) R3 `NE (Char.code c);
  ]

let check_eos cond ret_val =
  let op = 
    match cond with
    | `Eos -> `NE
    | `NotEos -> `EQ
  in
  let exit_val = exit_prog ret_val in
  jmpi_ (List.length exit_val) len op 0 ::
  exit_val


let load_dw r1 r2 i =
  movi r1 Int64.(logand 0xFFFFFFFFL i |> to_int) ::
  movi r2 Int64.(logand 0xFFFFFFFFL (shift_right_logical i 32) |> to_int) ::
  lshi r2 32 ::
  or_ r1 r2 ::
  []

let match_string str =
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
  let jmp_out i = jmpi "exit_false" R3 `NE (Int64.to_int i) in
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
            [ ldx' DW idx ] @
            load_dw R4 R5 c @
            [ jmp "exit_false" R3 `NE R4]
          in
          acc @ unrolled (idx + 8) (CCString.Sub.sub str 8 (CCString.Sub.length str - 8))
        end
  in
  let length = String.length str in
  [
    movi R5 length;
    jmp "exit_false" R5 `GT R2 ;
  ] @
  unrolled 0 (CCString.Sub.make str 0 ~len:(String.length str)) @
  [
    subi len length;
    addi ptr length;
  ]

let prog n =
  n @
  exit_true' @
  exit_false' 

let assemble n = EBPF.assemble ~options:({ default with jump_back = true }) @@ prog n

