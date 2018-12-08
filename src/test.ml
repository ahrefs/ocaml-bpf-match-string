
open Printf
open Bpf_match_string

let y x = x, 1
let n x = x, 0

let inputs = [
  "";
  " ";
  "42";
  "+++";
  "foobarBaz Long String Ahead";
  "fooBar";
  "bar baz";
]

let zip inputs outcomes = List.map2 (fun f x -> f x) outcomes inputs

let test_skip_0 = [ Skip 0; ], zip inputs [ y; y; y; y; y; y; y; ]
let test_skip_n = [ Skip 3; ], zip inputs [ n; n; n; y; y; y; y; ]
let test_skip_nn = [ Skip 4; Skip 2; ], zip inputs [ n; n; n; n; y; y; y; ]
let test_skip_big = [ Skip 100500; ], zip inputs [ n; n; n; n; n; n; n; ]

let test_match_string_0 = [ MatchString ""; ], zip inputs [ y; y; y; y; y; y; y; ]
let test_match_string_1 = [ MatchString "f"; ], zip inputs [ n; n; n; n; y; y; n; ]
let test_match_string_2 = [ MatchString "fo"; ], zip inputs [ n; n; n; n; y; y; n; ]
let test_match_string_3 = [ MatchString "foo"; ], zip inputs [ n; n; n; n; y; y; n; ]
let test_match_string_4 = [ MatchString "foob"; ], zip inputs [ n; n; n; n; y; n; n; ]
let test_match_string_5 = [ MatchString "fooba"; ], zip inputs [ n; n; n; n; y; n; n; ]
let test_match_string_6 = [ MatchString "foobar"; ], zip inputs [ n; n; n; n; y; n; n; ]
let test_match_string_7 = [ MatchString "foobarB"; ], zip inputs [ n; n; n; n; y; n; n; ]
let test_match_string_8 = [ MatchString "foobarBa"; ], zip inputs [ n; n; n; n; y; n; n; ]
let test_match_string_9 = [ MatchString "foobarBaz"; ], zip inputs [ n; n; n; n; y; n; n; ]
let test_match_string_rev_2 = [ MatchString "of"; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_string_rev_3a = [ MatchString "oof"; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_string_rev_3b = [ MatchString "ofo"; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_string_rev_4a = [ MatchString "boof"; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_string_rev_4b = [ MatchString "ofbo"; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_string_rev_7a = [ MatchString "Braboof"; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_string_rev_7b = [ MatchString "Braboof\000"; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_string_rev_7c = [ MatchString "\000Braboof"; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_string_rev_8a = [ MatchString "aBraboof"; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_string_rev_8b = [ MatchString "boofaBra"; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_string_rev_8c = [ MatchString "ofboraaB"; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_string_rev_9a = [ MatchString "zaBraboof"; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_string_rev_9b = [ MatchString "aBraboofz"; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_string_nn0 = [ MatchString "foo"; MatchString ""; ], zip inputs [ n; n; n; n; y; y; n; ]
let test_match_string_nn1 = [ MatchString "fo"; MatchString "o"; ], zip inputs [ n; n; n; n; y; y; n; ]
let test_match_string_nn2 = [ MatchString "f"; MatchString "oo"; ], zip inputs [ n; n; n; n; y; y; n; ]
let test_match_string_nn3 = [ MatchString ""; MatchString "foo"; ], zip inputs [ n; n; n; n; y; y; n; ]
let test_match_string_dup2 = [ MatchString "  "; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_string_dup3 = [ MatchString "   "; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_string_dup4 = [ MatchString "    "; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_string_over1 = [ MatchString "fooBar\000"; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_string_over2 = [ MatchString "fooBarar"; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_string_over3 = [ MatchString "fooBarBaz"; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_string_over4 = [ MatchString "fooBar\000\000fooBar\000\000"; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_string_over5 = [ MatchString "foobarBaz Long String Ahead1"; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_string_big = [ MatchString "foobarBaz Long String Ahead"; ], zip inputs [ n; n; n; n; y; n; n; ]

let test_match_chars_0 = [ MatchChars []; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_chars_1 = [ MatchChars [ 'f'; ]; ], zip inputs [ n; n; n; n; y; y; n; ]
let test_match_chars_2 = [ MatchChars [ 'f'; '4'; ]; ], zip inputs [ n; n; y; n; y; y; n; ]
let test_match_chars_3 = [ MatchChars [ 'f'; 'o'; '4'; ]; ], zip inputs [ n; n; y; n; y; y; n; ]
let test_match_chars_all = [ MatchChars [ ' '; '+'; '4'; 'b'; 'f'; ]; ], zip inputs [ n; y; y; y; y; y; y; ]
let test_match_chars_fix1 = [ MatchChars [ 'f'; ]; MatchChars [ 'o'; ]; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_match_chars_fix2 = [ MatchChars [ 'f'; ]; MatchString "foo"; ], zip inputs [ n; n; n; n; y; y; n; ]
let test_match_chars_case = [ MatchString "foo"; MatchChars [ 'b'; 'B'; ]; ], zip inputs [ n; n; n; n; y; y; n; ]

let test_mismatch_chars_0 = [ MismatchChars []; ], zip inputs [ n; y; y; y; y; y; y; ]
let test_mismatch_chars_1 = [ MismatchChars [ 'f'; ]; ], zip inputs [ n; y; y; y; n; n; y; ]
let test_mismatch_chars_2 = [ MismatchChars [ 'f'; '4'; ]; ], zip inputs [ n; y; n; y; n; n; y; ]
let test_mismatch_chars_3 = [ MismatchChars [ 'f'; 'o'; '4'; ]; ], zip inputs [ n; y; n; y; n; n; y; ]
let test_mismatch_chars_all = [ MismatchChars [ ' '; '+'; '4'; 'b'; 'f'; ]; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_mismatch_chars_fix1 = [ MismatchChars [ 'f'; ]; MismatchChars [ 'o'; ]; ], zip inputs [ n; y; y; y; n; n; y; ]
let test_mismatch_chars_fix2 = [ MismatchChars [ 'f'; ]; MatchString "bar"; ], zip inputs [ n; n; n; n; n; n; y; ]
let test_mismatch_chars_case1 = [ MatchString "foo"; MismatchChars [ 'b'; 'B'; ]; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_mismatch_chars_case2 = [ MatchString "foo"; MismatchChars [ ' '; ]; ], zip inputs [ n; n; n; n; y; y; n; ]

let test_check_eos_0 = [ CheckEOS true; ], zip inputs [ y; n; n; n; n; n; n; ]
let test_check_eos_1 = [ CheckEOS false; ], zip inputs [ n; y; y; y; y; y; y; ]
let test_check_eos_2 = [ Skip 3; CheckEOS true], zip inputs [ n; n; n; y; n; n; n; ]
let test_check_eos_3 = [ Skip 3; CheckEOS false], zip inputs [ n; n; n; n; y; y; y; ]
let test_check_eos_4 = [ MatchString "foo"; CheckEOS true; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_check_eos_5 = [ MatchString "foo"; CheckEOS false; ], zip inputs [ n; n; n; n; y; y; n; ]
let test_check_eos_6 = [ Skip 7; CheckEOS true; ], zip inputs [ n; n; n; n; n; n; y; ]
let test_check_eos_7 = [ Skip 7; CheckEOS false; ], zip inputs [ n; n; n; n; y; n; n; ]
let test_check_eos_dup1 = [ CheckEOS true; CheckEOS true; ], zip inputs [ y; n; n; n; n; n; n; ]
let test_check_eos_dup2 = [ CheckEOS false; CheckEOS false; ], zip inputs [ n; y; y; y; y; y; y; ]
let test_check_eos_contr1 = [ CheckEOS true; CheckEOS false; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_check_eos_contr2 = [ CheckEOS false; CheckEOS true; ], zip inputs [ n; n; n; n; n; n; n; ]

let test_skip_to_char_0 = [ SkipToChar ' '; ], zip inputs [ n; y; n; n; y; n; y; ]
let test_skip_to_char_1 = [ SkipToChar '2'; ], zip inputs [ n; n; y; n; n; n; n; ]
let test_skip_to_char_2a = [ SkipToChar '+'; ], zip inputs [ n; n; n; y; n; n; n; ]
let test_skip_to_char_2b = [ SkipToChar '+'; SkipToChar '+'; SkipToChar '+'; ], zip inputs [ n; n; n; y; n; n; n; ]
let test_skip_to_char_3a = [ SkipToChar 'a'; ], zip inputs [ n; n; n; n; y; y; y; ]
let test_skip_to_char_3b = [ SkipToChar 'a'; SkipToChar 'a'; ], zip inputs [ n; n; n; n; y; n; y; ]
let test_skip_to_char_4 = [ SkipToChar 'b'; ], zip inputs [ n; n; n; n; y; n; y; ]
let test_skip_to_char_5 = [ SkipToChar 'B'; SkipToChar ' '; ], zip inputs [ n; n; n; n; y; n; n; ]
let test_skip_to_char_mov1 = [ SkipToChar ' '; MatchString "Long"; ], zip inputs [ n; n; n; n; y; n; n; ]
let test_skip_to_char_mov2 = [ SkipToChar 'B'; MatchString "a"; ], zip inputs [ n; n; n; n; y; y; n; ]

let test_and_0 = [ And [ [ SkipToChar ' '; ]; [ MatchString "foo"; ]; ]; ], zip inputs [ n; n; n; n; y; n; n; ]
let test_and_1 = [ And [ [ MismatchChars [ 'f'; ]; ]; [ Skip 3; ]; ]; ], zip inputs [ n; n; n; y; n; n; y; ]
let test_and_2 = [ SkipToChar 'a'; And [ [ Skip 3; ]; [ MatchChars [ 'r'; ]; ]; ]; ], zip inputs [ n; n; n; n; y; n; y; ]
let test_and_3 = [ And [ [ SkipToChar ' '; ]; [ CheckEOS false; ]; ]; ], zip inputs [ n; y; n; n; y; n; y; ]
let test_and_4 = [ And [ [ Skip 7; ]; [ CheckEOS false; ]; ]; ], zip inputs [ n; n; n; n; y; n; y; ]
let test_and_pre = [ And [ [ MatchString "foo"; ]; [ MatchString "f"; ]; ]; ], zip inputs [ n; n; n; n; y; y; n; ]
let test_and_contr = [ And [ [ MatchString "foo"; ]; [ MatchString "bar"; ]; ]; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_and_fix = [ And [ [ MatchString "foo"; ]; [ Skip 7; ]; ]; MatchString "foobarBaz"; ], zip inputs [ n; n; n; n; y; n; n; ]

let test_or_0 = [ Or [ [ SkipToChar ' '; ]; [ MatchString "foo"; ]; ]; ], zip inputs [ n; y; n; n; y; y; y; ]
let test_or_1 = [ Or [ [ MismatchChars [ 'f'; ]; ]; [ Skip 3; ]; ]; ], zip inputs [ n; y; y; y; y; y; y; ]
let test_or_2 = [ SkipToChar 'a'; Or [ [ Skip 3; ]; [ MatchChars [ 'r'; ]; ]; ]; ], zip inputs [ n; n; n; n; y; y; y; ]
let test_or_3 = [ SkipToChar 'r'; Or [ [ CheckEOS true; ]; [ MatchString " "; ]; ]; ], zip inputs [ n; n; n; n; n; y; y; ]
let test_or_4 = [ Or [ [ SkipToChar '/'; ]; [ MatchString "foo"; ]; ]; ], zip inputs [ n; n; n; n; y; y; n; ]
let test_or_5 = [ Or [ [ Skip 100; ]; [ MatchString "bar"; ]; ]; ], zip inputs [ n; n; n; n; n; n; y; ]
let test_or_pre = [ Or [ [ MatchString "foo"; ]; [ MatchString "f"; ]; ]; ], zip inputs [ n; n; n; n; y; y; n; ]
let test_or_contr = [ Or [ [ MatchString "foo"; ]; [ MatchString "bar"; ]; ]; ], zip inputs [ n; n; n; n; y; y; y; ]
let test_or_mov1 = [ Or [ [ MatchString "foo"; ]; [ Skip 7; ]; ]; MatchString "barBaz"; ], zip inputs [ n; n; n; n; y; n; n; ]
let test_or_mov2 = [ Or [ [ Skip 7; ]; [ MatchString "foo"; ]; ]; MatchString "barBaz"; ], zip inputs [ n; n; n; n; n; n; n; ]
let test_or_mov3 = [ Or [ [ Skip 7; ]; [ MatchString "foo"; ]; ]; MatchString "az"; ], zip inputs [ n; n; n; n; y; n; n; ]
let test_or_mov4 = [ Or [ [ Skip 7; ]; [ MatchString "foo"; ]; ]; CheckEOS true; ], zip inputs [ n; n; n; n; n; n; y; ]

let test_not_fix1 = [ Not [ MatchString "foo"; ]; MatchString "bar"; ], zip inputs [ n; n; n; n; n; n; y; ]
let test_not_fix2 = [ Not [ MatchString "bar"; ]; MatchString "foo"; ], zip inputs [ n; n; n; n; y; y; n; ]

let tests = [
  "skip_0", test_skip_0;
  "skip_n", test_skip_n;
  "skip_nn", test_skip_nn;
  "skip_big", test_skip_big;
  "match_string_0", test_match_string_0;
  "match_string_1", test_match_string_1;
  "match_string_2", test_match_string_2;
  "match_string_3", test_match_string_3;
  "match_string_4", test_match_string_4;
  "match_string_5", test_match_string_5;
  "match_string_6", test_match_string_6;
  "match_string_7", test_match_string_7;
  "match_string_8", test_match_string_8;
  "match_string_9", test_match_string_9;
  "match_string_rev_2", test_match_string_rev_2;
  "match_string_rev_3a", test_match_string_rev_3a;
  "match_string_rev_3b", test_match_string_rev_3b;
  "match_string_rev_4a", test_match_string_rev_4a;
  "match_string_rev_4b", test_match_string_rev_4b;
  "match_string_rev_7a", test_match_string_rev_7a;
  "match_string_rev_7b", test_match_string_rev_7b;
  "match_string_rev_7c", test_match_string_rev_7c;
  "match_string_rev_8a", test_match_string_rev_8a;
  "match_string_rev_8b", test_match_string_rev_8b;
  "match_string_rev_8c", test_match_string_rev_8c;
  "match_string_rev_9a", test_match_string_rev_9a;
  "match_string_rev_9b", test_match_string_rev_9b;
  "match_string_nn0", test_match_string_nn0;
  "match_string_nn1", test_match_string_nn1;
  "match_string_nn2", test_match_string_nn2;
  "match_string_nn3", test_match_string_nn3;
  "match_string_dup2", test_match_string_dup2;
  "match_string_dup3", test_match_string_dup3;
  "match_string_dup4", test_match_string_dup4;
  "match_string_over1", test_match_string_over1;
  "match_string_over2", test_match_string_over2;
  "match_string_over3", test_match_string_over3;
  "match_string_over4", test_match_string_over4;
  "match_string_over5", test_match_string_over5;
  "match_string_big", test_match_string_big;
  "match_chars_0", test_match_chars_0;
  "match_chars_1", test_match_chars_1;
  "match_chars_2", test_match_chars_2;
  "match_chars_3", test_match_chars_3;
  "match_chars_all", test_match_chars_all;
  "match_chars_fix1", test_match_chars_fix1;
  "match_chars_fix2", test_match_chars_fix2;
  "match_chars_case", test_match_chars_case;
  "mismatch_chars_0", test_mismatch_chars_0;
  "mismatch_chars_1", test_mismatch_chars_1;
  "mismatch_chars_2", test_mismatch_chars_2;
  "mismatch_chars_3", test_mismatch_chars_3;
  "mismatch_chars_all", test_mismatch_chars_all;
  "mismatch_chars_fix1", test_mismatch_chars_fix1;
  "mismatch_chars_fix2", test_mismatch_chars_fix2;
  "mismatch_chars_case1", test_mismatch_chars_case1;
  "mismatch_chars_case2", test_mismatch_chars_case2;
  "check_eos_0", test_check_eos_0;
  "check_eos_1", test_check_eos_1;
  "check_eos_2", test_check_eos_2;
  "check_eos_3", test_check_eos_3;
  "check_eos_4", test_check_eos_4;
  "check_eos_5", test_check_eos_5;
  "check_eos_6", test_check_eos_6;
  "check_eos_7", test_check_eos_7;
  "check_eos_dup1", test_check_eos_dup1;
  "check_eos_dup2", test_check_eos_dup2;
  "check_eos_contr1", test_check_eos_contr1;
  "check_eos_contr2", test_check_eos_contr2;
  "skip_to_char_0", test_skip_to_char_0;
  "skip_to_char_1", test_skip_to_char_1;
  "skip_to_char_2a", test_skip_to_char_2a;
  "skip_to_char_2b", test_skip_to_char_2b;
  "skip_to_char_3a", test_skip_to_char_3a;
  "skip_to_char_3b", test_skip_to_char_3b;
  "skip_to_char_4", test_skip_to_char_4;
  "skip_to_char_5", test_skip_to_char_5;
  "skip_to_char_mov1", test_skip_to_char_mov1;
  "skip_to_char_mov2", test_skip_to_char_mov2;
  "and_0", test_and_0;
  "and_1", test_and_1;
  "and_2", test_and_2;
  "and_3", test_and_3;
  "and_4", test_and_4;
  "and_pre", test_and_pre;
  "and_contr", test_and_contr;
  "and_fix", test_and_fix;
  "or_0", test_or_0;
  "or_1", test_or_1;
  "or_2", test_or_2;
  "or_3", test_or_3;
  "or_4", test_or_4;
  "or_5", test_or_5;
  "or_pre", test_or_pre;
  "or_contr", test_or_contr;
  "or_mov1", test_or_mov1;
  "or_mov2", test_or_mov2;
  "or_mov3", test_or_mov3;
  "or_mov4", test_or_mov4;
  "not_fix1", test_not_fix1;
  "not_fix2", test_not_fix2;
]

let not_tests =
  List.map begin fun (name, (prog, inputs)) ->
    let not_inputs = List.map (fun (input, output) -> input, 1 - output) inputs in
    "not_" ^ name, ([ Not prog; ], not_inputs)
  end tests

let with_bpf f prog =
  let open Ubpf in
  let prog = Ubpf.load (`Bpf prog) in
  try
    jit_compile prog;
    let res = f (exec prog) in
    release prog;
    res
  with exn ->
    release prog;
    raise exn

let () =
  (tests @ not_tests) |>
  List.iter begin fun (name, (prog, inputs)) ->
    printf "test %s ..." name;
    try
      make prog |> assemble |>
      with_bpf begin fun f ->
        List.iter begin fun (input, output) ->
          assert (f input = output);
          printf ".";
        end inputs
      end;
      printf " OK\n"
    with exn ->
      printf " FAILURE!\n";
      raise exn
  end
