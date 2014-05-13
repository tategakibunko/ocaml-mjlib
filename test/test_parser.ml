open OUnit
open Mjlib
open Mjlib.Utils
open Mjlib.Types
open Mjlib.Parser
open ExtList

let test_cut_pais () =
  assert_equal (cut_pais [P1;P2;P3;TON;TON] [TON;TON]) [P1;P2;P3]
;;

let test_cut_mentsu () =
  assert_equal (cut_mentsu [P1;P2;P3;TON;TON] (Shuntsu(P1))) [TON;TON]
;;

let test_count_mentsu () =
  assert_equal (count_mentsu (Shuntsu(P1)) [Shuntsu(P1); Shuntsu(P1)]) 2
;;

let test_find_tumo_naki () =
  let pais = [M4;M5;M6;P2;P3;P4;P5;P6;P7;P8;P9;S1;S1] in
  let naki_list = find_tumo_naki pais [] in
  assert_equal naki_list []
;;

let suite = "test_parser" >::: [
  "cut_pais" >:: test_cut_pais;
  "cut_mentsu" >:: test_cut_mentsu;
  "count_mentsu" >:: test_count_mentsu;
  "find_tumo_naki" >:: test_find_tumo_naki;
]
;;

