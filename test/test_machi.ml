open OUnit
open Mjlib
open Mjlib.Utils
open Mjlib.Types
open Mjlib.Parser
open Mjlib.Machi
open ExtList

let check_machi machi_list expect =
  let ret = List.exists (Machi.eq expect) machi_list in
  if not ret then
    print_endline @@ String.concat "\n" @@ List.map Machi.string_of_machi machi_list
  ;
  assert_equal true ret
;;

let test_ryanmen () =
  check_machi (parse_machi [P1;P1;P2;P3]) (Ryanmen(P1,P4))
;;

let test_penchan () =
  check_machi (parse_machi [S1;S1;P1;P2]) (Penchan(P3));
  check_machi (parse_machi [S1;S1;S1;S2]) (Penchan(S3))
;;

let test_kanchan () =
  check_machi (parse_machi [S1;S1;P1;P3]) (Kanchan(P2))
;;

let test_tanki () =
  check_machi (parse_machi [S1;S1;P1]) (Tanki(P1));
  check_machi (parse_machi [TON;TON;S1;S1;S1;S2]) (Tanki(S2))
;;

let test_tamen () =
  check_machi (parse_machi [M1;M9;P1;P9;S1;S9;TON;NAN;SHA;PEI;HAKU;HATSU;CHUN])
    (Tamen(Pai.yaochuhai))
;;

let test_noten () =
  assert_equal (parse_machi [M1;M2;M3;M4;TON]) []
;;

let test_fu () =
  assert_equal (Machi.fu @@ Ryanmen(P1,P4)) 0;
  assert_equal (Machi.fu @@ Shanpon(P2,P3)) 0;
  assert_equal (Machi.fu @@ Kanchan(P2)) 2;
  assert_equal (Machi.fu @@ Penchan(P2)) 2;
  assert_equal (Machi.fu @@ Tanki(P2)) 2;
;;

let suite = "test_machi" >::: [
  "ryanmen" >:: test_ryanmen;
  "penchan" >:: test_penchan;
  "kanchan" >:: test_kanchan;
  "tanki" >:: test_tanki;
  "tamen" >:: test_tamen;
  "noten" >:: test_noten;
  "fu" >:: test_fu;
]
;;

