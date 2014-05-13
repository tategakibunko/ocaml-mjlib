open OUnit
open Mjlib
open Mjlib.Utils
open Mjlib.Types
open Mjlib.Mentsu
open ExtList

let test_pais_of_mentsu () =
  assert_equal (pais_of_mentsu @@ Shuntsu(P1)) [P1;P2;P3];
  assert_equal (pais_of_mentsu @@ Koutsu(P1)) [P1;P1;P1];
  assert_equal (pais_of_mentsu @@ Kantsu(P1)) [P1;P1;P1;P1];
  assert_equal (pais_of_mentsu @@ Atama(P1)) [P1;P1];
;;

let test_fu () =
  assert_equal (Mentsu.fu ~is_naki:false ~jicha:TONCHA ~bafu:NANBA @@ Koutsu(TON)) 8;
  assert_equal (Mentsu.fu ~is_naki:true  ~jicha:TONCHA ~bafu:NANBA @@ Koutsu(TON)) 4;
  assert_equal (Mentsu.fu ~is_naki:false ~jicha:TONCHA ~bafu:NANBA @@ Koutsu(P2)) 4;
  assert_equal (Mentsu.fu ~is_naki:true  ~jicha:TONCHA ~bafu:NANBA @@ Koutsu(P2)) 2;

  assert_equal (Mentsu.fu ~is_naki:false ~jicha:TONCHA ~bafu:NANBA @@ Kantsu(TON)) 32;
  assert_equal (Mentsu.fu ~is_naki:true  ~jicha:TONCHA ~bafu:NANBA @@ Kantsu(TON)) 16;
  assert_equal (Mentsu.fu ~is_naki:false ~jicha:TONCHA ~bafu:NANBA @@ Kantsu(P2)) 16;
  assert_equal (Mentsu.fu ~is_naki:true  ~jicha:TONCHA ~bafu:NANBA @@ Kantsu(P2)) 8;

  assert_equal (Mentsu.fu ~is_naki:false ~jicha:PEICHA ~bafu:NANBA @@ Atama(P1)) 0;
  assert_equal (Mentsu.fu ~is_naki:false ~jicha:PEICHA ~bafu:NANBA @@ Atama(P2)) 0;
  assert_equal (Mentsu.fu ~is_naki:false ~jicha:PEICHA ~bafu:NANBA @@ Atama(TON)) 0;
  assert_equal (Mentsu.fu ~is_naki:false ~jicha:TONCHA ~bafu:NANBA @@ Atama(TON)) 2;
  assert_equal (Mentsu.fu ~is_naki:false ~jicha:TONCHA ~bafu:TONBA @@ Atama(TON)) 4;

  assert_equal (Mentsu.fu ~is_naki:false ~jicha:TONCHA ~bafu:NANBA @@ Atama(CHUN)) 2;
;;

let suite = "test_mentsu" >::: [
  "pais_of_mentsu" >:: test_pais_of_mentsu;
  "fu" >:: test_fu;
]
