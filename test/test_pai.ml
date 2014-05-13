open OUnit
open Mjlib
open Mjlib.Utils
open Mjlib.Types
open Mjlib.Pai
open ExtList

let test_sort () =
  assert_equal (List.sort [P3;P1;P7;M1;S9] ~cmp:Pai.compare) [M1;P1;P3;P7;S9]
;;

let test_seq () =
  assert_equal (seq P1 3) [P1;P2;P3]
;;

let test_make_shun () =
  assert_equal (make_shun P1) [P1;P2;P3]
;;

let test_succ () =
  assert_equal (succ P1) P2;
  assert_equal (succ P2) P3;
  assert_equal (succ P9) S1;
  assert_equal (succ max_pai) max_pai;
;;

let test_pred () =
  assert_equal (pred min_pai) min_pai;
  assert_equal (pred P2) P1;
  assert_equal (pred P3) P2;
  assert_equal (pred S1) P9;
;;

let test_chuchanhai () =
  assert_equal (is_chuchanhai M1) false;
  assert_equal (is_chuchanhai M2) true;
  assert_equal (is_chuchanhai M9) false;
  assert_equal (is_chuchanhai P1) false;
  assert_equal (is_chuchanhai P2) true;
  assert_equal (is_chuchanhai P9) false;
  assert_equal (is_chuchanhai S1) false;
  assert_equal (is_chuchanhai S2) true;
  assert_equal (is_chuchanhai S9) false;
  assert_equal (is_chuchanhai TON) false;
  assert_equal (is_chuchanhai CHUN) false;
;;
  

let test_pnum_of_pai () =
  assert_equal (pnum_of_pai M1) 1;
  assert_equal (pnum_of_pai M2) 2;
  assert_equal (pnum_of_pai M8) 8;
  assert_equal (pnum_of_pai M9) 9;

  assert_equal (pnum_of_pai P1) 1;
  assert_equal (pnum_of_pai P2) 2;
  assert_equal (pnum_of_pai P8) 8;
  assert_equal (pnum_of_pai P9) 9;

  assert_equal (pnum_of_pai S1) 1;
  assert_equal (pnum_of_pai S2) 2;
  assert_equal (pnum_of_pai S8) 8;
  assert_equal (pnum_of_pai S9) 9;
;;

let test_family_of_pai () =
  assert_equal (family_of_pai M1) MANZ;
  assert_equal (family_of_pai M9) MANZ;
  assert_equal (family_of_pai P1) PINZ;
  assert_equal (family_of_pai P9) PINZ;
  assert_equal (family_of_pai S1) SOUZ;
  assert_equal (family_of_pai S9) SOUZ;
  assert_equal (family_of_pai TON) KAZE;
  assert_equal (family_of_pai PEI) KAZE;
  assert_equal (family_of_pai HAKU) SANG;
  assert_equal (family_of_pai HATSU) SANG;
;;

let test_less_than () =
  assert_equal (less_than M1 P1) true;
  assert_equal (less_than P1 M1) false;
  assert_equal (less_than TON NAN) true;
  assert_equal (less_than NAN TON) false;
  assert_equal (less_than PEI CHUN) true;
  assert_equal (less_than CHUN PEI) false;
;;


let suite = "test_pai" >::: [
  "sort" >:: test_sort;
  "seq" >:: test_seq;
  "make_shun" >:: test_make_shun;
  "succ" >:: test_succ;
  "pred" >:: test_pred;
  "pnum_of_pai" >:: test_pnum_of_pai;
  "chuchanhai" >:: test_chuchanhai;
  "family_of_pai" >:: test_family_of_pai;
]
;;

