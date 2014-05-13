open OUnit
open Mjlib
open Mjlib.Utils
open Mjlib.Types
open Mjlib.Naki

let test_chi () =
  assert_equal (mentsu_of_naki @@ Chi(P1,P1)) (Shuntsu(P1));
  assert_equal (mentsu_of_naki @@ Chi(P1,P2)) (Shuntsu(P1));
  assert_equal (mentsu_of_naki @@ Chi(P1,P3)) (Shuntsu(P1));

  assert_equal (nakihai_of_naki @@ Chi(P1,P1)) P1;
  assert_equal (nakihai_of_naki @@ Chi(P1,P2)) P2;
  assert_equal (nakihai_of_naki @@ Chi(P1,P3)) P3;

  assert_equal (is_menzen_naki @@ Chi(P1,P3)) false;
;;

let test_kan () = 
  assert_equal (mentsu_of_naki @@ Minkan(P1,TOIMEN)) (Kantsu(P1));
  assert_equal (nakihai_of_naki @@ Minkan(P1,TOIMEN)) P1;
  assert_equal (is_menzen_naki @@ Minkan(P1,TOIMEN)) false;

  assert_equal (mentsu_of_naki @@ Kakan(P1)) (Kantsu(P1));
  assert_equal (nakihai_of_naki @@ Kakan(P1)) P1;
  assert_equal (is_menzen_naki @@ Kakan(P1)) false;

  assert_equal (mentsu_of_naki @@ Ankan(P1)) (Kantsu(P1));
  assert_equal (nakihai_of_naki @@ Ankan(P1)) P1;
  assert_equal (is_menzen_naki @@ Ankan(P1)) true;
;;

let test_pon () =
  assert_equal (mentsu_of_naki @@ Pon(P1,TOIMEN)) (Koutsu(P1));
  assert_equal (is_menzen_naki @@ Pon(P1,TOIMEN)) false;
;;

let suite = "test_naki" >::: [
  "test_chi" >:: test_chi;
  "test_kan" >:: test_kan;
  "test_pon" >:: test_pon;
]
;;

