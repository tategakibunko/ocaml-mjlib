open OUnit
open Mjlib
open Mjlib.Utils
open Mjlib.Types
open Mjlib.Pai

let normal_tumo pai = Tumo(pai, NormalTumo)

let test_fu () =
  assert_equal (Fetch.fu (normal_tumo P1)) 2;
  assert_equal (Fetch.fu (Ron(P1))) 0;
  assert_equal (Fetch.fu Empty) 0;
;;

let suite = "test_fetch" >::: [
  "fu" >:: test_fu;
]
