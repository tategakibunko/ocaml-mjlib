open OUnit
open Mjlib
open Mjlib.Utils
open Mjlib.Types
open Mjlib.Pai

let test_count_with_bonus () =
  assert_equal (Fu.count_with_bonus (FuBonus  0)) 20;
  assert_equal (Fu.count_with_bonus (FuBonus  2)) 30;
  assert_equal (Fu.count_with_bonus (FuBonus 10)) 30;
  assert_equal (Fu.count_with_bonus (FuBonus 12)) 40;
;;

let suite = "test_fu" >::: [
  "count_with_bonus" >:: test_count_with_bonus;
]
