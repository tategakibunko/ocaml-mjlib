open OUnit
open Mjlib
open Mjlib.Utils
open Mjlib.Types
open Mjlib.Pai
open ExtList

let test_group_by () =
  assert_equal (group_by [1;2;1;3;2]) [[1;1]; [2;2]; [3]]
;;

let suite = "test_utils" >::: [
  "group_by" >:: test_group_by;
]
;;

