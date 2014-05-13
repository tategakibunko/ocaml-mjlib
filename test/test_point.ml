open OUnit
open Mjlib
open Mjlib.Utils
open Mjlib.Types
open Mjlib.Jicha
open ExtList

let test_child_ron () =
  (** chitoitsu *)
  assert_equal (Point.of_child_ron ~yaku_count:2 ~fu_count:25) 1600;
  assert_equal (Point.of_child_ron ~yaku_count:3 ~fu_count:25) 3200;
  assert_equal (Point.of_child_ron ~yaku_count:4 ~fu_count:25) 6400;
  assert_equal (Point.of_child_ron ~yaku_count:5 ~fu_count:25) 8000;

  (** pinfu ron *)
  assert_equal (Point.of_child_ron ~yaku_count:1 ~fu_count:30) 1000;

  (** pinfu dora1 *)
  assert_equal (Point.of_child_ron ~yaku_count:2 ~fu_count:30) 2000;
;;

let test_child_tumo () =
  (** chitoitsu *)
  assert_equal (Point.of_child_tumo ~yaku_count:3 ~fu_count:25) (800, 1600);
  assert_equal (Point.of_child_tumo ~yaku_count:4 ~fu_count:25) (1600, 3200);
  assert_equal (Point.of_child_tumo ~yaku_count:5 ~fu_count:25) (2000, 4000);

  assert_equal (Point.of_child_tumo ~yaku_count:2 ~fu_count:30) (500, 1000);
  assert_equal (Point.of_child_tumo ~yaku_count:3 ~fu_count:30) (1000, 2000);
  assert_equal (Point.of_child_tumo ~yaku_count:4 ~fu_count:30) (2000, 3900);
;;

let test_parent_ron () =
  (** chitoitsu *)
  assert_equal (Point.of_parent_ron ~yaku_count:2 ~fu_count:25) 2400;
  assert_equal (Point.of_parent_ron ~yaku_count:3 ~fu_count:25) 4800;
  assert_equal (Point.of_parent_ron ~yaku_count:4 ~fu_count:25) 9600;
  assert_equal (Point.of_parent_ron ~yaku_count:5 ~fu_count:25) 12000;

  (** pinfu ron *)
  assert_equal (Point.of_parent_ron ~yaku_count:1 ~fu_count:30) 1500;

  (** pinfu dora1 *)
  assert_equal (Point.of_parent_ron ~yaku_count:2 ~fu_count:30) 2900;
;;

let test_parent_tumo () =
  (** chitoitsu *)
  assert_equal (Point.of_parent_tumo ~yaku_count:3 ~fu_count:25) 1600;
  assert_equal (Point.of_parent_tumo ~yaku_count:4 ~fu_count:25) 3200;
  assert_equal (Point.of_parent_tumo ~yaku_count:5 ~fu_count:25) 4000;

  assert_equal (Point.of_parent_tumo ~yaku_count:1 ~fu_count:30) 500;
  assert_equal (Point.of_parent_tumo ~yaku_count:2 ~fu_count:30) 1000;
  assert_equal (Point.of_parent_tumo ~yaku_count:3 ~fu_count:30) 2000;
  assert_equal (Point.of_parent_tumo ~yaku_count:4 ~fu_count:30) 3900;
  assert_equal (Point.of_parent_tumo ~yaku_count:5 ~fu_count:30) 4000;
;;

let suite = "test_point" >::: [
  "child_ron" >:: test_child_ron;
  "child_tumo" >:: test_child_tumo;
  "parent_ron" >:: test_parent_ron;
  "parent_tumo" >:: test_parent_tumo;
]
;;

