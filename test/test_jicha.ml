open OUnit
open Mjlib
open Mjlib.Utils
open Mjlib.Types
open Mjlib.Jicha
open ExtList

let test_sub () =
  assert_equal (sub ~src:TONCHA ~dst:TONCHA) MENZEN;
  assert_equal (sub ~src:TONCHA ~dst:NANCHA) SIMOCHA;
  assert_equal (sub ~src:TONCHA ~dst:SHACHA) TOIMEN;
  assert_equal (sub ~src:TONCHA ~dst:PEICHA) KAMICHA;
;;

let test_jicha_of_relative_pos () =
  assert_equal (jicha_of_relative_pos TONCHA MENZEN) TONCHA;
  assert_equal (jicha_of_relative_pos TONCHA SIMOCHA) NANCHA;
  assert_equal (jicha_of_relative_pos TONCHA TOIMEN) SHACHA;
  assert_equal (jicha_of_relative_pos TONCHA KAMICHA) PEICHA;

  assert_equal (jicha_of_relative_pos NANCHA MENZEN) NANCHA;
  assert_equal (jicha_of_relative_pos NANCHA SIMOCHA) SHACHA;
  assert_equal (jicha_of_relative_pos NANCHA TOIMEN) PEICHA;
  assert_equal (jicha_of_relative_pos NANCHA KAMICHA) TONCHA;
;;

let suite = "test_jicha" >::: [
  "sub" >:: test_sub;
  "jicha_of_relative_pos" >:: test_jicha_of_relative_pos;
]
;;
