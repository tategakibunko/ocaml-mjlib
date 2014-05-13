open OUnit
open Mjlib
open Mjlib.Utils
open Mjlib.Types
open Mjlib.Jicha
open ExtList

let test_dora_list () =
  let yama = Yama.create () in
  let dora_list = Yama.get_cur_dora_list yama in
  let uradora_list = Yama.get_cur_uradora_list yama in
  assert_equal (List.length dora_list) 1;
  assert_equal (List.length uradora_list) 1;

  let (rinshan, yama) = Yama.fetch_rinshan yama in
  let dora_list = Yama.get_cur_dora_list yama in
  let uradora_list = Yama.get_cur_uradora_list yama in
  assert_equal (List.length dora_list) 2;
  assert_equal (List.length uradora_list) 2;

  let (rinshan, yama) = Yama.fetch_rinshan yama in
  let dora_list = Yama.get_cur_dora_list yama in
  let uradora_list = Yama.get_cur_uradora_list yama in
  assert_equal (List.length dora_list) 3;
  assert_equal (List.length uradora_list) 3;


  let (rinshan, yama) = Yama.fetch_rinshan yama in
  let dora_list = Yama.get_cur_dora_list yama in
  let uradora_list = Yama.get_cur_uradora_list yama in
  assert_equal (List.length dora_list) 4;
  assert_equal (List.length uradora_list) 4;
;;

let suite = "test_yama" >::: [
  "create" >:: test_dora_list;
]
;;
