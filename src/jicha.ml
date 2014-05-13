(*
  jicha.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Utils
open Types

type t = jicha

let int_of_jicha jicha : int =
  Obj.magic jicha
;;

let jicha_of_int i : jicha =
  Obj.magic i
;;

let is_parent = function
  | TONCHA -> true
  | _ -> false
;;

let is_child jicha =
  not (is_parent jicha)
;;

let compare jicha1 jicha2 =
  int_of_jicha jicha1 - int_of_jicha jicha2
;;

let compare_desc jicha1 jicha2 =
  -(compare jicha1 jicha2)
;;

let min_jicha = TONCHA
;;

let max_jicha = PEICHA
;;

let nth n =
  jicha_of_int (n mod 4)
;;

let succ ?(player_count=4) jicha =
  let player_count = max 1 player_count in
  let next = (int_of_jicha jicha) + 1 in
  jicha_of_int (next mod player_count)
;;

let pred ?(player_count=4) jicha =
  let player_count = max 1 player_count in
  let prev = (int_of_jicha jicha) - 1 in
  if prev < 0 then jicha_of_int (player_count - 1) else
    jicha_of_int prev
;;

let sub ~src ~dst  : relative_pos =
  let distance j1 j2 =
    abs (int_of_jicha j1 - int_of_jicha j2) in
  match src, dst with
    | src, dst when src = dst -> MENZEN
    | src, dst when distance src dst = 2 -> TOIMEN
    | TONCHA, NANCHA
    | NANCHA, SHACHA
    | SHACHA, PEICHA
    | PEICHA, TONCHA -> SIMOCHA
    | _ -> KAMICHA
;;

let jicha_of_relative_pos src relative_pos =
  let dst_i = int_of_jicha src + Relative_pos.int_of_relative_pos relative_pos in
  jicha_of_int (dst_i mod 4)
;;
    
