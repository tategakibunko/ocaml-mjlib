(*
  jifu.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Types

let int_of_jifu jifu : int =
  Obj.magic jifu
;;

let jifu_of_int i : jifu =
  Obj.magic i
;;

let is_parent = function
  | TONCHA -> true
  | _ -> false
;;

let is_child jifu =
  not (is_parent jifu)
;;

let compare jifu1 jifu2 =
  int_of_jifu jifu1 - int_of_jifu jifu2
;;

let succ = function
  | TONCHA -> NANCHA
  | NANCHA -> SHACHA
  | SHACHA -> PEICHA
  | PEICHA -> TONCHA
;;

let pred = function
  | TONCHA -> PEICHA
  | NANCHA -> TONCHA
  | SHACHA -> NANCHA
  | PEICHA -> SHACHA
;;

