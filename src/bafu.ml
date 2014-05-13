(*
  bafu.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Utils
open Types

type t = bafu

let int_of_bafu = function
  | TONBA -> 0
  | NANBA -> 1
;;

let bafu_of_int i = function
  | 0 -> TONBA
  | 1 -> NANBA
  | _ -> failwith @@ spf "invalid bafu code(%d)" i
;;

let min_bafu = TONBA
;;

let max_bafu = NANBA
;;

let compare bafu1 bafu2 =
  int_of_bafu bafu1 - int_of_bafu bafu2
;;

let succ = function
  | TONBA -> NANBA
  | NANBA -> TONBA
;;

let pred = function
  | TONBA -> NANBA
  | NANBA -> TONBA
;;
