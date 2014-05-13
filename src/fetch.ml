(*
  fetch.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Utils
open Types

type t = fetch

let pai_of_fetch = function
  | Tumo(p1,_) -> p1
  | Ron p1 -> p1
  | Empty -> failwith "fetch pai is not set"
;;

let is_tumo = function
  | Tumo(_,_) -> true
  | _ -> false
;;

let is_rinshan_tumo = function
  | Tumo(_, RinshanTumo) -> true
  | _ -> false
;;

let is_haitei_tumo = function
  | Tumo(_, HaiteiTumo) -> true
  | _ -> false
;;

let is_ron = function
  | Ron _ -> true
  | _ -> false
;;

let is_empty = function
  | Empty -> true
  | _ -> false
;;

let fu = function
  | Tumo(_,_) -> 2
  | _ -> 0
;;

let string_of_fetch = function
  | Empty -> "Empty"
  | Tumo(p1,_) -> spf "%s(tumo)" (Pai.string_of_pai p1)
  | Ron(p1) -> spf "%s(ron)" (Pai.string_of_pai p1)
;;
