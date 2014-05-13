(*
  relative_pos.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Types

type t = relative_pos

let int_of_relative_pos pos : int =
  Obj.magic pos
;;

let relative_pos_of_int i : relative_pos =
  Obj.magic i
;;

let nth n =
  relative_pos_of_int (n mod 4)
;;

let succ = function
  | MENZEN -> SIMOCHA
  | SIMOCHA -> TOIMEN
  | TOIMEN -> KAMICHA
  | KAMICHA -> MENZEN
;;

let pred = function
  | MENZEN -> KAMICHA
  | SIMOCHA -> MENZEN
  | TOIMEN -> SIMOCHA
  | KAMICHA -> TOIMEN
;;

(** lowest is highest priority *)
let atamahane_priority = function
  | SIMOCHA -> 0
  | TOIMEN -> 1
  | KAMICHA -> 2
  | MENZEN -> 99
;;

(** atamahane *)
let compare relpos1 relpos2 =
  atamahane_priority relpos1 - atamahane_priority relpos2
;;
