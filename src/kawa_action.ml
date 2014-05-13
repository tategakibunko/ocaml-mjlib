(*
  kawa_action.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Types

type t = kawa_action

let int_of_kawa_action action : int =
  Obj.magic action
;;

let kawa_action_of_int i : kawa_action =
  Obj.magic i
;;
