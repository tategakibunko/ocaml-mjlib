(*
  agari.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Utils
open Types

type t = agari

let eq agari1 agari2 : bool =
  Machi.eq agari1.machi agari2.machi &&
    (agari1.fu_count = agari2.fu_count) &&
    (agari1.yaku_count = agari2.yaku_count) &&
    (agari1.yaku_list = agari2.yaku_list)
;;
(** yaku_list already sorted *)

let compare agari1 agari2 =
  agari1.yaku_count - agari2.yaku_count
;;

let compare_desc agari1 agari2 =
  -(compare agari1 agari2)
;;
  
