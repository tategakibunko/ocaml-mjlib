(*
  wanpai.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Utils
open Types
open ExtList

type t = wanpai

let rinshan_count = 4
let dorahai_count = 5

let create pais14 =
  let (pais4, pais10) = List.split_nth rinshan_count pais14 in
  let (pais5, pais5') = List.split_nth dorahai_count pais10 in
  {cur_dora_list = [List.hd pais5];
   cur_uradora_list = [List.hd pais5'];
   dora_list = List.tl pais5;
   uradora_list = List.tl pais5';
   rinshanhai = pais4;
  }
;;

let fetch_rinshan wanpai =
  match wanpai.rinshanhai with
    | [] -> raise @@ Ryukyoku(RINSHAN_OVER)
    | rinshan :: rinshan_rest ->
      (match wanpai.dora_list, wanpai.uradora_list with
	| dora1 :: dora_rest, uradora1 :: uradora_rest ->
	  rinshan, {
	    cur_dora_list = dora1 :: wanpai.cur_dora_list;
	    cur_uradora_list = uradora1 :: wanpai.cur_uradora_list;
	    rinshanhai = rinshan_rest;
	    dora_list = dora_rest;
	    uradora_list = uradora_rest;
	  }
	| _ -> failwith "fetch_rinshan:dora_list not exists")
;;

let get_cur_dora_list wanpai =
  wanpai.cur_dora_list
;;

let get_cur_uradora_list wanpai =
  wanpai.cur_uradora_list
;;

