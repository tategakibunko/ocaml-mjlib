(*
  yama.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Types
open Utils
open ExtList

type t = yama

let wanpai_count = 14
let haipai_count = 13

let create () =
  let rec make_pais ret i =
    if i < Pai.max_pcode then
      make_pais (i :: i :: i :: i :: ret) (i+1)
    else ret in
  (make_pais [] Pai.min_pcode)
  +> Array.of_list
  +> Utils.shuffle_array
  +> Array.map Pai.pai_of_int
  +> Array.to_list
  +> List.split_nth wanpai_count
  +> fun (pais14, pais_rest) ->

    Yama(pais_rest, Wanpai.create pais14)
;;

let is_tumohai_end = function
  | Yama([], _) -> true
  | _ -> false
;;

let tumohai_of_yama = function
  | Yama(tumohai, wanpai) -> tumohai
;;

let wanpai_of_yama = function
  | Yama(tumohai, wanpai) -> wanpai
;;

let fetch_tumo = function
  | Yama(tumohai, wanpai) ->
    let (pai, tumohai') = Tumohai.fetch_tumo tumohai in
    pai, Yama(tumohai', wanpai)
;;

let fetch_rinshan = function
  | Yama(tumohai, wanpai) ->
    let (pai, wanpai') = Wanpai.fetch_rinshan wanpai in
    pai, Yama(tumohai, wanpai')
;;

let fetch_haipai yama =
  let rec iter ret yama i  =
    if i >= haipai_count then
      (List.sort ret ~cmp:Pai.compare, yama)
    else
      let (pai, yama) = fetch_tumo yama in
      iter (pai::ret) yama (i+1) in
  iter [] yama 0
;;
      
let get_cur_dora_list = function
  | Yama(tumohai, wanpai) -> Wanpai.get_cur_dora_list wanpai
;;

let get_cur_uradora_list = function
  | Yama(tumohai, wanpai) -> Wanpai.get_cur_uradora_list wanpai
;;

let get_rest_tumo_count = function
  | Yama(tumohai, wanpai) -> Tumohai.get_rest_tumo_count tumohai
;;
