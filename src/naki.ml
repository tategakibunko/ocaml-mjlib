(*
  naki.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Utils
open Types

type t = naki

let mentsu_of_naki = function
  | Pon(p,_) -> Koutsu(p)
  | Chi(p,_) -> Shuntsu(p)
  | Minkan(p,_) -> Kantsu(p)
  | Kakan(p) -> Kantsu(p)
  | Ankan(p) -> Kantsu(p)
;;

let nakihai_of_naki = function
  | Pon(p,_) -> p
  | Chi(p,nakihai) -> nakihai
  | Minkan(p,_) -> p
  | Kakan(p) -> p
  | Ankan(p) -> p
;;

let src_pos_of_naki = function
  | Pon(_,relative_pos) -> relative_pos
  | Chi(_,_) -> KAMICHA
  | Minkan(_,relative_pos) -> relative_pos
  | Kakan(_) -> MENZEN
  | Ankan(_) -> MENZEN
;;

let is_kui_naki = function
  | Pon(_,_) -> true
  | Chi(_,_) -> true
  | Minkan(_,_) -> true
  | _ -> false
;;

let is_pon = function
  | Pon(_,_) -> true
  | _ -> false
;;

let is_chi = function
  | Chi(_,_) -> true
  | _ -> false
;;

let is_ankan = function
  | Ankan(_) -> true
  | _ -> false
;;

let is_kan = function
  | Minkan(_,_) -> true
  | Kakan(_) -> true
  | Ankan(_) -> true
  | _ -> false
;;

let is_menzen_naki naki =
  is_ankan naki
;;

let is_kui_naki naki =
  not (is_menzen_naki naki)
;;

let string_of_naki = function
  | Pon(p1,_) ->
    let sp1 = Pai.string_of_pai p1 in
    spf "[%s,%s,%s]" sp1 sp1 sp1
  | Chi(start, nakihai) ->
    let i = Pai.int_of_pai start in
    let p3 = List.map Pai.pai_of_int [i; i+1; i+2] in
    spf "[%s]" (String.concat "," @@ List.map Pai.string_of_pai p3)
  | Minkan(p1,_) ->
    let sp1 = Pai.string_of_pai p1 in
    spf "[%s]" (String.concat "," [sp1; sp1; sp1; sp1])
  | Ankan(p1) ->
    let sp1 = Pai.string_of_pai p1 in
    spf "[%s]" (String.concat "," [sp1; sp1; sp1; sp1])
  | Kakan(p1) ->
    let sp1 = Pai.string_of_pai p1 in
    spf "[%s]" (String.concat "," [sp1; sp1; sp1; sp1])
;;
    
let eq naki1 naki2 =
  match naki1, naki2 with
    | Pon(p1,relpos1), Pon(p2,relpos2) -> p1 = p2 && relpos1 = relpos2
    | Chi(start1, fetch1), Chi(start2, fetch2) -> start1 = start2 && fetch1 = fetch2
    | Minkan(p1,relpos1), Minkan(p2,relpos2) -> p1 = p2 && relpos1 = relpos2
    | Kakan(p1), Kakan(p2) -> p1 = p2
    | Ankan(p1), Ankan(p2) -> p1 = p2
    | _, _ -> false
;;
