(*
  mentsu.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Utils
open Types
open ExtList

type t = mentsu

let pai_of_mentsu = function
  | Shuntsu p -> p
  | Koutsu p -> p
  | Kantsu p -> p
  | Atama p -> p
;;

let pais_of_mentsu = function
  | Shuntsu p -> Pai.make_shun p
  | Koutsu p -> [p; p; p]
  | Kantsu p -> [p; p; p; p]
  | Atama p -> [p; p]
;;

let string_of_mentsu m =
  let pai_str = pai_of_mentsu m +> Pai.string_of_pai in
  match m with
    | Shuntsu p -> spf "Shuntsu(%s)" pai_str
    | Koutsu p -> spf "Koutsu(%s)" pai_str
    | Kantsu p -> spf "Kantsu(%s)" pai_str
    | Atama p -> spf "Atama(%s)" pai_str
;;

let is_shun = function
  | Shuntsu _ -> true
  | _ -> false
;;

let is_kou = function
  | Koutsu _ -> true
  | Kantsu _ -> true
  | _ -> false
;;

let is_kan = function
  | Kantsu _ -> true
  | _ -> false
;;

let is_atama = function
  | Atama _ -> true
  | _ -> false
;;

let is_junchan = function
  | Shuntsu p ->
    let num = Pai.pnum_of_pai p in
    List.exists ((=) num) [1; 7]
  | Koutsu p -> Pai.is_rotohai p
  | Kantsu p -> Pai.is_rotohai p
  | Atama p -> Pai.is_rotohai p
;;

let is_chanta = function
  | Shuntsu p -> is_junchan (Shuntsu p)
  | Koutsu p -> Pai.is_yaochuhai p
  | Kantsu p -> Pai.is_yaochuhai p
  | Atama p -> Pai.is_yaochuhai p
;;

let is_roto = function
  | Koutsu p -> Pai.is_rotohai p
  | Kantsu p -> Pai.is_rotohai p
  | Atama p -> Pai.is_rotohai p
  | _ -> false
;;

let is_sangen m =
  pai_of_mentsu m +> Pai.is_sangenhai 
;;

let is_kaze m =
  pai_of_mentsu m +> Pai.is_kazehai 
;;

let is_tanyao m =
  not (is_chanta m)
;;

let int_of_mentsu_kind = function
  | Shuntsu _ -> 0
  | Koutsu _ -> 1
  | Kantsu _ -> 2
  | Atama _ -> 3
;;

let compare m1 m2 =
  let k1 = int_of_mentsu_kind m1 in
  let k2 = int_of_mentsu_kind m2 in
  if k1 <> k2 then
    k1 - k2
  else
    Pai.compare (pai_of_mentsu m1) (pai_of_mentsu m2)
;;

let eq m1 m2 =
  compare m1 m2 = 0
;;
    
let eq_mlist mlist1 mlist2 =
  if List.length mlist1 <> List.length mlist2 then
    false
  else
    List.combine (List.sort mlist1 ~cmp:compare) (List.sort mlist2 ~cmp:compare)
    +> List.for_all (fun (m1,m2) -> m1 = m2)
;;

let rec fu ~is_naki ~jicha ~bafu mentsu =
  let fix point = if is_naki then point / 2 else point in
  let is_jicha p = try jicha = Pai.jicha_of_pai p with e -> false in
  let is_bafu p = try bafu = Pai.bafu_of_pai p with e -> false in
  match mentsu with
    | Koutsu(p) when Pai.is_yaochuhai p -> fix 8
    | Koutsu(p) -> fix 4
    | Atama(p) when Pai.is_sangenhai p -> 2
    | Atama(p) when Pai.is_kazehai p ->
      List.fold_left (fun sum (checker, plus) ->
	if checker p then sum + plus else sum
      ) 0 [(is_jicha, 2); (is_bafu, 2)]
    | Kantsu(p) -> 4 * (fu ~is_naki ~jicha ~bafu @@ Koutsu(p))
    | _ -> 0
;;

