(*
  machi.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Utils
open Types
open ExtList

type t = machi

let is_ryanmen = function
  | Ryanmen(_,_) -> true
  | _ -> false
;;

let is_tanki = function
  | Tanki(_) -> true
  | _ -> false
;;

let is_waitable pai machi =
  match machi with
    | Ryanmen(p1,p2) -> pai = p1 || pai = p2
    | Shanpon(p1,p2) -> pai = p1 || pai = p2
    | Kanchan(p1) -> pai = p1
    | Penchan(p1) -> pai = p1
    | Tanki(p1) -> pai = p1
    | Tamen(pais) -> List.exists ((=) pai) pais
;;

let string_of_machi = function
  | Ryanmen(p1,p2) -> spf "Ryanmen(%s,%s)" (Pai.string_of_pai p1) (Pai.string_of_pai p2)
  | Shanpon(p1,p2) -> spf "Shanpon(%s,%s)" (Pai.string_of_pai p1) (Pai.string_of_pai p2)
  | Kanchan(p1) -> spf "Kanchan(%s)" (Pai.string_of_pai p1)
  | Penchan(p1) -> spf "Penchan(%s)" (Pai.string_of_pai p1)
  | Tanki(p1) -> spf "Tanki(%s)" (Pai.string_of_pai p1)
  | Tamen(pais) -> spf "Tamen(%s)" (List.map Pai.string_of_pai pais +> String.concat ",")
;;

let eq machi1 machi2 =
  match machi1, machi2 with
    | Ryanmen(p1,p2), Ryanmen(p1',p2') -> p1 = p1'
    | Shanpon(p1,p2), Shanpon(p1',p2') -> p1 = p1' && p2 = p2'
    | Kanchan(p1), Kanchan(p1') -> p1 = p1'
    | Penchan(p1), Penchan(p1') -> p1 = p1'
    | Tanki(p1), Tanki(p1') -> p1 = p1'
    | Tamen(pais), Tamen(pais') when List.length pais = List.length pais' ->
      let pais1 = List.sort pais  ~cmp:Pai.compare in
      let pais2 = List.sort pais' ~cmp:Pai.compare in
      List.for_all (fun (p1,p2) -> p1 = p2) (List.combine pais1 pais2)
    | _ -> false
;;

let fu = function
  | Kanchan(_) -> 2
  | Penchan(_) -> 2
  | Tanki(_) -> 2
  | _ -> 0
;;
