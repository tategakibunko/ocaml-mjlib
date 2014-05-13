(*
  kawahai.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Types

type t = kawahai

let pai_of_kawahai = function
  | Kawahai(pai,actions) -> pai
;;

let is_nakare = function
  | Kawahai(pai, actions) ->
    List.exists (function Nakare -> true | _ -> false) actions
;;

let is_yaochuhai = function
  | Kawahai(pai, _) -> Pai.is_yaochuhai pai
;;

let is_richi = function
  | Kawahai(pai, actions) ->
    List.exists (function Richi -> true | _ -> false) actions
;;

let is_richi_but_nakare pai =
  is_richi pai = true && is_nakare pai = false

let set_nakare = function
  | Kawahai(pai, actions) -> Kawahai(pai, Nakare :: actions)
;;
