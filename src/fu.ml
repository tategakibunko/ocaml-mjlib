(*
  fu.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Types

let min_fu = 20
;;

let of_menzen_ron = 10
;;

let of_chitoitsu = 25
;;

let of_kokushi = 30
;;

let count_with_bonus = function
  | FuBonus(bonus) ->
    let sum = min_fu + bonus in
    let shou = sum / 10 in
    let amari = sum mod 10 in
    let kuriage = if amari > 0 then 10 else 0 in
    10 * shou + kuriage
;;
