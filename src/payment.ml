(*
  payment.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Types

let is_ron = function
  | ParentRon(_) -> true
  | ChildRon(_) -> true
  | _ -> false
;;

let add_tumifu ?(player_count=4) honba payment =
  let tumifu_many = Point.of_tumifu_many ~player_count honba in
  let tumifu_single = Point.of_tumifu_single honba in

  match payment with
    | ParentTumo(point) ->
      ParentTumo(point + tumifu_many)

    | ChildTumo(ParentPay(p1), ChildPay(p2)) ->
      ChildTumo(ParentPay(p1+tumifu_many), ChildPay(p2+tumifu_many))

    | ParentRon(point) ->
      ParentRon(point + tumifu_single)

    | ChildRon(point) ->
      ChildRon(point + tumifu_single)
;;

let add_kyotaku ribou_count payment =
  let kyotaku_point = ribou_count * 1000 in
  TotalPayment(payment, kyotaku_point)
;;

let point_of_agari_payment ?(player_count = 4) = function
  | ParentTumo(point) -> point * (player_count - 1)
  | ChildTumo(ParentPay(p1), ChildPay(p2)) -> p1 + p2 * (player_count - 1)
  | ParentRon(point) -> point
  | ChildRon(point) -> point
;;

let point_of_total_payment ?(player_count = 4) = function
  | TotalPayment(agari_payment, kyotaku_point) ->
    point_of_agari_payment agari_payment + kyotaku_point
;;
    
