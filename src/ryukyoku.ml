(*
  ryukyoku.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Types

let string_of_ryukyoku_reason = function
  | TUMO_OVER -> "TUMO_OVER"
  | RINSHAN_OVER -> "RINSHAN_OVER"
  | SUFON(_) -> "SUFON"
  | SUKAI_KAN -> "SUKAI_KAN"
  | SUCHA_RICHI(_) -> "SUCHA_RICHI"
;;
    
