(*
  tumohai.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Utils
open Types

type t = tumohai

let fetch_tumo = function
  | [] -> raise @@ Ryukyoku(TUMO_OVER)
  | pai :: rest -> (pai, rest)
;;

let get_rest_tumo_count tumohai =
  List.length tumohai
;;
