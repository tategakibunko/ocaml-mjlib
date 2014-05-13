(*
  pai.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Utils
open Types
open ExtList

type t = pai

let int_of_pai (pai:pai) : int =
  Obj.magic pai
;;

let pai_of_int (pcode:int) : pai =
  Obj.magic pcode
;;

let min_pai = M1
;;

let max_pai = CHUN
;;

let min_pcode = int_of_pai min_pai
;;

let max_pcode = int_of_pai max_pai
;;

let manz = [M1;M2;M3;M4;M5;M6;M7;M8;M9]
;;

let pinz = [P1;P2;P3;P4;P5;P6;P7;P8;P9]
;;

let souz = [S1;S2;S3;S4;S5;S6;S7;S8;S9]
;;

let suhai = manz @ pinz @ souz
;;

let kazehai = [TON;NAN;SHA;PEI]
;;

let sangenhai = [HAKU;HATSU;CHUN]
;;

let jihai = kazehai @ sangenhai
;;

let rotohai = [M1;M9;P1;P9;S1;S9]
;;

let yaochuhai = rotohai @ jihai
;;

let greenhai = [S2;S3;S4;S6;S8;HATSU]
;;

let suhai_count = 4 * 9 * 3
let kazehai_count = 4 * 4
let sangenhai_count = 4 * 3
let pai_count = suhai_count + kazehai_count + sangenhai_count

let is_suhai pai =
  List.exists ((=) pai) suhai
;;

let is_manz pai =
  List.exists ((=) pai) manz
;;

let is_pinz pai =
  List.exists ((=) pai) pinz
;;

let is_souz pai =
  List.exists ((=) pai) souz
;;

let is_kazehai pai =
  List.exists ((=) pai) kazehai
;;

let is_sangenhai pai =
  List.exists ((=) pai) sangenhai
;;

let is_jihai pai =
  int_of_pai pai >= int_of_pai TON
;;

let is_rotohai pai =
  List.exists ((=) pai) rotohai
;;

let is_yaochuhai pai =
  List.exists ((=) pai) yaochuhai
;;

let is_greenhai pai =
  List.exists ((=) pai) greenhai
;;

let family_of_pai pai =
  if is_manz pai then
    MANZ
  else if is_pinz pai then
    PINZ
  else if is_souz pai then
    SOUZ
  else if is_kazehai pai then
    KAZE
  else
    SANG
;;

let jicha_of_pai = function
  | TON -> TONCHA
  | NAN -> NANCHA
  | SHA -> SHACHA
  | PEI -> PEICHA
  | _ -> failwith "jicha_of_pai:invalid pai(not kazehai)"
;;

let bafu_of_pai = function
  | TON -> TONBA
  | NAN -> NANBA
  | _ -> failwith "bafu_of_pai:invalid pai(not kazehai)"
;;

let pnum_of_pai pai =
  match family_of_pai pai with
    | MANZ -> int_of_pai pai - int_of_pai M1 + 1
    | PINZ -> int_of_pai pai - int_of_pai P1 + 1
    | SOUZ -> int_of_pai pai - int_of_pai S1 + 1
    | _ -> failwith "pnum_of_pai:not suhai"
;;

let string_of_pai pai =
  match pai with
    | M1 | M2 | M3 | M4 | M5 | M6 | M7 | M8 | M9 -> spf "M%d" (pnum_of_pai pai)
    | P1 | P2 | P3 | P4 | P5 | P6 | P7 | P8 | P9 -> spf "P%d" (pnum_of_pai pai)
    | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 -> spf "S%d" (pnum_of_pai pai)
    | TON -> "TON"
    | NAN -> "NAN"
    | SHA -> "SHA"
    | PEI -> "PEI"
    | HAKU -> "HAKU"
    | HATSU -> "HATSU"
    | CHUN -> "CHUN"
;;

let is_chuchanhai pai =
  if is_jihai pai then
    false
  else
    let pn = pnum_of_pai pai in
    pn <> 1 && pn <> 9
;;

let seq pai count =
  let start = int_of_pai pai in
  List.init count (fun i -> start + i)
  +> List.map pai_of_int
;;

let make_shun pai =
  seq pai 3
;;

let compare p1 p2 =
  int_of_pai p1 - int_of_pai p2
;;

let succ p1 =
  pai_of_int @@ min max_pcode (int_of_pai p1 + 1)
;;

let succ_family = function
  | M9 -> M1
  | P9 -> P1
  | S9 -> S1
  | PEI -> TON
  | CHUN -> HAKU
  | pai -> succ pai
;;

let pred p1 =
  pai_of_int @@ max min_pcode (int_of_pai p1 - 1)
;;

let less_than p1 p2 =
  int_of_pai p1 < int_of_pai p2
;;

