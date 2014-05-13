(*
  yaku.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Utils
open Types
open ExtList

type t = yaku

let int_of_yaku yaku : int =
  Obj.magic yaku
;;

let yaku_of_int ycode : yaku =
  Obj.magic ycode
;;

let string_of_yaku = function
  | RICHI -> "RICHI"
  | TUMO_AGARI -> "TUMO_AGARI"
  | RINSHAN_TUMO -> "RINSHAN_TUMO"
  | HAITEI_TUMO -> "HAITEI_TUMO"
  | IPPATSU -> "IPPATSU"
  | TANYAO -> "TANYAO"
  | PINFU -> "PINFU"
  | IPEKO -> "IPEKO"
  | YAKUHAI_TON -> "YAKUHAI_TON"
  | YAKUHAI_TON2 -> "YAKUHAI_TON2"
  | YAKUHAI_NAN -> "YAKUHAI_NAN"
  | YAKUHAI_NAN2 -> "YAKUHAI_NAN2"
  | YAKUHAI_SHA -> "YAKUHAI_SHA"
  | YAKUHAI_PEI -> "YAKUHAI_PEI"
  | YAKUHAI_HAKU -> "YAKUHAI_HAKU"
  | YAKUHAI_HATSU -> "YAKUHAI_HATSU"
  | YAKUHAI_CHUN -> "YAKUHAI_CHUN"

  | ITTSU -> "ITTSU"
  | DOUBLE_RICHI -> "DOUBLE_RICHI"
  | ITTSU_NAKI -> "ITTSU_NAKI"
  | CHANTA -> "CHANTA"
  | CHANTA_NAKI -> "CHANTA_NAKI"
  | SANANKO -> "SANANKO"
  | SANKANTSU -> "SANKANTSU"
  | SANSHOKU_DOJUN -> "SANSHOKU_DOJUN"
  | SANSHOKU_DOJUN_NAKI -> "SANSHOKU_DOJUN_NAKI"
  | SANSHOKU_DOKO -> "SANSHOKU_DOKO"
  | SANSHOKU_DOKO_NAKI -> "SANSHOKU_DOKO_NAKI"
  | TOITOI -> "TOITOI"
  | CHITOITSU -> "CHITOITSU"

  | JUNCHANTA -> "JUNCHANTA"
  | JUNCHANTA_NAKI -> "JUNCHANTA_NAKI"
  | RYANPEKO -> "RYANPEKO"
  | HONITSU -> "HONITSU"
  | HONITSU_NAKI -> "HONITSU_NAKI"

  | SHOSANGEN -> "SHOSANGEN"
  | HONROTO -> "HONROTO"

  | CHINITSU -> "CHINITSU"
  | CHINITSU_NAKI -> "CHINITSU_NAKI"

  | TIHO -> "TIHO"
  | TENHO -> "TENHO"
  | SUANKO -> "SUANKO"
  | SUANKO2 -> "SUANKO2"
  | SUKANTSU -> "SUKANTSU"
  | DAISANGEN -> "DAISANGEN"
  | DAISUSHI -> "DAISUSHI"
  | SHOSUSHI -> "SHOSUSHI"
  | CHINROTO -> "CHINROTO"
  | TUISO -> "TUISO"
  | RYUISO -> "RYUISO"
  | CHURENPOTO -> "CHURENPOTO"
  | KOKUSHI -> "KOKUSHI"
;;

let yaku_count = function
  | RICHI
  | TUMO_AGARI
  | RINSHAN_TUMO
  | HAITEI_TUMO
  | IPPATSU
  | TANYAO
  | PINFU
  | IPEKO
  | YAKUHAI_TON
  | YAKUHAI_NAN
  | YAKUHAI_SHA
  | YAKUHAI_PEI
  | YAKUHAI_HAKU
  | YAKUHAI_HATSU
  | YAKUHAI_CHUN

  | SANSHOKU_DOJUN_NAKI
  | SANSHOKU_DOKO_NAKI
  | ITTSU_NAKI
  | CHANTA_NAKI
  | YAKUHAI_TON2
  | YAKUHAI_NAN2 -> 1

  | DOUBLE_RICHI
  | ITTSU
  | CHANTA
  | SANANKO
  | SANKANTSU
  | SANSHOKU_DOJUN
  | SANSHOKU_DOKO
  | TOITOI
  | CHITOITSU
  | JUNCHANTA_NAKI
  | HONITSU_NAKI -> 2

  | JUNCHANTA
  | RYANPEKO
  | HONITSU -> 3

  | SHOSANGEN
  | HONROTO -> 4

  | CHINITSU_NAKI -> 5

  | CHINITSU -> 6

  | TIHO
  | TENHO
  | SUANKO
  | SUANKO2
  | SUKANTSU
  | DAISANGEN
  | DAISUSHI
  | SHOSUSHI
  | CHINROTO
  | TUISO
  | RYUISO
  | CHURENPOTO
  | KOKUSHI -> 14
;;

let min_yaku = int_of_yaku RICHI
;;

let max_yaku = int_of_yaku KOKUSHI
;;

let min_yaku_count = 1
;;

let max_yaku_count = 14
;;

let is_yakuman yaku =
  yaku_count yaku = max_yaku_count
;;

let all =
  List.init (max_yaku - min_yaku + 1) (fun i -> i)
  +> List.map yaku_of_int
;;
  
let yakuhai_kaze =
  [YAKUHAI_TON; YAKUHAI_NAN; YAKUHAI_SHA; YAKUHAI_PEI]
;;

let yakuhai_sangen =
  [YAKUHAI_HAKU; YAKUHAI_HATSU; YAKUHAI_CHUN]
;;

let conflict_table =
  [(YAKUHAI_TON2,   [YAKUHAI_TON]);
   (YAKUHAI_NAN2,   [YAKUHAI_NAN]);
   (RYANPEKO,       [CHITOITSU]);
   (ITTSU,          [ITTSU_NAKI]);
   (CHANTA,         [CHANTA_NAKI]);
   (SANSHOKU_DOJUN, [SANSHOKU_DOJUN_NAKI]);
   (SANSHOKU_DOKO,  [SANSHOKU_DOKO_NAKI]);
   (JUNCHANTA,      [CHANTA; CHANTA_NAKI; JUNCHANTA_NAKI]);
   (HONITSU,        [HONITSU_NAKI]);
   (SHOSANGEN,      [SANANKO; HONITSU] @ yakuhai_sangen);
   (HONROTO,        [CHANTA; TOITOI]);
   (CHINITSU,       [CHINITSU_NAKI]);
  ]
;;
(**
   (prefered_yaku, disabled_yaku_list) list

   when prefered_yaku is enabled,
   members of disabled_yaku_list are disabled.
*)

let merge yaku_list =
  match List.filter is_yakuman yaku_list with
    | [] ->
      List.fold_left (fun acc (prefered_yaku, disabled_yaku_list) ->
	if List.exists ((=) prefered_yaku) yaku_list then
	  List.fold_left List.remove acc disabled_yaku_list
	else acc
      ) yaku_list conflict_table

    | yakuman_list -> yakuman_list
;;

let compare yaku1 yaku2 =
  int_of_yaku yaku1 - int_of_yaku yaku2
;;

