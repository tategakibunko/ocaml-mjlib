(*
  types.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
type pai =
    M1 | M2 | M3 | M4 | M5 | M6 | M7 | M8 | M9
  | P1 | P2 | P3 | P4 | P5 | P6 | P7 | P8 | P9
  | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9
  | TON | NAN | SHA | PEI
  | HAKU | HATSU | CHUN
;;

type kawahai = Kawahai of pai * kawa_action list
and kawa_action =
    Richi
  | Nakare

type fetch =
    Tumo of pai * tumo_context
  | Ron of pai
  | Empty
and tumo_context =
  | NormalTumo
  | RinshanTumo
  | HaiteiTumo
;;

type family = MANZ | PINZ | SOUZ | KAZE | SANG
;;

type relative_pos = MENZEN | SIMOCHA | TOIMEN | KAMICHA
;;

type jicha = TONCHA | NANCHA | SHACHA | PEICHA
;;

type bafu = TONBA | NANBA
;;

type mentsu =
    Shuntsu of pai
  | Koutsu of pai
  | Kantsu of pai
  | Atama of pai
;;

type machi =
    Ryanmen of pai * pai
  | Shanpon of pai * pai
  | Kanchan of pai
  | Penchan of pai
  | Tanki of pai
  | Tamen of pai list
;;

type naki =
    Pon of pai * relative_pos
  | Chi of mentsu_starthai * nakihai
  | Minkan of pai * relative_pos
  | Kakan of pai
  | Ankan of pai
and mentsu_starthai = pai
and nakihai = pai
;;

type fu_bonus = FuBonus of int
;;

type yaku =
    RICHI
  | TUMO_AGARI
  | RINSHAN_TUMO
  | HAITEI_TUMO
  | IPPATSU
  | TANYAO
  | PINFU
  | IPEKO
  | YAKUHAI_TON
  | YAKUHAI_TON2
  | YAKUHAI_NAN
  | YAKUHAI_NAN2
  | YAKUHAI_SHA
  | YAKUHAI_PEI
  | YAKUHAI_HAKU
  | YAKUHAI_HATSU
  | YAKUHAI_CHUN

  | DOUBLE_RICHI
  | ITTSU
  | ITTSU_NAKI
  | CHANTA
  | CHANTA_NAKI
  | SANANKO
  | SANKANTSU
  | SANSHOKU_DOJUN
  | SANSHOKU_DOJUN_NAKI
  | SANSHOKU_DOKO
  | SANSHOKU_DOKO_NAKI
  | TOITOI
  | CHITOITSU

  | JUNCHANTA
  | JUNCHANTA_NAKI
  | RYANPEKO
  | HONITSU
  | HONITSU_NAKI

  | SHOSANGEN
  | HONROTO

  | CHINITSU
  | CHINITSU_NAKI

  | TENHO
  | TIHO
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
  | KOKUSHI
;;

type agari = {
  machi: machi;
  fu_count: int;
  yaku_count: int;
  dora_count: int;
  yaku_list: yaku list;
}

type total_payment =
    TotalPayment of agari_payment * kyotaku

and agari_payment =
    ParentTumo of int
  | ChildTumo of parent_pay * child_pay
  | ParentRon of int
  | ChildRon of int

and kyotaku = int
and child_pay = ChildPay of int
and parent_pay = ParentPay of int
;;

type tehai = {
  naki_list: naki list;
  pais:pai list;
  fetch: fetch;
}

type yama = Yama of tumohai * wanpai
and tumohai = pai list
and wanpai = {
  cur_dora_list: pai list;
  cur_uradora_list: pai list;
  dora_list: pai list; (* pai * 5 *)
  uradora_list: pai list; (* pai * 5 *)
  rinshanhai: pai list; (* pai * 4 *)
}

type game = {
  bafu: bafu;
  kyoku: int;
  jun: int;
  honba: int;
  yama: yama;
  ribou_count: int;
  players: player list;
  player_count: int;
  parent_player_id: string;
  current_player_id: string;
}
and player = {
  player_id: player_id;
  jicha: jicha;
  point: int;
  tehai: tehai;
  kawa:kawahai list;
}
and player_id = string

type ryukyoku_reason =
    TUMO_OVER
  | RINSHAN_OVER
  | SUKAI_KAN
  | SUFON of pai
  | SUCHA_RICHI of pai

type point_map = (player_id * int) list
type chonbo_reason =
    NOTEN
  | KAWA_CHONBO

exception DuplicatePlayerId
exception Ryukyoku of ryukyoku_reason
exception ChonboAgari of player_id * chonbo_reason
exception HanchanEnd
