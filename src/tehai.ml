(*
  tehai.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Utils
open Types
open ExtList

type t = tehai

let create pais =
  {naki_list = []; pais = pais; fetch = Empty}
;;

let set_pais tehai pais =
  {tehai with pais = pais}
;;

let reset_naki_list tehai =
  {tehai with naki_list = []}
;;

let set_tumo ?(tumo_action=NormalTumo) tehai pai =
  {tehai with fetch = Tumo(pai,tumo_action)}
;;

let find_tumo_naki tehai =
  match tehai.fetch with
    | Tumo(pai,_) ->
      Parser.find_tumo_naki (pai :: tehai.pais) tehai.naki_list
    | _ -> []
;;

let set_ron tehai pai =
  {tehai with fetch = Ron pai}
;;

let do_dahai tehai pai =
  match tehai.fetch with
    | Empty -> {tehai with pais = List.remove tehai.pais pai}
    | Ron(p) -> failwith "do_dahai:ron is already set"
    | Tumo(p,_) ->
      {tehai with
	fetch = Empty;
	pais = List.remove (p :: tehai.pais) pai +> List.sort ~cmp:Pai.compare;
      }
;;

let find_dahai_naki tehai pai relative_pos =
  Parser.find_dahai_naki tehai.pais pai relative_pos
;;

let rec do_naki tehai naki =
  match naki, tehai.fetch with
    | Kakan(pai), Tumo(tumohai, _) when pai = tumohai ->
      {tehai with
	fetch = Empty;
	naki_list = apply_kakan naki tehai.naki_list;
      }

    | Kakan(pai), Tumo(tumohai, _) ->
      {tehai with
	pais = Parser.cut_pais tehai.pais [pai];
	naki_list = apply_kakan naki tehai.naki_list;
      }

    | Ankan(pai), Tumo(tumohai, _) when pai = tumohai ->
      {fetch = Empty;
       pais = Parser.cut_pais tehai.pais [pai; pai; pai];
       naki_list = naki :: tehai.naki_list;
      }

    | Ankan(pai), Tumo(tumohai, _) ->
      {tehai with
	pais = Parser.cut_pais tehai.pais [pai; pai; pai; pai];
	naki_list = naki :: tehai.naki_list;
      }

    | _, Empty ->
      let nakihai = Naki.nakihai_of_naki naki in
      let naki_mentsu = Naki.mentsu_of_naki naki in
      {tehai with
	pais = Parser.cut_mentsu (nakihai :: tehai.pais) naki_mentsu;
	naki_list = naki :: tehai.naki_list;
      }

    | _, _ ->
      failwith "tehai.do_naki: invalid naki operation"

and apply_kakan naki naki_list =
  let kakan_pai = Naki.nakihai_of_naki naki in
  naki :: List.remove_if (fun naki ->
    Naki.is_pon naki && Naki.nakihai_of_naki naki = kakan_pai
  ) naki_list
;;

let get_kan_count tehai =
  List.filter Naki.is_kan tehai.naki_list
  +> List.length
;;

let get_pais_all tehai =
  match tehai.fetch with
    | Tumo(p1,_) -> p1 :: tehai.pais
    | Ron p1 -> p1 :: tehai.pais
    | Empty -> tehai.pais
;;

let get_pais tehai =
  tehai.pais
;;

let get_fetch tehai =
  tehai.fetch
;;

let is_kyushu_kyuhai tehai =
  Parser.is_kyushu_kyuhai tehai.pais
;;

let get_tenpai tehai : machi list =
  Parser.parse_machi tehai.pais
;;

let is_tenpai tehai =
  get_tenpai tehai <> []
;;

let is_chonbo_ron tehai kawa machi =
  Fetch.is_ron tehai.fetch && List.exists (fun kawahai ->
    Machi.is_waitable (Kawahai.pai_of_kawahai kawahai) machi
  ) kawa
;;  

let get_agari bafu yama jicha kawa tehai : agari list =
  Parser.parse_agari bafu yama jicha kawa tehai
;;

let get_agari_payment ?(player_count=4) tehai jicha agari =
  match jicha, tehai.fetch with
    | TONCHA, Tumo(_) ->
      ParentTumo(Point.of_parent_tumo ~yaku_count:agari.yaku_count ~fu_count:agari.fu_count)

    | TONCHA, Ron(_) ->
      ParentRon(Point.of_parent_ron ~yaku_count:agari.yaku_count ~fu_count:agari.fu_count)

    | _, Tumo(_) ->
      let (child, parent) = Point.of_child_tumo ~yaku_count:agari.yaku_count ~fu_count:agari.fu_count in
      ChildTumo(ParentPay(parent), ChildPay(child))

    | _, Ron(_) ->
      ChildRon(Point.of_child_ron ~yaku_count:agari.yaku_count ~fu_count:agari.fu_count)

    | _, Empty ->
      failwith "get_agari_payment:tehai.fetch is Empty"
;;

let rec string_of_tehai tehai =
  String.concat ", " [
    spf "menzen:%s, [%s]"  (string_of_menzen_pais tehai.pais) (Fetch.string_of_fetch tehai.fetch);
    spf "naki:%s" (string_of_naki_list tehai.naki_list);
  ]

and string_of_menzen_pais pais =
  List.map Pai.string_of_pai pais
  +> String.concat ","

and string_of_naki_list naki_list =
  List.map Naki.string_of_naki naki_list
  +> String.concat ","
;;

let count_dora tehai dora_list =
  0 (** TODO *)
;;

let count_uradora tehai uradora_list =
  0 (** TODO *)
;;
