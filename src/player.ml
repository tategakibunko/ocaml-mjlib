(*
  player.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Utils
open Types

type t = player

let create player_id jicha start_point = {
  player_id = player_id;
  jicha = jicha;
  point = start_point;
  tehai = Tehai.create [];
  kawa = [];
}
;;

let eq player1 player2 =
  player1.jicha = player2.jicha
;;

let compare player1 player2 =
  Jicha.compare player1.jicha player2.jicha
;;

let compare_point player1 player2 =
  player1.point - player2.point
;;

let compare_point_desc player1 player2 =
  player2.point - player1.point
;;

let is_parent player =
  Jicha.is_parent player.jicha
;;

let step_jicha player =
  {player with jicha = Jicha.pred player.jicha}
;;

let set_tumo player pai =
  {player with tehai = Tehai.set_tumo player.tehai pai}
;;

let set_pais player pais =
  {player with tehai = Tehai.set_pais player.tehai pais}
;;

let reset_kawa player =
  {player with kawa = []}
;;

let reset_naki_list player =
  {player with tehai = Tehai.reset_naki_list player.tehai}
;;

let find_tumo_naki player =
  Tehai.find_tumo_naki player.tehai
;;

let set_ron ron_player pai =
  {ron_player with tehai = Tehai.set_ron ron_player.tehai pai}
;;

let do_dahai ?(is_richi=false) player pai =
  let kawa_actions = if is_richi then [Richi] else [] in
  {player with
    point = if is_richi then player.point - 1000 else player.point;
    tehai = Tehai.do_dahai player.tehai pai;
    kawa = player.kawa @ [Kawahai(pai, kawa_actions)];
  }
;;

let find_dahai_naki player pai relative_pos =
  Tehai.find_dahai_naki player.tehai pai relative_pos
;;

let do_naki naki_player naki =
  {naki_player with tehai = Tehai.do_naki naki_player.tehai naki}
;;

let do_nakare player =
  let kawa' =
    match List.rev player.kawa with
      | kawahai :: rest ->
	List.rev (Kawahai.set_nakare(kawahai) :: rest)
      | [] -> [] in
  {player with kawa = kawa'}
;;

let get_kan_count player =
  Tehai.get_kan_count player.tehai
;;

let get_pais player =
  Tehai.get_pais player.tehai
;;

let get_pais_all player =
  Tehai.get_pais_all player.tehai
;;

let get_fetch player =
  Tehai.get_fetch player.tehai
;;

let is_hakoten player =
  player.point < 0
;;

let is_richi player =
  List.exists Kawahai.is_richi player.kawa
;;

let is_first_tumo player =
  List.length player.kawa = 0
;;

let is_kyushu_kyuhai player =
  Tehai.is_kyushu_kyuhai player.tehai
;;

let get_first_dahai player : kawahai option =
  match player.kawa with
    | kawahai :: _ -> Some kawahai
    | _ -> None
;;

let get_kawa_len player =
  List.length player.kawa
;;

let get_kawa player =
  player.kawa
;;

let get_tenpai player : machi list =
  Tehai.get_tenpai player.tehai
;;

let is_tenpai player =
  Tehai.is_tenpai player.tehai
;;

let is_chonbo_richi player =
  is_richi player && is_tenpai player = false
;;

let is_chonbo_ron player machi =
  Tehai.is_chonbo_ron player.tehai player.kawa machi
;;

let get_agari bafu yama player : agari list =
  Tehai.get_agari bafu yama player.jicha player.kawa player.tehai
;;

let get_agari_payment player agari =
  Tehai.get_agari_payment player.tehai player.jicha
;;

let is_nagashimangan player =
  List.for_all (fun kawahai ->
    Kawahai.is_nakare kawahai = false &&
      Kawahai.is_yaochuhai kawahai
  ) player.kawa
;;

let add_point player point_add =
  {player with point = player.point + point_add}
;;

let string_of_tehai player =
  Tehai.string_of_tehai player.tehai
;;

let count_dora player dora_list =
  Tehai.count_dora player.tehai dora_list
;;

let count_uradora player uradora_list =
  Tehai.count_dora player.tehai uradora_list
;;
