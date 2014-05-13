(*
  game.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Utils
open Types
open ExtList

type t = game

let create ?(player_count=4) () = {
  yama = Yama.create ();
  player_count = player_count;
  bafu = TONBA;
  kyoku = 0;
  jun = 0;
  honba = 0;
  current_player_id = "";
  parent_player_id = "";
  ribou_count = 0;
  players = [];
}

let map_player game player_id fn =
  {game with
    players = List.map (fun player ->
      if player.player_id = player_id then
	fn player
      else player) game.players
  }
;;

let map_players game fn =
  {game with players = List.map fn game.players}
;;
    
let fetch_tumo game =
  let (pai, yama') = Yama.fetch_tumo game.yama in
  let game = {game with yama = yama'} in
  let game = map_player game game.current_player_id (fun current_player ->
    Player.set_tumo current_player pai
  ) in
  (pai, game)
;;

let fetch_rinshan game =
  let (pai, yama') = Yama.fetch_rinshan game.yama in
  let game = {game with yama = yama'} in
  let game = map_player game game.current_player_id (fun current_player ->
    Player.set_tumo current_player pai
  ) in
  (pai, game)
;;

let get_cur_dora_list game =
  Yama.get_cur_dora_list game.yama
;;

let get_rest_tumo_count game =
  Yama.get_rest_tumo_count game.yama
;;

let get_players game =
  game.players
;;

let get_game_player_count game =
  game.player_count
;;
  
let get_current_player_count game =
  List.length game.players
;;

let get_rest_player_count game =
  game.player_count - (get_current_player_count game)
;;

let is_waiting_game game =
  get_rest_player_count game > 0
;;

let get_current_player_id game =
  game.current_player_id
;;

let get_parent_player_id game =
  game.parent_player_id
;;

let rec create_player game player_id start_point =
  check_duplicate_player_id game player_id;
  let cur_player_count = get_current_player_count game in
  let jicha = Jicha.nth cur_player_count in
  let player = Player.create player_id jicha start_point in
  let current_player_id = if game.current_player_id = "" then player_id else game.current_player_id in
  let parent_player_id = if game.parent_player_id = "" then player_id else game.parent_player_id in
  {game with
    players = game.players @ [player]; (** push back *)
    current_player_id = current_player_id;
    parent_player_id = parent_player_id;
  }

and check_duplicate_player_id game player_id =
  if List.exists (fun p -> p.player_id = player_id) game.players then
    raise DuplicatePlayerId
;;

let remove_player game player_id =
  {game with
    players = List.filter (fun p -> p.player_id <> player_id) game.players;
  }
;;

let get_player game player_id =
  List.find (fun p -> p.player_id = player_id) game.players
;;

let get_player_by_jicha game jicha =
  List.find (fun p -> p.jicha = jicha) game.players
;;

let get_current_player game =
  get_player game game.current_player_id
;;

let get_parent_player game =
  get_player game game.parent_player_id
;;

let get_player_rank game player_id =
  let rec iter i = function
    | [] -> failwith @@ spf "get_rank: player_id %s not exists" player_id
    | player :: _ when player.player_id = player_id -> i
    | _ :: rest -> iter (i+1) rest in
  iter 0 @@ List.sort game.players ~cmp:Player.compare_point_desc
;;

let get_pais game player_id =
  Player.get_pais (get_player game player_id)
;;

let get_pais_all game player_id =
  Player.get_pais_all (get_player game player_id)
;;

let get_kawa game player_id =
  Player.get_kawa (get_player game player_id)
;;

let get_fetch game player_id =
  Player.get_fetch (get_player game player_id)
;;

let rec set_haipai game =
  let  fetch_haipai game =
    let (haipai, yama') = Yama.fetch_haipai game.yama in
    (haipai, {game with yama = yama'}) in
  List.fold_left (fun game player ->
    let (haipai, game') = fetch_haipai game in
    map_player game' player.player_id (fun player ->
      (Player.set_pais player haipai)
      +> Player.reset_kawa
      +> Player.reset_naki_list
    )
  ) game game.players
;;

let check_hakoten game =
  if List.exists Player.is_hakoten game.players then
    raise HanchanEnd
;;

let is_last_kyoku game =
  game.kyoku = 3 && game.bafu = NANBA
;;

let step_kyoku ?(is_ryukyoku=false) game =
  if is_last_kyoku game then
    raise HanchanEnd
  ;
  check_hakoten game;
  let next_kyoku = (game.kyoku + 1) mod game.player_count in
  let next_bafu = if game.kyoku = 3 then Bafu.succ game.bafu else game.bafu in
  let next_parent = Utils.rotate_next game.players (fun p -> p.player_id = game.parent_player_id) in
  let next_players = List.map Player.step_jicha game.players in
  let next_ribou_count = if is_ryukyoku then game.ribou_count else 0 in
  {game with
    yama = Yama.create ();
    kyoku = next_kyoku;
    bafu = next_bafu;
    jun = 0;
    honba = 0;
    ribou_count = next_ribou_count;
    players = next_players;
    parent_player_id = next_parent.player_id;
    current_player_id = next_parent.player_id;
  }
;;

let keep_kyoku game =
  check_hakoten game;
  {game with
    yama = Yama.create ();
    jun = 0;
    honba = game.honba + 1;
    current_player_id = game.parent_player_id
  }
;;

let step_jun game =
  let next_current = Utils.rotate_next game.players (fun p -> p.player_id = game.current_player_id) in
  {game with
    jun = game.jun + 1;
    current_player_id = next_current.player_id;
  }
;;

let find_tumo_naki game =
  Player.find_tumo_naki (get_current_player game)
;;

let is_kyushu_kyuhai game =
  let current_player = get_current_player game in
  Player.is_first_tumo current_player &&
    Player.is_kyushu_kyuhai current_player
;;

(** caution: game.current_player changes to ron_player_id *)
let set_ron game ron_player_id pai =
  let game = map_player game ron_player_id (fun ron_player ->
    Player.set_ron ron_player pai
  ) in
  {game with current_player_id = ron_player_id}
;;

let rec do_dahai ?(is_richi=false) game pai =
  let game = map_player game game.current_player_id (fun player ->
    Player.do_dahai player pai ~is_richi
  ) in
  let game =
    if is_richi then
      {game with ribou_count = game.ribou_count + 1}
    else game in
  if game.player_count = 4 then
    begin
      if game.jun = game.player_count - 1 then
	check_sufon game pai
      ;
      if is_richi then
	check_sucha_richi game pai
      ;
    end
  ;
  game

and check_sufon game pai : unit =
  let dahais = List.map (fun player ->
    Player.get_first_dahai player
  ) game.players in
  let sample = List.hd dahais in
  if List.for_all ((=) sample) dahais then
    raise @@ Ryukyoku(SUFON pai)

and check_sucha_richi game pai : unit =
  if List.for_all (fun player ->
    Player.is_richi player
  ) game.players then
    raise @@ Ryukyoku(SUCHA_RICHI pai)
;;

let is_richi game player_id =
  let player = get_player game player_id in
  Player.is_richi player
;;

let find_dahai_naki game naki_player_id pai =
  if is_richi game naki_player_id then
    []
  else
    let cur_player = get_current_player game in
    let naki_player = get_player game naki_player_id in
    let relative_pos = Jicha.sub ~src:naki_player.jicha ~dst:cur_player.jicha in
    if relative_pos = MENZEN then []
    else Player.find_dahai_naki naki_player pai relative_pos
;;

(** caution: game.current_player changes to naki_player_id *)
let rec do_naki game naki_player_id naki =
  let game = map_player game naki_player_id (fun player ->
    Player.do_naki player naki
  ) in
  ignore @@ check_sukaikan game;
  {game with
    current_player_id = naki_player_id;
    jun = game.jun + 1;
  }

and check_sukaikan game : unit =
  let (kan_player_count, kan_count) = List.fold_left (fun (kpc, kc) player ->
    match Player.get_kan_count player with
      | 0 -> (kpc, kc)
      | kan_count -> (kpc+1, kc + kan_count)
  ) (0, 0) game.players in
  if kan_count >= 4 && kan_player_count > 1 then
    raise @@ Ryukyoku(SUKAI_KAN)
;;

let do_nakare game nakare_player_id =
  map_player game nakare_player_id (fun player ->
    Player.do_nakare player
  )
;;

let get_tenpai game player_id : machi list =
  let player = get_player game player_id in
  Player.get_tenpai player
;;

let is_tenpai game player_id =
  let player = get_player game player_id in
  Player.is_tenpai player
;;

let is_chonbo_richi game player_id =
  let player = get_player game player_id in
  Player.is_chonbo_richi player
;;

let is_chonbo_ron game player_id machi =
  let player = get_player game player_id in
  Player.is_chonbo_ron player machi
;;

let get_cur_dora_list game =
  Yama.get_cur_dora_list game.yama
;;

let get_cur_uradora_list game =
  Yama.get_cur_uradora_list game.yama
;;

let get_agari game player_id : agari list =
  let player = get_player game player_id in
  Player.get_agari game.bafu game.yama player
;;

let get_tumo_agari game : agari list =
  get_agari game game.current_player_id
;;

(** raise [ChonboAgari] if there is something not formal *)
let get_agari_payment game player_id agari_list : (agari * total_payment) =
  let player = get_player game player_id in
  match agari_list with
    | [] -> raise @@ ChonboAgari(player_id, NOTEN)
    | _ ->
      if List.exists (fun agari -> Player.is_chonbo_ron player agari.machi) agari_list then
	raise @@ ChonboAgari(player_id, KAWA_CHONBO)
      ;
      let agari_max =
	List.sort agari_list ~cmp:Agari.compare_desc
	+> List.hd in
      
      let total_payment =
	Player.get_agari_payment player game.honba agari_max
	+> Payment.add_tumifu game.honba
	+> Payment.add_kyotaku game.ribou_count in
      (agari_max, total_payment)
;;

let get_nagashimangan_player game =
  if Yama.is_tumohai_end game.yama = false then
    None
  else
    match List.filter Player.is_nagashimangan game.players with
      | [] -> None
      | players ->
	Some (List.sort players ~cmp:Player.compare +> List.hd)
;;
      
let get_nagashimangan_payment game player_id =
  let player = get_player game player_id in
  let agari_payment =
    if player.jicha = TONCHA then
      ParentTumo(4000)
    else ChildTumo(ParentPay 4000, ChildPay 2000) in
  Payment.add_tumifu game.honba agari_payment
  +> Payment.add_kyotaku game.ribou_count
;;

    
let apply_point_map game point_map =
  map_players game (fun player ->
    Player.add_point player @@ List.assoc player.player_id point_map
  )
;;

let rec apply_payment_tumo game total_payment : (game * point_map) =
  let point_map : (player_id * int) list =
    make_point_map_tumo game total_payment in
  let game' = apply_point_map game point_map in
  (game', point_map)
	  
and apply_payment_ron game ~agari_player_id ~furi_player_id ~total_payment : (game * point_map) =
  let point_map : (player_id * int) list =
    make_point_map_ron game ~agari_player_id ~furi_player_id ~total_payment in
  let game' = apply_point_map game point_map in
  (game', point_map)

and make_point_map_tumo game total_payment : point_map =
  let plus = Payment.point_of_total_payment total_payment ~player_count:game.player_count in
  match total_payment with
    | TotalPayment(ParentTumo(point), kyotaku) ->
      List.map (fun player ->
	if player.player_id = game.current_player_id then
	  (player.player_id, plus)
	else
	  (player.player_id, -point)
      ) game.players
    | TotalPayment(ChildTumo(ParentPay(parent_point), ChildPay(child_point)), kyotaku) ->
      List.map (fun player ->
	if player.player_id = game.current_player_id then
	  (player.player_id, plus)
	else if player.player_id = game.parent_player_id then
	  (player.player_id, -parent_point)
	else
	  (player.player_id, -child_point)
      ) game.players
    | _ -> failwith "make_point_map_tumo:not tumo payment"

and make_point_map_ron game ~agari_player_id ~furi_player_id ~total_payment : point_map =
  let plus = Payment.point_of_total_payment total_payment ~player_count:game.player_count in
  let ron_point =
    match total_payment with
      | TotalPayment(ParentRon(point), kyotaku) -> point
      | TotalPayment(ChildRon(point), kyotaku) -> point
      | _ -> failwith "make_point_map_ron:not ron payment" in
  List.map (fun player ->
    if player.player_id = agari_player_id then
      (player.player_id, plus)
    else if player.player_id = furi_player_id then
      (player.player_id, -ron_point)
    else
      (player.player_id, 0)
  ) game.players
;;

let string_of_tehai game player_id =
  let player = get_player game player_id in
  Player.string_of_tehai player
;;

let rec apply_chonbo game chonbo_player_id =
  if chonbo_player_id = game.parent_player_id then
    apply_chonbo_parent game chonbo_player_id
  else
    apply_chonbo_child game chonbo_player_id

and apply_chonbo_parent game chonbo_player_id =
  let chonbo_point = Point.of_parent_chonbo in
  let chonbo_point_plus = chonbo_point / (game.player_count - 1) in
  let point_map = List.map (fun player ->
    if player.player_id = chonbo_player_id then
      (player.player_id, -chonbo_point)
    else
      (player.player_id, chonbo_point_plus)
  ) game.players in
  let game' = map_players game (fun player ->
    Player.add_point player @@ List.assoc player.player_id point_map
  ) in
  (game', point_map)

and apply_chonbo_child game chonbo_player_id =
  let chonbo_point = Point.of_child_chonbo in
  let chonbo_point_plus_parent = chonbo_point / 2 in
  let chonbo_point_plus_child = chonbo_point_plus_parent / 2 in
  let point_map = List.map (fun player ->
    if player.player_id = chonbo_player_id then
      (player.player_id, -chonbo_point)
    else if player.player_id = game.parent_player_id then
      (player.player_id, chonbo_point_plus_parent)
    else
      (player.player_id, chonbo_point_plus_child)
  ) game.players in
  let game' = map_players game (fun player ->
    Player.add_point player @@ List.assoc player.player_id point_map
  ) in
  (game', point_map)
;;

let rec apply_noten_payment game =
  let point_map = make_point_map_noten game in
  let game' = apply_point_map game point_map in
  (game', point_map) (** TODO *)

and make_point_map_noten game =
  let tenpai_players = List.filter (fun player ->
    is_tenpai game player.player_id
  ) game.players in
  let tenpai_count = List.length tenpai_players in
  let plus = if tenpai_count = 0 then 0 else 3000 / tenpai_count in
  let minus = plus * tenpai_count in
  
  List.map (fun player ->
    if List.exists (fun p -> p.player_id = player.player_id) tenpai_players then
      (player.player_id, plus)
    else (player.player_id, -minus)
  ) game.players
;;
