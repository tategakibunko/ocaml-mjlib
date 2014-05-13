(*
  parser.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
open Types
open Utils
open Utils.ListM
open ExtList

let cut_pais pais delete_pais =
  List.fold_left List.remove pais delete_pais
;;

let cut_mentsu pais mentsu =
  cut_pais pais @@ Mentsu.pais_of_mentsu mentsu
;;

let count_pai pai pais =
  List.filter ((=) pai) pais
  +> List.length
;;

let count_mentsu mentsu mlist =
  List.filter (Mentsu.eq mentsu) mlist
  +> List.length
;;

let count_family pais =
  List.map Pai.family_of_pai pais
  +> List.unique
  +> List.length
;;

let count_toitsu pais =
  let ps = List.unique pais in
  List.filter (fun p -> count_pai p pais == 2) ps
  +> List.length
;;

let count_ipeko mlist =
  let shuns = List.filter Mentsu.is_shun mlist in
  let uniq_shuns = List.unique shuns ~cmp:Mentsu.eq in
  List.fold_left (fun c m ->
    if count_mentsu m shuns > 1 then c + 1 else c
  ) 0 uniq_shuns
;;

let count_anko mlist =
  List.filter Mentsu.is_kou mlist
  +> List.length
;;

let count_ankan naki_list =
  List.filter Naki.is_ankan naki_list
  +> List.length
;;

let count_sang_kou mlist =
  List.filter Mentsu.is_kou mlist
  +> List.filter Mentsu.is_sangen
  +> List.length
;;

let count_sang_atama mlist =
  List.filter Mentsu.is_atama mlist
  +> List.filter Mentsu.is_sangen
  +> List.length
;;

let count_kaze_kou mlist =
  List.filter Mentsu.is_kou mlist
  +> List.filter Mentsu.is_kaze
  +> List.length
;;

let count_kaze_atama mlist =
  List.filter Mentsu.is_atama mlist
  +> List.filter Mentsu.is_kaze
  +> List.length
;;

let count_dora yama pais =
  let dora_list = Yama.get_cur_dora_list yama in
  let dora_list_succ = List.map Pai.succ_family dora_list in
  List.fold_left (fun acc dora ->
    acc + count_pai dora pais
  ) 0 dora_list_succ
;;

let count_uradora yama pais =
  let uradora_list = Yama.get_cur_uradora_list yama in
  let uradora_list_succ = List.map Pai.succ_family uradora_list in
  List.fold_left (fun acc dora ->
    acc + count_pai dora pais
  ) 0 uradora_list_succ
;;

let find_seq pai count pais =
  List.for_all (fun p ->
    List.exists ((=) p) pais
  ) @@ Pai.make_shun pai
;;

let find_same pai count pais =
  count_pai pai pais >= count
;;

let fetch_shun pai pais =
  if Pai.is_jihai pai ||
    Pai.pnum_of_pai pai > 7 ||
    find_seq pai 3 pais = false
  then
    None
  else
    Some (Shuntsu pai)
;;

let fetch_kou pai pais =
  if find_same pai 3 pais then
    Some (Koutsu pai)
  else
    None
;;

let fetch_atama pai pais =
  if find_same pai 2 pais then
    Some (Atama pai)
  else
    None
;;

let rec find_tumo_naki pais naki_list =
  List.fold_left (fun acc fn ->
    acc @ (fn pais naki_list)
  ) [] [find_ankan; find_kakan]

and find_ankan pais naki_list =
  List.fold_left (fun (rest_pais, ankans) pai ->
    if find_same pai 4 rest_pais then
      let rest_pais' = cut_pais rest_pais [pai; pai; pai; pai] in
      let ankans' = (Ankan pai) :: ankans in
      (rest_pais', ankans')
    else (rest_pais, ankans)
  ) (pais, []) pais
  +> fun (rest, ankans) ->  ankans

and find_kakan pais naki_list =
  List.fold_left (fun kakans naki ->
    let nakihai = Naki.nakihai_of_naki naki in
    if Naki.is_pon naki && List.exists ((=) nakihai) pais then
      (Kakan nakihai) :: kakans
    else kakans
  ) [] naki_list
;;

let is_kyushu_kyuhai pais =
  let yaochuhai_count = List.unique pais
  +> List.filter Pai.is_yaochuhai
  +> List.length in
  yaochuhai_count >= 9
;;

let rec find_dahai_naki pais pai relative_pos =
  List.fold_left (fun acc fn ->
    match fn pais pai relative_pos with
      | Some naki -> naki :: acc
      | None -> acc
  ) [] [find_pon; find_minkan; find_chi_left; find_chi_center; find_chi_right]

and find_pon pais pai relative_pos =
  if find_same pai 2 pais then
    Some(Pon(pai, relative_pos))
  else
    None

and find_minkan pais pai relative_pos =
  if find_same pai 3 pais then
    Some(Minkan(pai, relative_pos))
  else
    None

and find_chi_left pais pai relative_pos =
  if relative_pos <> KAMICHA then
    None
  else if Pai.is_suhai pai = false then
    None
  else if Pai.pnum_of_pai pai < 3 then
    None
  else
    let p2 = Pai.pred pai in
    let p1 = Pai.pred p2 in
    if find_p2 p1 p2 pais then
      Some(Chi(p1, pai))
    else None

and find_chi_center pais pai relative_pos =
  if relative_pos <> KAMICHA then
    None
  else if Pai.is_suhai pai = false then
    None
  else if Pai.pnum_of_pai pai = 1 || Pai.pnum_of_pai pai = 9 then
    None
  else
    let p1 = Pai.pred pai in
    let p2 = Pai.succ pai in
    if find_p2 p1 p2 pais then
      Some(Chi(p1, pai))
    else None

and find_chi_right pais pai relative_pos =
  if relative_pos <> KAMICHA then
    None
  else if Pai.is_suhai pai = false then
    None
  else if Pai.pnum_of_pai pai > 7 then
    None
  else
    let p1 = Pai.succ pai in
    let p2 = Pai.succ p1 in
    if find_p2 p1 p2 pais then
      Some(Chi(pai, pai))
    else None

and find_p2 p1 p2 pais =
  List.exists ((=) p1) pais && List.exists ((=) p2) pais
;;

let parse_a1 pais =
  List.unique pais >>= fun p1 ->
  guard (count_pai p1 pais >= 2) >>= fun _ ->
  return @@ (Atama p1, cut_mentsu pais (Atama p1))
;;

let parse_m1 pais =
  List.unique pais >>= fun start ->
  [fetch_shun start pais; fetch_kou start pais] >>= fun mopt ->
  guard (mopt <> None) >>= fun _ ->
  let m1 = from_opt mopt in
  return @@ (m1, cut_mentsu pais m1)
;;

let rec parse_a1_mn pais =
  parse_a1 pais >>= fun (a1, rest) ->
  parse_a1_mn_rest [a1] rest

and parse_a1_mn_rest mlist = function
  | [] ->
    return @@ List.sort mlist ~cmp:Mentsu.compare
  | pais ->
    parse_m1 pais >>= fun (m1, rest) ->
    parse_a1_mn_rest (m1 :: mlist) rest
;;

let parse_machi_p2 atama ps2 =
  match atama, List.sort ps2 ~cmp:Pai.compare with
    | Atama(p), [p1; p2] when p1 = p2 && p <> p1 ->
      if Pai.less_than p1 p then
	Some (Shanpon (p1,p))
      else
	Some (Shanpon (p,p1))
    | Atama(p), [p1; p2] when Pai.is_suhai p1 && Pai.is_suhai p2 ->
      let pn1 = Pai.pnum_of_pai p1 in
      if (1 <= pn1 && pn1 <= 8) && (Pai.succ p1 = p2) then
	(match pn1 with
	  | 1 -> Some (Penchan (Pai.succ p2)) (* [1, 2, x] *)
	  | 8 -> Some (Penchan (Pai.pred p1)) (* [x, 8, 9] *)
	  | _ -> Some (Ryanmen ((Pai.pred p1), (Pai.succ p2)))) (* [x, 2, 3, y], [x, 3, 4, y], ... etc *)
      else if (1 <= pn1 && pn1 <= 7) && (Pai.succ p1 == Pai.pred p2) then
	Some (Kanchan (Pai.succ p1)) (* [1, x, 3], [2, x, 4] ... etc *)
      else
	None
    | _ -> None
;;

let rec parse_machi_a1_mn pais =
  parse_a1 pais >>= fun (a1, rest) ->
  parse_machi_a1_mn_rest a1 @@ List.sort rest ~cmp:Pai.compare

and parse_machi_a1_mn_rest a1 pais =
  let plen = List.length pais in
  if plen > 2 then
    begin
      parse_m1 pais >>= fun (m1, rest) ->
      parse_machi_a1_mn_rest a1 rest
    end
  else if plen = 2 then
    (match parse_machi_p2 a1 pais with
      | Some m -> [m]
      | None -> [])
  else if plen = 1 then
    [Tanki (List.hd pais)]
  else
    []
;;

let rec parse_machi_mn pais =
  match pais with
    | [] -> []
    | [p] -> [Tanki p]
    | _ -> 
      parse_m1 pais >>= fun (m1, rest) ->
      parse_machi_mn (List.sort rest ~cmp:Pai.compare)
;;

let parse_machi_normal pais =
  ((parse_machi_a1_mn pais) @ (parse_machi_mn pais))
  +> List.unique ~cmp:Machi.eq
;;

let parse_machi_chitoitsu pais =
  if count_toitsu pais == 6 then
    (try
       let p = List.find (fun p -> count_pai p pais = 1) pais in
       [Tanki p]
     with
	 Not_found -> [])
  else
    []
;;

let parse_machi_kokushi pais =
  let uniq_pais = List.filter Pai.is_yaochuhai pais +> List.unique in
  match List.length uniq_pais with
    | 13 -> [Tamen Pai.yaochuhai]
    | 12 ->
      (* find yaochuhai that is not a menber of this pais *)
      let p1 = List.find (fun yao -> List.exists ((=) yao) uniq_pais = false) Pai.yaochuhai in
      [Tanki p1]
    | _ -> []
;;
	
let parse_machi pais =
  (parse_machi_normal pais) @ (parse_machi_chitoitsu pais) @ (parse_machi_kokushi pais)
  +> List.unique ~cmp:Machi.eq
;;

(** here is YAKU calc. assume that MENTSU is complete *)

(** 1 han *)
let is_richi kawa =
  if List.length kawa < 2 then false else
    List.exists Kawahai.is_richi_but_nakare kawa
;;
let is_tumo_agari fetch =
  Fetch.is_tumo fetch
;;

let is_ippatsu kawa =
  if kawa = [] then false else
    let last = List.last kawa in
    Kawahai.is_richi last = true &&
	Kawahai.is_nakare last = false
;;

let is_tanyao pais_all =
  List.for_all Pai.is_chuchanhai pais_all
;;

let is_pinfu machi mlist =
  Machi.is_ryanmen machi && (List.filter Mentsu.is_shun mlist +> List.length) = 4
;;

let is_ipeko mlist =
  count_ipeko mlist = 1
;;

let is_yakuhai pai mlist =
  Pai.is_jihai pai &&
    List.exists (fun m ->
      Mentsu.eq m (Koutsu(pai)) || Mentsu.eq m (Kantsu(pai))
    ) mlist
;;

(** 2 han  *)
let is_double_richi kawa =
  if kawa = [] then false else
    List.hd kawa +> Kawahai.is_richi_but_nakare
;;

let is_chanta mlist =
  List.for_all Mentsu.is_chanta mlist
;;

let is_sanshoku_dojun mlist =
  List.filter Mentsu.is_shun mlist
  +> List.map Mentsu.pai_of_mentsu
  +> List.unique
  +> List.map Pai.pnum_of_pai
  +> Utils.group_by
  +> List.exists (fun group -> List.length group >= 3)
;;

let is_sanshoku_doko mlist =
  List.filter Mentsu.is_kou mlist
  +> List.map Mentsu.pai_of_mentsu
  +> List.filter Pai.is_suhai
  +> List.unique
  +> List.map Pai.pnum_of_pai
  +> Utils.group_by
  +> List.exists (fun group -> List.length group >= 3)
;;

let is_sananko fetch machi mlist =
  match count_anko mlist, fetch, machi with
    | 3, Tumo _, _ -> true
    | 3, Ron _, Shanpon(_,_) -> false
    | 4, Ron _, Shanpon(_,_) -> true
    | 3, Ron _, _ -> true
    | _ -> false
;;

let is_sankantsu ankan_list kui_mlist =
  let ankan_count = List.length ankan_list in
  let nakikan_count = List.filter Mentsu.is_kan kui_mlist +> List.length in
  ankan_count + nakikan_count = 3
;;

let is_chitoitsu machi pais =
  match count_toitsu pais, machi with
    | 7, Tanki(_) -> true
    | _ -> false
;;

let is_ittsu mlist =
  let shun_pais =
    List.filter Mentsu.is_shun mlist
    +> List.map Mentsu.pai_of_mentsu
    +> List.unique in
  let ittsu_patterns = [
    [M1; M4; M7];
    [P1; P4; P7];
    [S1; S4; S7];
  ] in
  List.exists (fun ittsu_pat ->
    List.for_all (fun ittsu_pai ->
      List.exists ((=) ittsu_pai) shun_pais
    ) ittsu_pat
  ) ittsu_patterns
;;
    
let is_toitoi mlist =
  count_anko mlist = 4
;;

(** 3 han *)
let is_ryanpeko mlist =
  count_ipeko mlist = 2
;;

let is_junchanta mlist =
  List.for_all Mentsu.is_junchan mlist
;;

let is_honitsu pais =
  count_family (List.filter Pai.is_suhai pais) = 1 &&
  List.exists Pai.is_jihai pais
;;

(** 4 han *)
let is_shosangen mlist =
  count_sang_kou mlist = 2 && count_sang_atama mlist = 1
;;

let is_honroto pais =
  List.for_all Pai.is_yaochuhai pais
;;

(** 6 han *)
let is_chinitsu pais =
  count_family pais = 1
;;

(** yakuman *)
let is_tiho jicha kawa =
  jicha <> TONCHA && kawa = []
;;

let is_tenho jicha kawa =
  jicha = TONCHA && kawa = []
;;

let is_suanko fetch machi mlist =
  match count_anko mlist, fetch, machi with
    | 4, Tumo _, _ -> true
    | 4, Ron _, Tanki(_) -> true
    | _ -> false
;;

let is_suanko2 machi mlist =
  match count_anko mlist, machi with
    | 4, Tanki(_) -> true
    | _ -> false
;;
(** suanko2 is double yakuman *)

let is_sukantsu naki_list =
  count_ankan naki_list = 4
;;

let is_daisangen mlist =
  count_sang_kou mlist = 3
;;

let is_daisushi mlist =
  count_kaze_kou mlist = 4
;;

let is_shosushi mlist =
  count_kaze_kou mlist = 3 && count_kaze_atama mlist = 1
;;

let is_chinroto mlist =
  List.for_all Mentsu.is_roto mlist
;;

let is_tuiso mlist =
  List.for_all (Pai.is_jihai $ Mentsu.pai_of_mentsu) mlist
;;

let is_ryuiso mlist =
  List.for_all (Pai.is_greenhai $ Mentsu.pai_of_mentsu) mlist
;;

let is_churenpoto pais =
  count_pai M1 pais >= 3 &&
    count_pai M9 pais >= 3 &&
    List.for_all (fun p -> List.exists ((=) p) pais) [M2; M3; M4; M5; M6; M7; M8]
;;

let is_kokushi pais =
  let uniq_pais = List.unique pais in
  List.length uniq_pais = 13 && List.for_all Pai.is_yaochuhai uniq_pais
;;

let parse_agari_normal bafu yama jicha kawa {naki_list; pais; fetch} =
  let p1 = Fetch.pai_of_fetch fetch in
  let naki_mlist = List.map Naki.mentsu_of_naki naki_list in
  let naki_mlist_kui = List.filter Naki.is_kui_naki naki_list +> List.map Naki.mentsu_of_naki in
  let naki_mlist_ankan = List.filter Naki.is_ankan naki_list +> List.map Naki.mentsu_of_naki in
  let naki_pais = List.map Mentsu.pais_of_mentsu naki_mlist +> List.concat in
  let pais_tehai  = List.sort (p1 :: pais) ~cmp:Pai.compare in
  let pais_all = List.sort (pais_tehai @ naki_pais) ~cmp:Pai.compare in
  let is_menzen = naki_mlist_kui = [] in
  parse_machi_normal pais
  +> List.filter (Machi.is_waitable p1) >>= fun machi ->
  parse_a1_mn pais_tehai
  +> List.unique ~cmp:Mentsu.eq_mlist >>= fun mlist ->

  let mlist_menzen = mlist @ naki_mlist_ankan in
  let mlist_with_kui = mlist_menzen @ naki_mlist_kui in
  let richi_flag = is_menzen && is_richi kawa in
  let check_yaku = function
    | RICHI -> richi_flag
    | TUMO_AGARI -> is_menzen && is_tumo_agari fetch
    | RINSHAN_TUMO -> Fetch.is_rinshan_tumo fetch
    | HAITEI_TUMO -> Fetch.is_haitei_tumo fetch
    | IPPATSU -> is_ippatsu kawa
    | PINFU -> is_pinfu machi mlist_menzen
    | TANYAO -> is_tanyao pais_all
    | IPEKO -> is_ipeko mlist_menzen
    | YAKUHAI_TON -> (jicha = TONCHA || bafu = TONBA) && is_yakuhai TON mlist_with_kui
    | YAKUHAI_TON2 -> jicha = TONCHA && bafu = TONBA && is_yakuhai TON mlist_with_kui
    | YAKUHAI_NAN -> (jicha = NANCHA || bafu = NANBA) && bafu = NANBA && is_yakuhai NAN mlist_with_kui
    | YAKUHAI_NAN2 -> jicha = NANCHA && bafu = NANBA && is_yakuhai NAN mlist_with_kui
    | YAKUHAI_SHA -> jicha = SHACHA && is_yakuhai SHA mlist_with_kui
    | YAKUHAI_PEI -> jicha = PEICHA && is_yakuhai PEI mlist_with_kui
    | YAKUHAI_HAKU -> is_yakuhai HAKU mlist_with_kui
    | YAKUHAI_HATSU -> is_yakuhai HATSU mlist_with_kui
    | YAKUHAI_CHUN -> is_yakuhai CHUN mlist_with_kui

    | DOUBLE_RICHI -> is_menzen && is_double_richi kawa
    | ITTSU -> is_menzen && is_ittsu mlist_menzen
    | ITTSU_NAKI -> is_menzen = false && is_ittsu mlist_with_kui
    | CHANTA -> is_menzen && is_chanta mlist_menzen
    | CHANTA_NAKI -> is_menzen = false && is_chanta mlist_with_kui
    | SANSHOKU_DOJUN -> is_sanshoku_dojun mlist_menzen
    | SANSHOKU_DOJUN_NAKI -> is_menzen = false && is_sanshoku_dojun mlist_with_kui
    | SANSHOKU_DOKO -> is_sanshoku_doko mlist_menzen
    | SANSHOKU_DOKO_NAKI -> is_menzen = false && is_sanshoku_doko mlist_with_kui
    | SANANKO -> is_sananko fetch machi mlist_menzen
    | SANKANTSU -> is_sankantsu naki_mlist_ankan naki_mlist_kui
    | TOITOI -> is_menzen = false && is_toitoi mlist_with_kui

    | RYANPEKO -> is_ryanpeko mlist_menzen
    | JUNCHANTA -> is_menzen && is_junchanta mlist_menzen
    | JUNCHANTA_NAKI -> is_menzen = false && is_junchanta mlist_with_kui
    | HONITSU -> is_menzen && is_honitsu pais_all
    | HONITSU_NAKI -> is_menzen = false && is_honitsu pais_all

    | SHOSANGEN -> is_shosangen mlist_with_kui
    | HONROTO -> is_menzen = false && is_honroto pais_all

    | CHINITSU -> is_menzen && is_chinitsu pais_all
    | CHINITSU_NAKI -> is_menzen = false && is_chinitsu pais_all

    | TIHO -> is_tiho jicha kawa
    | TENHO -> is_tenho jicha kawa
    | SUANKO -> is_menzen && is_suanko fetch machi mlist_menzen
    | SUANKO2 -> is_menzen && is_suanko2 machi mlist_menzen
    | SUKANTSU -> is_sukantsu naki_list
    | DAISANGEN -> is_daisangen mlist_with_kui
    | DAISUSHI -> is_daisushi mlist_with_kui
    | SHOSUSHI -> is_shosushi mlist_with_kui
    | TUISO -> is_tuiso mlist_with_kui
    | CHINROTO -> is_chinroto mlist_with_kui
    | RYUISO -> is_ryuiso mlist_with_kui
    | CHURENPOTO -> is_churenpoto pais_tehai
    | _ -> false in
  
  let yaku_list =
    List.filter check_yaku Yaku.all
    +> List.sort ~cmp:Yaku.compare
    +> Yaku.merge in
  guard (yaku_list <> []) >>= fun _ ->
  let menzen_ron_fu = if is_menzen && Fetch.is_ron fetch then Fu.of_menzen_ron else 0 in
  let fetch_fu = Fetch.fu fetch in
  let machi_fu = Machi.fu machi in
  let mentsu_fu_menzen =
    List.map (Mentsu.fu ~is_naki:false ~jicha ~bafu) mlist_menzen
    +> List.fold_left (+) 0 in
  let mentsu_fu_naki =
    List.map (Mentsu.fu ~is_naki:true ~jicha ~bafu) naki_mlist_kui
    +> List.fold_left (+) 0 in
  let fu_bonus = FuBonus(menzen_ron_fu + fetch_fu + machi_fu + mentsu_fu_menzen + mentsu_fu_naki) in
  let fu_count = Fu.count_with_bonus fu_bonus in
  let fu_count =
    (** if not pinfu but fu = 20, treat it as 30 *)
    if fu_count = 20 && List.exists ((=) PINFU) yaku_list = false then 30 else fu_count in
  let yaku_count = List.map Yaku.yaku_count yaku_list +> List.fold_left (+) 0 in
  let dora_count = if yaku_count < 14 then count_dora yama pais_all else 0 in
  let uradora_count = if richi_flag && yaku_count < 14 then count_uradora yama pais_all else 0 in
  return {
    machi = machi;
    fu_count = fu_count;
    yaku_count = yaku_count + dora_count + uradora_count;
    dora_count = dora_count + uradora_count;
    yaku_list = yaku_list;
  }
;;

let parse_agari_chitoitsu yama jicha kawa {naki_list; pais; fetch} =
  let p1 = Fetch.pai_of_fetch fetch in
  parse_machi_chitoitsu pais +> List.unique ~cmp:Machi.eq +> List.filter (Machi.is_waitable p1) >>= fun machi ->
  let pais_tehai = List.sort (p1 :: pais) ~cmp:Pai.compare in
  let richi_flag = is_richi kawa in
  let check_yaku = function
    | RICHI -> richi_flag
    | DOUBLE_RICHI -> is_double_richi kawa
    | TUMO_AGARI -> is_tumo_agari fetch
    | RINSHAN_TUMO -> Fetch.is_rinshan_tumo fetch
    | HAITEI_TUMO -> Fetch.is_haitei_tumo fetch
    | IPPATSU -> is_ippatsu kawa
    | TANYAO -> is_tanyao pais_tehai
    | CHITOITSU -> is_chitoitsu machi pais_tehai
    | HONITSU -> is_honitsu pais_tehai
    | CHINITSU -> is_chinitsu pais_tehai
    | TIHO -> is_tiho jicha kawa
    | TENHO -> is_tenho jicha kawa
    | _ -> false in
  let yaku_list =
    List.filter check_yaku [TUMO_AGARI; TANYAO; CHITOITSU; HONITSU; CHINITSU]
    +> List.sort ~cmp:Yaku.compare
    +> Yaku.merge in
  guard (yaku_list <> []) >>= fun _ ->
  let yaku_count = List.map Yaku.yaku_count yaku_list +> List.fold_left (+) 0 in
  let dora_count = if yaku_count < 14 then count_dora yama pais else 0 in
  let uradora_count = if richi_flag && yaku_count < 14 then count_uradora yama pais else 0 in
  return {
    machi = machi;
    fu_count = Fu.of_chitoitsu;
    yaku_count = yaku_count + dora_count + uradora_count;
    dora_count = dora_count + uradora_count;
    yaku_list = yaku_list;
  }
;;

let parse_agari_kokushi kawa {naki_list; pais; fetch} =
  let p1 = Fetch.pai_of_fetch fetch in
  parse_machi_kokushi pais +> List.unique ~cmp:Machi.eq +> List.filter (Machi.is_waitable p1) >>= fun machi ->
  let pais_tehai = List.sort (p1 :: pais) ~cmp:Pai.compare in
  guard (is_kokushi pais_tehai) >>= fun _ ->
  return {
    machi = machi;
    fu_count = Fu.of_kokushi;
    yaku_count = 14;
    dora_count = 0;
    yaku_list = [KOKUSHI];
  }
;;

let parse_machi pais : machi list =
  (parse_machi_normal pais @
     parse_machi_chitoitsu pais @
     parse_machi_kokushi pais)
  +> List.unique ~cmp:Machi.eq
;;

let parse_agari bafu yama jicha kawa tehai : agari list =
  parse_agari_normal bafu yama jicha kawa tehai @
    parse_agari_chitoitsu yama jicha kawa tehai @
    parse_agari_kokushi kawa tehai
;;
