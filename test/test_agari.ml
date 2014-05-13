open OUnit
open Mjlib
open Mjlib.Utils
open Mjlib.Types
open Mjlib.Parser

let verbose = true
;;

let debug_agari agari_list =
  print_endline @@ spf "agari pattern = %d" (List.length agari_list);
  List.iter (fun agari ->
    List.iter (fun yaku ->
      print_endline @@ Yaku.string_of_yaku yaku
    ) agari.yaku_list
  ) agari_list;
  print_endline "-----------------";
;;

(** check if yaku is included in agari *)
let check_yaku agari_list yaku =
  let result = List.exists (fun agari ->
    List.exists ((=) yaku) agari.yaku_list
  ) agari_list in
  assert_equal result true
;;

let parse ?(bafu=TONBA) ?(yama=Yama.create ()) ?(jicha=TONCHA) ?(kawa=[Kawahai(PEI, [])]) naki_list fetch pais =
  Mjlib.Parser.parse_agari bafu yama TONCHA kawa {naki_list = naki_list; pais = pais; fetch = fetch}
;;

let normal_tumo pai =
  Tumo(pai, NormalTumo)
;;

let testRichi () =
  let ret = parse [] (normal_tumo P7) ~kawa:[Kawahai(PEI,[]); Kawahai(PEI,[Richi])]
  [P1;P1;P2;P2;P3;P3;P4;P4;P5;P5;P6;P6;P7] in
  check_yaku ret RICHI
;;

let testDoubleRichi () =
  let ret = parse [] (normal_tumo P7) ~kawa:[Kawahai(PEI,[Richi])]
  [P1;P1;P2;P2;P3;P3;P4;P4;P5;P5;P6;P6;P7] in
  check_yaku ret DOUBLE_RICHI
;;

let testIppatsu () =
  let ret = parse [] (normal_tumo P7) ~kawa:[Kawahai(PEI,[]); Kawahai(PEI,[Richi])]
  [P1;P1;P2;P2;P3;P3;P4;P4;P5;P5;P6;P6;P7] in
  check_yaku ret IPPATSU
;;

let testTumoAgari () =
  let ret = parse [] (normal_tumo P7) [P1;P1;P2;P2;P3;P3;P4;P4;P5;P5;P6;P6;P7] in
  check_yaku ret TUMO_AGARI
;;

let testTanyao () =
  let ret = parse [] (normal_tumo P7) [M2;M2;P2;P2;P3;P3;P4;P4;P5;P5;P6;P6;P7] in
  check_yaku ret TANYAO
;;
   
let testYakuhaiHaku () =
  let ret = parse [Pon(HAKU,TOIMEN)] (normal_tumo P9) [M2;M2;P1;P2;P3;P4;P5;P6;P7;P8] in
  check_yaku ret YAKUHAI_HAKU
;;

let testYakuhaiHatsu () =
  let ret = parse [Pon(HATSU,TOIMEN)] (normal_tumo P9) [M2;M2;P1;P2;P3;P4;P5;P6;P7;P8] in
  check_yaku ret YAKUHAI_HATSU
;;

let testYakuhaiChun () =
  let ret = parse [Pon(CHUN,TOIMEN)] (normal_tumo P9) [M2;M2;P1;P2;P3;P4;P5;P6;P7;P8] in
  check_yaku ret YAKUHAI_CHUN
;;

let testPinfu () =
  let ret = parse [] (normal_tumo P7) [P1;P1;P2;P2;P3;P3;P4;P4;P5;P5;P6;P6;P7] in
  check_yaku ret PINFU
;;

let testIpeko () =
  let ret = parse [] (normal_tumo P8) [M2;M2;S2;S3;S4;P3;P4;P5;P6;P6;P7;P7;P8] in
  check_yaku ret IPEKO
;;

let testSanshokuDojun () =
  let ret = parse [] (normal_tumo TON) [M2;M2;M4;M5;M6;P4;P5;P6;S4;S5;S6;TON;TON] in
  check_yaku ret SANSHOKU_DOJUN
;;

let testSanshokuDojunNaki () =
  let ret = parse [Chi(M4,M4)] (normal_tumo TON) [M2;M2;P4;P5;P6;S4;S5;S6;TON;TON] in
  check_yaku ret SANSHOKU_DOJUN_NAKI
;;

let testSanshokuDoko () =
  let ret = parse [] (normal_tumo NAN) [M2;M2;M2;P2;P2;P2;S2;S2;S2;P5;P6;P7;NAN] in
  check_yaku ret SANSHOKU_DOKO

let testSanshokuDokoNaki () =
  let ret = parse [Pon(M2,TOIMEN)] (normal_tumo NAN) [P2;P2;P2;S2;S2;S2;P5;P6;P7;NAN] in
  check_yaku ret SANSHOKU_DOKO_NAKI
;;

let testIttsu () =
  let ret = parse [] (normal_tumo NAN) [M2;M2;M2;P1;P2;P3;P4;P5;P6;P7;P8;P9;NAN] in
  check_yaku ret ITTSU
;;

let testIttsuNaki () =
  let ret = parse [Chi(P1,P1)] (normal_tumo NAN) [M2;M2;M2;P4;P5;P6;P7;P8;P9;NAN] in
  check_yaku ret ITTSU_NAKI
;;

let testToitoi () =
  let ret = parse [Pon(TON,TOIMEN)] (normal_tumo NAN) [P1;P1;P1;P4;P4;P4;P6;P6;NAN;NAN] in
  check_yaku ret TOITOI
;;

let testChitoitsu () =
  let ret = parse [] (normal_tumo NAN) [P1;P1;P2;P2;P4;P4;P6;P6;TON;TON;S1;S1;NAN] in
  check_yaku ret CHITOITSU
;;

let testChanta () =
  let ret = parse [] (normal_tumo NAN) [M1;M2;M3;P1;P2;P3;S1;S2;S3;P9;P9;P9;NAN] in
  check_yaku ret CHANTA
;;

let testChantaNaki () =
  let ret = parse [Pon(P9,TOIMEN)] (normal_tumo NAN) [M1;M2;M3;P1;P2;P3;S1;S2;S3;NAN] in
  check_yaku ret CHANTA_NAKI
;;

let testJunchanta () =
  let ret = parse [] (normal_tumo S9) [M1;M2;M3;P1;P2;P3;S1;S2;S3;P7;P8;P9;S9] in
  check_yaku ret JUNCHANTA
;;

let testJunchantaNaki () =
  let ret = parse [Pon(P9,TOIMEN)] (normal_tumo S9) [M1;M2;M3;P1;P2;P3;S1;S2;S3;S9] in
  check_yaku ret JUNCHANTA_NAKI
;;

let testRyanpeko () =
  let ret = parse [] (normal_tumo P7) [P1;P1;P2;P2;P3;P3;P4;P4;P5;P5;P6;P6;P7] in
  check_yaku ret RYANPEKO
;;

let testHonitsu () =
  let ret = parse [] (normal_tumo NAN) [M2;M2;M2;M1;M2;M3;M4;M5;M6;TON;TON;TON;NAN] in
  check_yaku ret HONITSU
;;

let testHonitsuNaki () =
  let ret = parse [Pon(TON,TOIMEN)] (normal_tumo NAN) [M2;M2;M2;M1;M2;M3;M4;M5;M6;NAN] in
  check_yaku ret HONITSU_NAKI
;;

let testHonroto () =
  let ret = parse [Pon(TON,TOIMEN)] (normal_tumo NAN) [M1;M1;M1;M9;M9;M9;P1;P1;P1;NAN] in
  check_yaku ret HONROTO
;;

let testChinitsu () =
  let ret = parse [] (normal_tumo M7) [M1;M1;M1;M9;M9;M9;M2;M3;M4;M5;M6;M7;M7] in
  check_yaku ret CHINITSU
;;

let testChinitsuNaki () =
  let ret = parse [Chi(M2,M2)] (normal_tumo M7) [M1;M1;M1;M9;M9;M9;M5;M6;M7;M7] in
  check_yaku ret CHINITSU_NAKI
;;

let testSuanko () =
  let ret = parse [] (normal_tumo M1) [P1;P1;P1;P2;P2;P2;P3;P3;P3;S8;S8;M1;M1] in
  check_yaku ret SUANKO
;;

let testSuanko2 () =
  let ret = parse [] (normal_tumo M1) [P1;P1;P1;P2;P2;P2;P3;P3;P3;S8;S8;S8;M1] in
  check_yaku ret SUANKO2
;;

let testSukantsu () =
  let ret = parse [Ankan(P1); Ankan(P2); Ankan(P3); Ankan(P4)] (normal_tumo M1) [M1] in
  check_yaku ret SUKANTSU
;;

let testDaisangen () =
  let ret = parse [] (normal_tumo M1) [HAKU;HAKU;HAKU;HATSU;HATSU;HATSU;CHUN;CHUN;CHUN;P1;P2;P3;M1] in
  check_yaku ret DAISANGEN
;;

let testKokushi () =
  let ret = parse [] (normal_tumo NAN) [M1;M9;P1;P9;S1;S9;HAKU;HATSU;CHUN;TON;SHA;PEI;PEI] in
  check_yaku ret KOKUSHI
;;

let testRyuiso () =
  let ret = parse [] (normal_tumo HATSU) [S2;S2;S3;S3;S4;S4;S6;S6;S6;S8;S8;S8;HATSU] in
  check_yaku ret RYUISO
;;

let testShosushi () =
  let ret = parse [] (normal_tumo M1) [TON;TON;TON;NAN;NAN;NAN;SHA;SHA;SHA;PEI;PEI;M2;M3] in
  check_yaku ret SHOSUSHI
;;

let testDaisushi () =
  let ret = parse [] (normal_tumo M1) [TON;TON;TON;NAN;NAN;NAN;SHA;SHA;SHA;PEI;PEI;PEI;M1] in
  check_yaku ret DAISUSHI
;;

let testTuiso () =
  let ret = parse [] (normal_tumo HATSU) [TON;TON;TON; NAN;NAN;NAN; CHUN;CHUN;CHUN; HAKU;HAKU; HATSU;HATSU] in
  check_yaku ret TUISO
;;

let testChinroto () =
  let ret = parse [] (normal_tumo M1) [P1;P1;P1;P9;P9;P9;S1;S1;S1;S9;S9;M1;M1] in
  check_yaku ret CHINROTO
;;

let testChurenpoto () =
  let ret = parse [] (normal_tumo M1) [M1;M1;M1; M2;M3;M4; M5;M6;M7; M8; M9;M9;M9] in
  check_yaku ret CHURENPOTO
;;

let suite = "test_agari" >::: [
  "RICHI" >:: testRichi;
  "DOUBLE_RICHI" >:: testDoubleRichi;
  "IPPATSU" >:: testIppatsu;
  "TUMO_AGARI" >:: testTumoAgari;
  "TANYAO" >:: testTanyao;
  "PINFU" >:: testPinfu;
  "IPEKO" >:: testIpeko;
  "YAKUHAI_HAKU" >:: testYakuhaiHaku;
  "YAKUHAI_HATSU" >:: testYakuhaiHatsu;
  "YAKUHAI_CHUN" >:: testYakuhaiChun;

  "ITTSU" >:: testIttsu;
  "ITTSU_NAKI" >:: testIttsuNaki;
  "CHANTA" >:: testChanta;
  "CHANTA_NAKI" >:: testChantaNaki;
  "SANSHOKU_DOJUN" >:: testSanshokuDojun;
  "SANSHOKU_DOJUN_NAKI" >:: testSanshokuDojunNaki;
  "SANSHOKU_DOKO" >:: testSanshokuDoko;
  "SANSHOKU_DOKO_NAKI" >:: testSanshokuDokoNaki;
  "CHITOITSU" >:: testChitoitsu;

  "RYANPEKO" >:: testRyanpeko;
  "JUNCHANTA" >:: testJunchanta;
  "JUNCHANTA_NAKI" >:: testJunchantaNaki;
  "HONITSU" >:: testHonitsu;
  "HONITSU_NAKI" >:: testHonitsuNaki;

  "HONROTO" >:: testHonroto;

  "CHINITSU" >:: testChinitsu;
  "CHINITSU_NAKI" >:: testChinitsuNaki;

  "SUANKO" >:: testSuanko;
  "SUANKO2" >:: testSuanko2;
  "SUKANTSU" >:: testSukantsu;
  "DAISANGEN" >:: testDaisangen;
  "RYUISO" >:: testRyuiso;
  "SHOSUSHI" >:: testShosushi;
  "DAISUSHI" >:: testDaisushi;
  "TUISO" >:: testTuiso;
  "CHINROTO" >:: testChinroto;
  "CHURENPOTO" >:: testChurenpoto;
  "KOKUSHI" >:: testKokushi;
]
;;
