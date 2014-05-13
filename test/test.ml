open OUnit

let suite = "Mjlib" >::: [
  Test_utils.suite;
  Test_pai.suite;
  Test_mentsu.suite;
  Test_machi.suite;
  Test_agari.suite;
  Test_parser.suite;
  Test_naki.suite;
  Test_fetch.suite;
  Test_fu.suite;
  Test_jicha.suite;
  Test_yama.suite;
  Test_point.suite;
]
;;

let _ =
  run_test_tt suite ~verbose:true
;;
