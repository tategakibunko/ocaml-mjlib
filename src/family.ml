(*
  family.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
let int_of_family = function
  | MANZ -> 0
  | PINZ -> 1
  | SOUZ -> 2
  | KAZE -> 3
  | SANG -> 4
;;

let compare family1 family2 =
  int_of_family family1 - int_of_family family2
;;
  
