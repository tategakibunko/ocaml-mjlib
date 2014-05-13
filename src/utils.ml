(*
  utils.ml
  Copyright (c) 2012 - by Masaki Watanabe
  License: see LICENSE
*)
let (@@) f g = f g
let (+>) f g = g f
let ($) f g x = f (g x)
let spf = Printf.sprintf

let from_opt = function
  | Some x -> x
  | None -> failwith "from_just:None"
;;

let group_by ?(eq=(=)) lst =
  let rec iter ret rest =
    match rest with
      | h :: tl ->
	let is_same x = eq h x in
	let not_same x = not (is_same x) in
	let group = List.filter is_same rest in
	let rest' = List.filter not_same rest in
	iter (group :: ret) rest'
      | [] -> List.rev ret in
  iter [] lst
;;

let shuffle_array ?(depth=3) ary =
  Random.self_init ();
  let alen = Array.length ary in
  for k = 0 to depth - 1 do
    for i = 0 to alen - 1 do
      let at = Random.int alen in
      let save = ary.(i) in
      ary.(i) <- ary.(at);
      ary.(at) <- save;
    done;
  done;
  ary
;;

let rotate_next lst is_cur =
  let rec find = function
    | h :: next :: rest when is_cur h -> next
    | h :: rest -> find rest
    | [] -> List.hd lst in
  find lst
;;

module ListM = struct
  let return x = [x]
  let bind x f = List.concat @@ List.map f x
  let guard x = if x then return () else []
  let (>>=) = bind
end
