(* This file contains sort algorithms *)

(** [swap t i j] exchanges [t.(i)] and [t.(j)] *)
let _swap t i j =
  let tmp = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- tmp;;

(** [sort_bubble t] sorts array t using bubble sorting (O(n**2)) *)
let sort_bubble t = 
  for _ = 0 to Array.length t - 1 do
    for j = 0 to Array.length t - 2 do
      if t.(j) > t.(j + 1) then _swap t j (j + 1)
      done
  done;;

(** [_split l] split l in two equal parts*)
let rec _split = function
    | [] -> [], []
    | [e] -> [e], []
    | e1::e2::q -> let q1, q2 = _split q in
        e1::q1, e2::q2;;
        
(** [_fusion l1 l2] adds two list together sorting elements by pairs *)
let rec _fusion l1 l2 = match l1, l2 with
    | [], _ -> l2
    | _, [] -> l1
    | e1::q1, e2::q2 when e1 < e2 -> e1::_fusion q1 l2
    | e1::q1, e2::q2 -> e2::_fusion l1 q2;;

(** [fusion_sort l] sorts list l using fusion sort (O(n???)) *)
let rec fusion_sort = function
    | [] -> []
    | [e] -> [e]
    | l -> let l1, l2 = _split l in
    _fusion (fusion_sort l1) (fusion_sort l2);;