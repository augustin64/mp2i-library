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

(** [fusion_sort l] sorts list l using fusion sort *)
let rec fusion_sort = function
    | [] -> []
    | [e] -> [e]
    | l -> let l1, l2 = _split l in
    _fusion (fusion_sort l1) (fusion_sort l2);;
    
    
    
<<<<<<< HEAD
(** [concat l1 l2] returns the list containing the elements of [l1], followed by the elements of [l2] *)
let rec _concat l1 l2 = match l1 with
    | [] -> l2
    | e::q -> e::(_concat q l2);;
    
(** [_partition p l] splits the list l in two parts, the elements greater or equal than p and the others *)
let rec _partition p l = match l with
    | [] -> ([], [])
    | e::q -> let l1, l2 = (_partition p q) in
                if e < p then ((e::l1), l2)
                else (l1, (e::l2));;

(** [quicksort l] uses the quicksort algorithm to sort the list l *)
let rec quicksort l = match l with
    | [] -> []
    | [e] -> [e]
    | e::q -> let l1, l2 = _partition e q in
        _concat (quicksort l1) (e::quicksort l2);;
=======
(** [_maximum t] returns the maximum in an array [t] *)
let _maximum t = 
    let m = ref t.(0) in
    for i=1 to Array.length t - 1 do
        m := max t.(i) !m
    done;
    !m;;
    
(** [tri_comptage t] sorts the array t using counting sort *)
let tri_comptage t =
    let m = _maximum t in
    let compte = Array.make (m+1) 0 in
    let n = Array.length t in
    for i=0 to n - 1 do
        compte.(t.(i)) <- compte.(t.(i)) + 1
    done;
    let k = ref 0 in
    for i=0 to m do
        for j=1 to compte.(i) do
            t.(!k) <- i;
            incr k
        done
    done;;
>>>>>>> 66ea5c17a9bca7f229fefa0f52759acdb609e4ca
