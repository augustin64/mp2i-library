(** [sum l] returns the sum of elements of [l] *)
let rec sum l = match l with
    | [] -> 0
    | e::q -> e + sum q;;

(** [maximum l] returns the biigest element of the list [l] *)
let rec maximum l = match l with
    | [] -> min_int
    | e::q -> let m = maximum q in
        if e > m then e
        else m;;

(** [egal l1 l2] returns wether [l1] is equal to [l2] or not *)
let rec equal l1 l2 = match l1, l2 with
  | [], [] -> true
  | e::q, [] -> false
  | [], e::q -> false
  | e1::q1, e2::q2 -> e1 = e2 && equal q1 q2;;

(** [duplicate l] returns wether [l] contains two consecutive similar items or not *)
let rec duplicate l = match l with
    | [] -> false
    | [e] -> false
    | e::f::q -> e = f || duplicate (f::q);;

(** [croissante l] returns wether [l] is `croissante` or not *)
let rec croissante = function
    | [] -> true
    | [e] -> true
    | e1::e2::q -> e1 <= e2 && croissante (e2::q);;

(** [length l] returns the number of elements in [l] *)
let rec length l = match l with
    | [] -> 0
    | e::q -> 1 + length q;;

(** [last l] returns the last element of [l] *)
let rec last l = match l with
    | [] -> failwith "Empty List"
    | [e] -> e
    | _::q -> last q;;

(** [mem l e] returns wether [l] contains e or not*)
let rec mem l e = match l with
    | [] -> false
    | x::q -> x = e || mem q e;;

(** [iter f l] applies the funstion f on every element of l, and returns nothing*)
let rec iter f l = match l with
    | [] -> ()
    | e::q -> f e; iter f q;;

(** [filter f l] returns a list containing all elements of l for which f is verified *)
let rec filter f l = match l with
    | [] -> []
    | e::q -> if f e then e::filter f q
                    else filter f q;;

(** [map f l] applies the function f on every element of l and returns a list containing the outputs *)
let rec map f l = match l with
    | [] -> []
    | e::q -> f e::map f q;;
