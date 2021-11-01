(** [mem l e] retruns wether the list l contains the element e *)
let rec mem l e = match l with
    | [] -> false
    | x::q -> x = e || mem q e

(** [dicho t e] returns wether the sorted array t contains the element e [O(log2(n))] *)
let dicho t e =
    let rec aux i j =
        if i > j then false
        else let m = (i + j)/2 in
            if t.(m) = e then true
            else if t.(m) < e then aux (m + 1) j
            else aux i (m - 1) in
    aux 0 (Array.length t - 1)
    
(** [tricho t e] returns wether the sorted array t contains the element e [O(log3(n))] *)
let tricho t e =
    let rec aux i j =
        if i > j then false
        else let m1 = (2*i + j + 1)/3 in
            let m2 = (i + 2*j + 2)/3 in
            if t.(m1) = e || t.(m2) = e then true
            else if e < t.(m1) then aux i (m1 - 1)
            else if e < t.(m2) then aux (m1 + 1) (m2 - 1)
            else aux (m2 + 1) j in
    aux 0 (Array.length t - 1)