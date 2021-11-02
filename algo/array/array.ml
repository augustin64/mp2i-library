(** [somme t] returns the sum of all the elements in an array *)
let somme t = 
    let res = ref 0 in
    for i=0 to Array.length t - 1 do
        res := !res + t.(i)
    done;
    !res;;
    
(** [maximum t] returns the maximum in an array [t] *)
let maximum t = 
    let m = ref t.(0) in
    for i=1 to Array.length t - 1 do
        m := max t.(i) !m
    done;
    !m;;
    
(** [minimum t] returns the minimum of the array [t] *)
let minimum t = 
    let m = ref t.(0) in
    for i=1 to Array.length t - 1 do
        m := min t.(i) !m
    done;
    !m;;
    
(** [list_of_array t] returns the list corresponding to the array t *)
let list_of_array t = 
    let rec aux i =
        if i = (Array.length t) then []
        else t.(i)::aux (i + 1) in
    aux 0;;
    
(** [croissant t] returns wether t is increasing or not *)
let croissant t =
    let res = ref true in
    for i=0 to Array.length t - 2 do
        if t.(i) > t.(i + 1)
        then res := false
    done;
    !res;;
    
(** [max_local t] finds and returns a local maximum *)
let max_local t = 
    let n = Array.length t in
    let rec aux i j =
        let m = (i + j)/2 in
        if (m = 0 || t.(m) >= t.(m-1)) && (m = n - 1 || t.(m) >= t.(m+1))
        then m
        else if t.(m) < t.(m - 1) then aux i (m - 1)
        else aux (m + 1) j in
    aux 0 (n + 1);;
    
(** [tranche_max t] returns the maximum value that can be reached by consecutive items in the array t *)
let tranche_max t =
    let m = ref t.(0) in
    let m_cur = ref t.(0) in
    for i = 1 to Array.length t - 1 do
        m_cur := max (!m_cur + t.(i)) t.(i);
        m := max !m !m_cur
    done;
    !m;;
    
(** [inv t] returns the number of inversions in the array t O(n²) *)
let inv t =
    let res = ref 0 in
    let n = Array.length t in
    for i=0 to n - 1 do 
        for j=i+1 to n - 1 do
            if t.(i) > t.(j) then res := !res + 1
        done
    done;
    !res;;