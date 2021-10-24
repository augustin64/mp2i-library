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
    
let list_of_array t = 
    let rec aux i = (* transforme t.(0), ..., t.(i) en liste *)
        if i = -1 then []
        else t.(i)::aux (i - 1) in
    aux (Array.length t - 1);;
    
let croissant t =
    let res = ref true in
    for i=0 to Array.length t - 2 do
        if t.(i) > t.(i + 1)
        then res := false
    done;
    !res;;
    
let max_local t = 
    let n = Array.length t in
    let rec aux i j = (* cherche un max local entre les indices i et j de t *)
        let m = (i + j)/2 in
        if (m = 0 || t.(m) >= t.(m-1)) && (m = n - 1 || t.(m) >= t.(m+1))
        then m (* m est un max local *)
        else if t.(m) < t.(m - 1) then aux i (m - 1)
        else aux (m + 1) j in
    aux 0 (n + 1);;

let tri_comptage t =
    let m = maximum t in
    let compte = Array.make (m+1) 0 in
    let n = Array.length t in
    for i=0 to n - 1 do
        compte.(t.(i)) <- compte.(t.(i)) + 1
    done;
    let k = ref 0 in (* k est la position où on va ajouter le prochain élément dans t *)
    for i=0 to m do
        for j=1 to compte.(i) do
            t.(!k) <- i;
            incr k
        done
    done;;
    
let tranche_max t =
    let m = ref t.(0) in
    let m_cur = ref t.(0) in
    for i = 1 to Array.length t - 1 do
        m_cur := max (!m_cur + t.(i)) t.(i);
        m := max !m !m_cur
    done;
    !m;;
    
let inv t =
    let res = ref 0 in
    let n = Array.length t in
    for i=0 to n - 1 do  (* répété n fois *)
        for j=i+1 to n - 1 do (* répété au plus n fois *)
            if t.(i) > t.(j) then res := !res + 1 (* répété au plus n² fois, donc complexité O(n²) *)
        done
    done;
    !res;;