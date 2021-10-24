(** defines entier type to avoid the max_int + 1 -> min_int problem *)
type entier = Infini | MoinsInfini | Entier of int



(** PRIVATE FUNCTIONS **)

(** [_add_int a b] adds two ints together and returns an object of type entier *)
let _add_int a b =
    if max_int - a <= b && a > 0 then Infini
    else if min_int + a >= b && a > 0 then MoinsInfini
    else Entier (a + b);;
    
(** [_oppose a] returns (-1*a) *)
let _oppose a = match a with
    | Infini -> MoinsInfini
    | MoinsInfini -> Infini
    | Entier(e) -> Entier(-e);;
    
(** [_inverse a] returns (1/a) *)
let _inverse a = match a with
    | Infini -> Entier(0)
    | MoinsInfini -> Entier(0)
    | Entier(e) -> Entier(1/e);;
    
    

(** BASIC OPERATIONS **)
    
(** ( +! ) defines the addition for type entier *)
let ( +! ) a b = match a, b with
    | Infini, Entier(_) | Entier(_), Infini -> Infini
    | Infini, Infini -> Infini
    | MoinsInfini, Entier(_) | Entier(_), MoinsInfini -> MoinsInfini
    | MoinsInfini, MoinsInfini -> MoinsInfini
    | Entier(e1), Entier(e2) -> _add_int e1 e2
    | _ -> failwith "Indéterminé";;
    
(** ( -! ) defines the substraction for type entier *)
let ( -! ) a b = 
    a +! _oppose b;;
    
(** ( *! ) defines the multiplication for type entier *)
let rec ( *! ) a b = match a, b with
    | Infini, Infini | MoinsInfini, MoinsInfini -> Infini
    | MoinsInfini, Infini | Infini, MoinsInfini -> MoinsInfini
    | MoinsInfini, Entier(e) | Entier(e), MoinsInfini when e > 0 -> MoinsInfini
    | MoinsInfini, Entier(e) | Entier(e), MoinsInfini when e < 0 -> Infini
    | Infini, Entier(e) | Entier(e), Infini when e > 0 -> Infini
    | Infini, Entier(e) | Entier(e), Infini when e < 0 -> MoinsInfini
    | Infini, Entier(0) | MoinsInfini, Entier(0) 
    | Entier(0), Infini | Entier(0), MoinsInfini -> Entier(0)
    | Entier(a), Entier(b) -> if a = 0 then Entier(0)
                                else let c = a*b in
                                if c/a = b then Entier(a*b)
                                else (Infini) *! (Entier(a)) *! (Entier(b)) ;
    | _ -> failwith "Indéterminé"
    
(** ( /! ) defines the division for type entier *)
let ( /! ) a b = match a, b with
    | Infini, Entier(e) | MoinsInfini, Entier(e) when e > 0 -> a
    | Infini, Entier(e) | MoinsInfini, Entier(e) when e < 0 -> _oppose a
    | Entier(e), Infini | Entier(e), MoinsInfini -> Entier(0)
    | Entier(e1), Entier(e2) -> Entier(e1/e2)
    | _ -> failwith "Indetermine";;
    
    