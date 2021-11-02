(** [has_cycle l []] renvoie si oui ou non la liste l contient un cycle, complexité en mémoire O(n)*)
let rec has_cycle l vus = match l with
    | Vide -> false
    | C (lc) -> (List.memq lc vus) || has_cycle lc.next (lc::vus);;
    
(** [step l] déplace le cycle de 1 *)
let step l = match l with
    | C c1 -> c1.next
    | Vide -> Vide;;
    
(** [lievre et tortue (step (C l)) (C l)] renvoie si oui ou non la liste l contient un cycle, complexité en mémoire O(1) *)
let rec lievre_et_tortue t l = 
    if l = Vide then false
    else t == l || lievre_et_tortue (step t) (step (step l));;