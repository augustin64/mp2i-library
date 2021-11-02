(** définition des types case et liste1 *)
type 'a case = { elem : 'a; mutable next : 'a liste1 }
and 'a liste1 = Vide | C of 'a case;;

(** [to_list l] renvoie la list correspondant à la liste1 l *)
let rec to_list l = match l with
    | Vide -> []
    | C {elem=e; next=q} -> e::(to_list q);;