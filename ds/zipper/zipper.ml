(** définition du type zipper *)
type 'a zipper = { left : 'a list; right : 'a list };;

(** [move_right z] déplace le curseur de z de 1 vers la droite (renvoie un nouveau zipper) *)
let move_right z = match z.right with
    | e::r -> { left = e::z.left; right = r }
    | [] -> failwith "Déjà à droite";;
    
(** [move_left z] déplace le curseur de z de 1 vers la gauche (renvoie un nouveau zipper) *)
let move_left z = match z.left with
    | e::l -> { left = l; right = e::z.right }
    | [] -> failwith "Déjà à gauche";;
    
(** [move n z] déplace le curseur de z de n pas vers la droite / -n pas vers la gauche (renvoie un nouveau zipper) *)
let rec move n z =
    if n = 0 then z
    else if n > 0 then
        let z1 = move (n-1) z in
        move_right z1
    else
        let z1 = move (n+1) z in
        move_left z1;;
      
(** [supprimer z] supprime l'élément sous le curseur de z *)
let supprimer z = match z.right with
    | e::q -> { left = z.left; right = q }
    | [] -> failwith "pas d'élément à supprimer";;
    
(** [ajouter e z] ajoute un élément au zipper z et renvoie ce nouveau zipper *)
let ajouter e z = { left = z.left; right = e::z.right};;

(** [convert z] renvoie la liste correspondant au zipper z *)
let convert z = (List.rev z.left) @ z.right;;