(** définition, du type l2c *)
type 'a l2c = {elem : 'a; mutable prev : 'a l2c; mutable next : 'a l2c};;

(** [create e] crée et renvoie une l2c possédant comme seul élément e *)
let create e =
    let rec l = {elem = e; prev = l; next = l} in
    l;;
    
(** [add e l] ajoute l'élément e après le bloc l de la l2c *)
let add e l = 
    let bloc = {elem = e; next = l.next; prev = l} in
    l.next.prev <- bloc;
    l.next <- bloc;;
    
(** [del l] supprime l'élément l de la l2c *)
let del l =
    l.prev.next <- l.next;
    l.next.prev <- l.prev;;
    
(** [length l] renvoie le nombre de blocs de la l2c *)
let length l = 
    let rec aux l1 =
        if l1 == l then 1 else 1 + aux l1.next in
    aux l.next;;
    
(** [print_l2c l] affiche la l2c l sous le format d'une liste *)
let print_l2c l =
    print_newline ();
    print_string ("[ ");
    let l_cur = ref l in
    while !l_cur.next != l do
        print_int !l_cur.elem;
        print_string ("; ");
        l_cur := !l_cur.next;
    done;
    print_int l.next.elem; print_string ("]"); print_newline ();;
    
(** [mem e l] renvoie si l contient l'élément e *)
let mem e l = 
    let bloc = ref l in
    let valeur = ref (!bloc.elem = e) in
    while l == !bloc do
        bloc := !bloc.next;
        valeur := !valeur || !bloc.elem = e;
    done;!valeur;;
    
(** [fusion l1 l2] fusionne les l2c l1 et l2 *)
let fusion l1 l2 =
    let next = l1.next in
    l1.next <- l2.next;
    l2.next <- next;
    l2.next.prev <- l2;
    l1.next.prev <- l1;;