(** définition du type table de hachage *)
type ('a, 'b) table_hachage = {hache: 'a -> int; donnees: ('a * 'b) list array; largeur: int};;

(** [creer h w] crée un dictionnaire (mise en place sous forme de table de hachage) avec h comme fonction de hachage et w cases *)
let creer h w =
    {hache=h; donnees=Array.make w []; largeur=w};;
    
(** [recherche t k] renvoie si oui ou non le dictionnaire t contient une entrée pour la clé k *)
let recherche t k =
    let hash = t.hache k in
    let rec valeur l k = match l with
        | (k', _)::q -> k'= k || valeur q k
        | [] -> false in
    valeur t.donnees.(hash) k;;

(** [element t k] renvoie la valeur de la clé k dans le dictionnaire t *)
let element t k =
    let hash = t.hache k in
    let rec valeur l k = match l with
        | (key, v)::q when key = k -> v
        | (_, _)::q -> valeur q k
        | [] -> failwith "Element non trouvé" in
    valeur t.donnees.(hash) k;;
    
(** [ajout t k e] ajoute l'élément e au dictionnaire t avec comme clé k *)
let ajout t k e = 
    if recherche t k then failwith "Clé existante"
    else let hash = t.hache k in
        t.donnees.(hash) <- (k, e)::t.donnees.(hash);;
        
(** [suppr t k] supprime l'entrée pour la clé k du dictionnaire t *)
let suppr t k =
    let hash = t.hache k in
    let rec suppr_l k l = match l with
        | (key, l)::q when key=k -> q
        | e::q -> e::(suppr_l k q)
        | [] -> [] in
    t.donnees.(hash) <- (suppr_l k t.donnees.(hash));;