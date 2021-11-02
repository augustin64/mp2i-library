(** définition du type tableau dynamique *)
type ('a, 'b) table_dyn = {
    hache: int -> 'a -> int;
    mutable taille: int;
    mutable donnees: ('a * 'b) list array;
    mutable largeur: int
};;

(** [creer_dyn h] crée et renvoie un tableau dynamique vide avec h comme fonction de hachage *)
let creer_dyn h =
    {hache=h; taille=0; donnees=[| [] |]; largeur=1};;

(** [rearrange t w2] change la taille du tableau dynamique t et réarrange les données qui y sont contenues *)
let rearrange t w2 =
    let a2 = Array.make w2 [] in
    let rec distrib l = match l with
    | [] -> ()
    | e::q -> let h = t.hache w2 (fst e) in
            a2.(h) <- e::a2.(h);
            distrib q in
    for i=0 to t.largeur - 1 do
        distrib t.donnees.(i)
    done;
    t.donnees <- a2;
    t.largeur <- w2;;
    
(** TODO: fonction add pour ajouter un élément, si le tableau est plein, utilise la fonction rearrange *)