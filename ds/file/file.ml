(** Définition du type file *)
type 'a file={tab: 'a array ; mutable deb: int ; mutable fin: int ; mutable vide: bool}

(** [ajoute f x] ajoute l'élément x à la fin de la file f *)
let ajoute f x =
    if f.deb = f.fin && not f.vide then
        failwith "File pleine"
    else (f.tab.(f.fin) <- x; 
        f.fin <- (f.fin + 1) mod (Array.length f.tab);
        f.vide <- false);;
        
(** [retire f] retire le premier élément de f et le renvoie *)
let retire f =
    if f.vide then
        failwith "File vide"
    else f.deb <- ((f.deb + 1) mod (Array.length f.tab));
    if f.deb = f.fin then f.vide <- true;
    f.tab.((f.deb-1) mod (Array.length f.tab));;