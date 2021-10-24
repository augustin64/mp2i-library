(** This library provides basic functions that does not belong anywhere else **)

(** [swap x y] swaps the values of x and y and returns unit () *)
let swap x y =
    let tmp = !y in
    y := !x;
    x := tmp;;
    
(** [fact n] returns n! *)
let rec fact n =
    if n = 0 then 1
    else n*fact (n - 1);;
    
(** [sum n] returns the sum of the integers from 0 to n *)
let sum n =
    n*(n+1)/2;;
    
(** [prime n] returns wether n is a prime number or not *)
let prime n =
    let rec aux d = 
        if d = 1 then false
        else n mod d = 0 || aux (d - 1) in
    not (aux (n/2));;
    
(** [pgcd a b] returns the pgcd of a and b, using euclide algorithm *)
let rec pgcd a b =
    if b = 0 then a
    else pgcd b (a mod b);;
    
(** [exp_rapide a n] returns a^n *)
let rec exp_rapide a n =
    if n = 0 then 1
    else let b = exp_rapide a (n/2) in
    if n mod 2 = 0 then b*b
    else a*b*b;;
    
(** [range n] returns a decreasing range from n to 0 [n; n-1; n-2; ...; 2; 1; 0] *)    
let rec range n = (*renvoie une range décroissante commençant à n et finisstant à 0*)
    if n = 0 then [0]
    else n::range (n - 1);;
    