(** complex type, with Imaginary and Real parts*)
type complex = {re: float; im: float};;

(** complex type, with r and theta to represent it *)
type polaire = {r: float; theta:float};;



(** BASIC OPERATIONS **)

(** ( +~ ) defines addition for type complex *)
let ( +~ ) z1 z2 = {re=z1.re+.z2.re; im=z1.im+.z2.im};;

(** ( -~ ) defines substraction for type complex *)
let ( -~ ) z1 z2 = {re=z1.re-.z2.re; im=z1.im-.z2.im};;

(** ( *~ ) defines multiplication for type complex *)
let ( *~ ) z1 z2 = {re=z1.re*.z2.re -. z1.im*.z2.im; im=z1.re*.z2.im +. z2.re*.z1.im};;

(** ( /~ ) defines division for type complex *)
let ( /~ ) z1 z2 =
  let denom = z2.re*.z2.re +. z2.im*.z2.im in
  {re=(z1.re*.z2.re +. z1.im*.z2.im)/.denom; im=(z1.im*.z2.re -. z1.re*.z2.im)/.denom};;



(** COMPLEX-SPECIFIC OPERATIONS **)

(** [conjugue z] returns the conjugue of the complex number z *)
let conjugue z = {re=z.re; im=(-1.)*.z.im};;

(** [abs z1 z2] returns the distance between z1 and z2 in the complex plan *)
let dist z1 z2 =
    let d = z1 -~ z2 in
    Float.sqrt(d.re**2. +. d.im**2.);;
    
(** [module_of z] returns the module of the complex number z, aka its distance to (0, 0)*)
let module_of z =
    dist z {re=0.; im=0.};;
    
(** [complex_of_polaire r theta] converts a complex number from its exponential form to its algebric form *)
let complex_of_polaire e = 
    {re=e.r*.cos e.theta; im=e.r*.sin e.theta};;



(** GEOMETRICAL EQUIVALENTS FROM TP1 **)

(** [milieu z1 z2] returns the complex number in the middle of z1-z2 *)
let milieu z1 z2 =
    (z1 +~ z2) /~ {im=0.; re=2.};;
    
(** [parallelogram z1 z2 z3 z4] returns wether or not the 4 complex numbers represents a parallelogram or not in the complex plan *)
let parallelogram z1 z2 z3 z4 = 
    abs_float ((dist z1 z2) -. (dist z3 z4)) < 0.001 &&
    abs_float ((dist z1 z4) -. (dist z3 z2)) < 0.001;;
    O