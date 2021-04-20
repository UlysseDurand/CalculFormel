(*fonctions a une seule variable*)
type ope_unaire = Moins | Inv;;

type ope_binaire = Plus | Fois;;

and expr = Aucune | 
		   C of float | 
		   Ou of ope_unaire*expr | 
		   Ob of ope_binaire*expr*expr | 
		   Var | 
		   Fonc of fonct;; 

let rec evalue expr v = match expr with
  |C(x) -> x
  |Var -> v
  |Ob(Plus,x,y) -> (evalue x v) +. (evalue y v)
  |Ob(Fois,x,y) -> (evalue x v) *. (evalue y v)
  |Ou(Moins,x) -> -. (evalue x v)
  |Ou(Inv,x) -> 1. /. (evalue x v)
  |Fonc(f) -> f.evaluation v;;

let rec clarifie expr = match expr with
  |C(x) -> C(x)
  |Var -> Var
  |Ob(Plus,x,C(0.))->x;
  |Ob(Plus,C(0.),y)->y
  |Ob(Plus,x,y) -> Ob(Plus, (clarifie x), (clarifie y))
  |Ob(Fois,C(0.),y) -> C(0.)
  |Ob(Fois,x,C(0.)) -> C(0.)
  |Ob(Fois,x,C(1.)) -> x
  |Ob(Fois,C(1.),y) -> y
  |Ob(Fois,x,y) -> Ob(Fois, (clarifie x),(clarifie y))
  |Ou(Moins,x) -> Ou(Moins, clarifie x)
  |Ou(Inv,x) ->  Ou(Inv, clarifie x )
  |Fonc(f) -> Fonc(f);;


let rec compose a b = match a with
  |C(x) -> C(x)
  |Var -> b
  |Ob(u,v,w) -> Ob(u, compose v b, compose w b)
  |Ou(u,v) -> Ou (u,compose v b)
  |Fonc(f) -> if f.expr = Aucune then {d_d=f.d_d ; d_a = f.d}

let rec enstring expr = match expr with
  |C(x) -> string_of_float x;
  |Var -> "x";
  |Ob(Plus,x,y) -> "("^(enstring x)^"+"^(enstring y)^")";
  |Ob(Fois,x,y) -> "("^(enstring x)^"*"^(enstring y)^")";
  |Ou(Moins,x) -> "-"^(enstring x);
  |Ou(Inv,x) -> "(1/"^(enstring x)^")";;

let rec derive expr = match expr with
  |C(x) -> C(0.);
  |Var -> C(1.);
  |Ob(Plus,x,y) -> Ob(Plus, (derive x), (derive y));
  |Ob(Fois,x,y) -> Ob(Plus, Ob(Fois, (derive x), y), Ob(Fois, (derive y), x));
  |Ou(Moins,x) -> Ou(Moins, derive x);
  |Ou(Inv,x) ->  Ou(Moins, Ob(Fois, (derive x), Ou(Moins, Ob(Fois, x , x))));;

(*Fonctions à plusieurs variables*)

let affichage = Prefixe | Infixe | Suffixe;;

let expr_plus_var = 
    Aucune of string*affichage |
    C of float |
    V of int |
    F of fonction * (expr_plus_var array)
and fonction = 
    {expression : expr_plus_var ;
     nbvarentr : int;
     nbvarsort : int;
     evaluation : float array -> float array};;

let exponentielle_base = {nbvarentr = 2 ; nbvarsort = 1 ; expression : Aucune ; evaluation = (fun v->(pow v.(0) v.(1)) };;

let plus = {nbvarentr = 2 ; nbvarsort = 1 ;  expression : Aucune("+", Infixe) ; evaluation = (fun v->(v.(0) +. v.(1) ) )};;

let compose f g = if f.nbvarentr = g.nbvarsort then {nbvarentr = g.nbvarentr ; nbvarsort = f.nbvarsort ; expression : Aucune("rond",Infixe) ; evaluation = (fun v -> (f.evaluation (g.evaluation v) ) )};;

(* (x,y) |-> x^y+y^x *)
let unefonctionsympas = {nbvar = 2 ; 
                         expression : F(plus,[|
                                                F(exponentielle_base,[|V(0);V(1)|]),
                                                F(exponentielle_base,[|V(1);V(0)|])
                                             |])};

let rec evalue f v = match f.expr with
    |Aucune;;

evalue unefonctionsympas [|2.,3.|];;

