(*fonctions a une seule variable*)
type ope_unaire = Moins | Inv;;

type ope_binaire = Plus | Fois;;

type borne = Ouvert of float | Ferme of float;;

type interval = borne*borne;;

type unionintervaux = interval list;;


type fonct = {d_d : unionintervaux ;
			  d_a : unionintervaux ;
			  expression : expr ;
			  derivee : fonct ;
			  evaluation : float -> float}
and expr = Aucune | 
		   C of float | 
		   Ou of ope_unaire*expr | 
		   Ob of ope_binaire*expr*expr | 
		   Var | 
		   Fonc of fonct;; 

let rec enlevepoint d x = match d with
  |[]->[];
  |t::q-> match t with 
    |(Ouvert(ia),Ouvert(ib)) -> if ia<0. && ib>0. then (Ouvert(ia),Ouvert(0.))::(Ouvert(0.),Ouvert(ib))::(enlevepoint q x) 
                                  else t::(enlevepoint q x);
    |(Ouvert(ia),Ferme(ib)) -> if ib=0. then (Ouvert(ia),Ouvert(ib))::(enlevepoint q x) 
                                 else t::(enlevepoint q x);
    |(Ferme(ia),Ouvert(ib)) -> if ia=0. 
                                 then (Ouvert(ia),Ouvert(ib))::(enlevepoint q x) else t::(enlevepoint q x);
    |(Ferme(ia),Ferme(ib)) -> if ia<0. && ib>0. then (Ferme(ia),Ouvert(0.))::(Ouvert(0.),Ferme(ib))::(enlevepoint q x) else 
                      if ia=0. then (Ouvert(ia),Ferme(ib))::(enlevepoint q x) else
                      if ib=0. then (Ferme(ia),Ouvert(ib))::(enlevepoint q x) else
                      t::(enlevepoint q x)
    ;;


let rec evalue expr v = match expr with
  |C(x) -> x
  |Var -> v
  |Ob(Plus,x,y) -> (evalue x v) +. (evalue y v)
  |Ob(Fois,x,y) -> (evalue x v) *. (evalue y v)
  |Ou(Moins,x) -> -. (evalue x v)
  |Ou(Inv,x) -> 1. /. (evalue x v)
  |Fonc(f) -> f.evaluation v;;

let uneexpression = (Ob(Plus,
                    Ob(Plus,
                      Ob(Fois,
                        Var,
                        Var),
                      Ou(Moins,
                        Var)),
                    C(4.))) 
                  ;;


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
