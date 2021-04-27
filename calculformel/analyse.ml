(*open Misc;;*)


type operation = {nbvar : int ;affichage : string ; evaluation : float array->float ; derive : deriv}
and expression = 
    C of float |
    V of int |
    F of operation * (expression array)
and deriv = NonDeriv | Deriv of ((expression array)->(expression -> expression) -> expression);;

let rec plus = {nbvar=2;
			affichage="%|0;%+%|1;%" ; 
			evaluation = (fun v->v.(0) +. v.(1) ) ; 
			derive = Deriv(fun ar d -> F(plus,[|d ar.(0);d ar.(1)|]) )
			} ;;

let rec oppose = {nbvar=1;
			affichage="-%|0;%";
			evaluation = (fun v-> 0.-.v.(0)) ;
			derive = Deriv(fun ar d -> F(oppose, [|d ar.(0)|]))
			} ;;

let rec fois = {nbvar=2;
			affichage="%|0;%*%|1;%";
			evaluation = (fun v-> v.(0) *. v.(1) ) ;
			derive = Deriv(fun ar d -> F(plus,[| F(fois,[|d ar.(0);ar.(1)|]) ; F(fois,[|ar.(0);d ar.(1)|]) |]))
			} ;;

let rec inverse = {nbvar=1;
			affichage="frac{1}{%|0;%}";
			evaluation = (fun v-> 1./.v.(0));
			derive = Deriv(fun ar d -> F(oppose,[|F(fois,[|d ar.(0) ; F(inverse,[|F(fois,[|ar.(0);ar.(0)|])|])|])|] ) )
			};;

let rec ln = {nbvar=1;
			affichage="ln(%|0;%)";
			evaluation= (fun v -> log v.(0));
			derive = Deriv(fun ar d -> F(fois, [|d ar.(0) ; F(inverse,[|ar.(0)|])|]))
			};;

let rec exponentielle = {nbvar = 1;
			affichage="exp(%|0;%)";
			evaluation = (fun v-> exp v.(0));
			derive = Deriv(fun ar d -> F(fois, [|d ar.(0);F(exponentielle,[|ar.(0)|])|] ))
			};;


let rec evalue f v = match f with
    |C(x) -> x
    |V(i) -> v.(i)
    |F(g,fa) -> g.evaluation (Array.mapi (fun i unef-> evalue unef v) fa );;

let rec affiche f = match f with
    |C(x) -> string_of_float x
    |V(i) -> "{x_{"^(string_of_int i)^"}}"
    |F(g,va) -> evaluelatex (g.nbvar) (fun a -> List.rev (explode (affiche va.(a)))) (g.affichage);;

let rec compose f garr = match f with
	|C(x)->C(x)
	|V(i)->garr.(i)
	|F(ge,fa) -> F(ge,Array.map (fun fi -> compose fi garr) fa);;

(*\frac{\partial f}{\partial x_k}*)
let rec derive k f = match f with
	|C(x)->C(0.);
	|V(i)-> if i=k then C(1.) else C(0.);
	|F(g,va) -> match g.derive with 
		|NonDeriv->failwith "Fonction non derivable !";
		|Deriv(laf)->laf va (derive k);;

let exponentielle_base = 
			let vraiefonction = (F(exponentielle, [|F(fois,[|V 1 ; F(ln,[|V 0|]) |])|]) ) in
			{
			nbvar=2;
			affichage="%|0;%^%|1;%" ; 
			evaluation= evalue vraiefonction;
			derive = Deriv(fun ar d -> d (compose vraiefonction [|ar.(0);ar.(1)|]) )
			} ;;

(* (x,y) |-> x^y+y^x *)
let unefonctionsympas = F(plus,[|
								F(exponentielle_base,[| V 0 ; V 1 |] );
                                F(exponentielle_base,[| V 1 ; V 0 |] )
                               |]);;

evalue unefonctionsympas [|2.;3.|];;

affiche unefonctionsympas;;

affiche (F(inverse, [|V 0|]));;

affiche (derive 0 unefonctionsympas);;
