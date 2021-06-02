open Misc
open Automate_affiche
open Automate_parse



type operation = {nbvar : int ;affichage : string ; evaluation : float array->float ; derive : deriv}
and expression =
    C of float |
    V of int |
    F of operation * (expression array)
and deriv = NonDeriv | Deriv of ((expression array)->(expression -> expression) -> expression)





(*##### les fonctions magiques #####*)

let rec evalue f v = match f with
    |C(x) -> x
    |V(i) -> v.(i)
    |F(g,fa) -> g.evaluation (Array.map (fun unef-> evalue unef v) fa )

let rec affiche f = match f with
    |C(x) -> string_of_float x
    |V(i) -> "{x_{"^(string_of_int i)^"}}"
    |F(g,va) -> compile_languinv (g.nbvar) (fun a ->(explode (affiche va.(a)))) (g.affichage)

let rec compose f garr = match f with
	|C(x)->C(x)
	|V(i)->garr.(i)
	|F(ge,fa) -> F(ge,Array.map (fun fi -> compose fi garr) fa)

let rec derive k f = match f with
	|C(x)->C(0.);
	|V(i)-> if i=k then C(1.) else C(0.);
	|F(g,va) -> match g.derive with
		|NonDeriv->failwith "Fonction non derivable !";
		|Deriv(laf)->laf va (derive k)





(*##### les operations utiles #####*)

let rec plus = {
        nbvar=2;
		affichage="%|0;%+%|1;%" ;
		evaluation = (fun v->v.(0) +. v.(1) ) ;
		derive = Deriv(fun ar d -> F(plus,[|d ar.(0);d ar.(1)|]) )
	}

let rec oppose = {
        nbvar=1;
		affichage="-%|0;%";
		evaluation = (fun v-> 0.-.v.(0)) ;
    	derive = Deriv(fun ar d -> F(oppose, [|d ar.(0)|]))
	}

let rec moins =
    let vraiefonction = F(plus,[|V 0 ; F(oppose, [| V(1) |] ) |] ) in
    {
        nbvar=2;
        affichage="%|0;%-%|1;%";
        evaluation= evalue vraiefonction;
        derive = Deriv(fun ar d -> d (compose vraiefonction ar) )
    }

let rec fois = {
        nbvar=2;
	    affichage="(%|0;%)*(%|1;%)";
	    evaluation = (fun v-> v.(0) *. v.(1) ) ;
    	derive = Deriv(fun ar d -> F(plus,[| F(fois,[|d ar.(0);ar.(1)|]) ; F(fois,[|ar.(0);d ar.(1)|]) |]))
	}

let rec inverse = {
        nbvar=1;
        affichage="frac{1}{%|0;%}";
        evaluation = (fun v-> 1./.v.(0));
        derive = Deriv(fun ar d -> F(oppose,[|F(fois,[|d ar.(0) ; F(inverse,[|F(fois,[|ar.(0);ar.(0)|])|])|])|] ) )
	}

let rec divise =
    let vraiefonction = (F(fois, [| V 0 ; F(inverse, [|V 1|]) |] )) in
    {
        nbvar=2;
        affichage="frac{%|0;%}{%|1;%}";
        evaluation = evalue vraiefonction;
        derive = Deriv(fun ar d -> d (compose vraiefonction ar) )
    }

let rec ln = {
        nbvar=1;
    	affichage="ln(%|0;%)";
		evaluation= (fun v -> log v.(0));
		derive = Deriv(fun ar d -> F(fois, [|d ar.(0) ; F(inverse,[|ar.(0)|])|]))
	}

let rec exponentielle = {
        nbvar = 1;
		affichage="exp(%|0;%)";
		evaluation = (fun v-> exp v.(0));
		derive = Deriv(fun ar d -> F(fois, [|d ar.(0);F(exponentielle,[|ar.(0)|])|] ))
	}

let exponentielle_base =
	let vraiefonction = (F(exponentielle, [|F(fois,[|V 1 ; F(ln,[|V 0|]) |])|]) ) in
	{
		nbvar=2;
		affichage="%|0;%^{%|1;%}" ;
		evaluation= evalue vraiefonction;
		derive = Deriv(fun ar d -> d (compose vraiefonction ar) )
	}





(*##### les pattern pour le parser #####*)

let unretourope lop lte c = let lc = Array.map lte c in Ok(F(lop,lc))

let lesgrospatterns =
	[
		("frac{%|0;%}{%|1;%}",2,unretourope divise);
		(*("frac{%|0;%}{%|1;%}",2,fun lte c -> let lc = Array.map lte c in F(fois,[| lc.(0) ; F(inverse,[| lc.(1) |]) |] ));*)
		("{%|0;%}",1,fun lte c -> Ok(lte c.(0)));
		("ln(%|0;%)",1, unretourope ln);
		("exp(%|0;%)",1,unretourope exponentielle);
		("(%|0;%*%|1;%)",2,unretourope fois);
		("(%|0;%)",1,fun lte c -> Ok(lte c.(0)));
		("%|0;%+%|1;%",2,unretourope plus);
		("%|0;%-%|1;%",2,unretourope moins);
		("%|0;%-%|1;%",2,fun lte c -> let lc = Array.map lte c in Ok(F(plus, [| lc.(0) ; F(oppose,[| lc.(1) |]) |] )));
		("-%|0;%",1,unretourope oppose);
		("%|0;%^%|1;%",2, unretourope exponentielle_base);
		("x_%|0;%",1,fun lte c ->  (* print_string (implode c.(0)); *) Ok(V (int_of_string (implode c.(0))) ) );
		("%|0;%",1,fun lte c ->  (* print_string (implode c.(0)); *) Ok(C (float_of_string (implode c.(0))) ) )
    ]



(*##### le parser #####*)

let rec parselatex lelatex =
    (* print_string (implode lelatex);print_string " : : "; *)
	(* print_newline (); *)
    unvraiteretourne
        (fun (expr_pat,n,retour) ->
            let a = (levaluation lelatex (expr_pat,n) ) in
            match a with |Ok(resa) -> (retour (fun c -> parselatex c) resa) |Erreur -> Erreur
        )
        lesgrospatterns

let latex_en_expression x = snd (parselatex (explode x))





(*##### le pattern matcher pour le type expression #####*)

let rec arbre_pattern_match expr pattern n =
    let casbase () = Array.make n (C 0.) in
    match pattern with
        |C(x) -> (match expr with
            |C(y) -> (x=y,casbase ())
            |_ -> (false, casbase ()))
        |V(i) -> (true, let res=(casbase ()) in res.(i) <- expr ; res)
        |F(ope,ar) -> match expr with
            |C(y) -> (false, casbase ())
            |V(j) -> (false, casbase ())
            |F(oppe,arr)->
		if not ( (Array.length arr) = (Array.length ar) && ope.affichage=oppe.affichage) then Erreur else
               let opeconc = (fun a b -> let res = casbase () in for i=0 to n-1 do if a.(i) = (C 0.) then (res.(i)<-b.(i)) else (res.(i)<-a.(i)) done;res) in
               let res = ref Ok(Array.make n (C 0.)) in
                   for k=0 to (Array.length arr)-1 do
		        let a = arbre_pattern_match arr.(k) ar.(k) n in match a with
		        	|Ok(resa) -> res:=(match (!res) with |Ok(resres)->Ok(opeconc resa resres) |Erreur -> Erreur);
		        	|Erreur -> res:=Erreur;
			(* print_string "aHA ";print_bool a; *)
                    done;Ok(!res)






(*##### les pattern pour le simplificateur d'expression #####*)

let listesimplifications = [
        (F(oppose,[| C 0.|]), 0, fun sim c -> C 0.);
		(F(fois,[| V 0 ; F(oppose,[|V 1|]) |]), 2,fun sim c -> F(oppose,[|F(fois,Array.map sim c)|]));
		(F(fois,[| F(oppose,[|V 1|]) ; V 0 |]), 2,fun sim c -> F(oppose,[|F(fois,Array.map sim c)|]));
        (F(fois,[| C 0. ; V 0 |]),1,fun sim c -> C 0.);
        (F(fois,[| V 0 ; C 0. |]),1,fun sim c -> C 0.);
        (F(fois,[| C 1. ; V 0 |]),1,fun sim c -> (sim c.(0)) );
        (F(fois,[| V 0 ; C 1. |]),1,fun sim c -> (sim c.(0)) );
        (F(plus,[| V 0 ; C 0. |]),1,fun sim c -> (sim c.(0)) );
        (F(plus,[| C 0. ; V 0 |]),1,fun sim c -> (sim c.(0)) );
		(F(moins,[| V 0 ; C 0. |]),1,fun sim c -> (sim c.(0)));
		(F(moins,[| C 0. ; V 0 |]),1,fun sim c -> F(oppose,[|sim c.(0)|]));
        (F(plus,[| V 0 ; F(oppose,[| V 1 |]) |]),2,fun sim c -> F(moins, Array.map sim c) );
		(F(divise, [| V 0 ; F(divise, [|V 1; V 2 |] ) |]),3,fun sim c -> F(divise,[| F(fois,[|sim c.(0) ; sim c.(2)|]) ; sim c.(1)|]) );
		(F(divise, [|F(divise, [|V 0 ; V 1 |] ) ; V 2 |]),3,fun sim c -> F(divise,[| sim c.(0) ; F(fois,[|sim c.(1) ; sim c.(2)|]) |]) );
		(F(divise, [| F(inverse , [|V 0 |]) ; V 1 |]),2,fun sim c -> F(inverse,[|F(fois,Array.map sim c)|]));
		(F(divise, [| V 0 ; F(inverse , [|V 1|]) |]),2,fun sim c -> F(fois,Array.map sim c));
		(F(divise,[|C 1. ; V 0|]),1,fun sim c -> F(inverse, [|sim c.(0)|]) );
        (F(fois,[| V 0 ; F(inverse, [|V 1|] ) |]), 2, fun sim c -> F(divise, Array.map sim c) );
        (F(fois,[| F(inverse, [|V 1|] ) ; V 0 |]), 2, fun sim c -> F(divise, Array.map sim c) );
        ((F(exponentielle, [|F(fois,[|V 1 ; F(ln,[|V 0|]) |])|]) ),2, fun sim c -> F(exponentielle_base,Array.map sim c))
    ]



(*##### le simplificateur #####*)

let rec simplifiesimpl expr =
    (* print_string (affiche expr);print_string " : : "; *)
    (* print_newline (); *)
    unvraiteretourne
        (fun (arbre_pat,n,retour) ->
            let a = (arbre_pattern_match expr arbre_pat n ) in
            match a with |Ok(resa)-> (retour (fun c -> match c with |Ok(resc) -> (simplifiebis c) |Erreur -> Erreur) resa) |Erreur-> Erreur
        )
        listesimplifications
and simplifiebis expr =
	let a = simplifiesimpl expr in 
	match a with 
		|Ok(resa) -> simplifiebis resa 
		|Erreur -> match expr with
			|F(ope,arr) -> Ok(simplifiesimpl (F(ope,Array.map (fun x -> snd (simplifiebis x)) arr)))
			|x -> expr
