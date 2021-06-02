open Misc



(*###### DES PETITES FONCTIONS BIEN UTILES ######*)

let explode s = let res = ref [] in for i=0 to (String.length s)-1 do res:=((String.get s i)::(!res)); done;List.rev !res
let rec implode cl = match cl with |[]->"" |x::xs -> (Char.escaped x)^(implode xs)





(*###### LES TYPES ######*)

type 'a espace_ecriture = {neutre : 'a ; operation : 'a->'a->'a}

type ('q, 's) automate_det = ('q -> bool)*'q*('q -> bool)*('q*'s -> 'q)
(*Q \inc 'q, \Sigma = 's, structure : (Q,I,F,delta)*)

type ('q, 's) automate_nondet = ('q -> bool)*('q list)*('q -> bool)*('q*'s -> 'q list)
(*Hyp : Q q = true \forall q \in I \union F, Q stable par delta*)

type ('q, 's) automate_priorisant = ('q, 's) automate_nondet

type ('q, 's, 't) automate_det_quiecrit = ('t espace_ecriture)*(('q*'t, 's ) automate_det)
(*structure : (monsig2,(Q,I,F,delta))
Q \inc 'q
\Sigma = 's
\Sigma_2 = 't (o\`u qu'on ecrit)
*)

type ('q, 's, 't) automate_nondet_quiecrit = ('t espace_ecriture)*(('q*'t, 's ) automate_nondet)

type ('q, 's, 't) automate_priorisant_quiecrit = ('t espace_ecriture)*(('q*'t, 's ) automate_nondet)



type ('q, 'sm, 's, 't) automate_det_quiecrit_avecmemoire = ('q*'sm,'s,'t) automate_det_quiecrit

type ('q, 'sm, 's, 't) automate_nondet_quiecrit_avecmemoire = ('q*'sm,'s,'t) automate_nondet_quiecrit

type ('q, 'sm, 's, 't) automate_priorisant_quiecrit_avecmemoire = ('q*'sm,'s,'t) automate_nondet_quiecrit



type pattern = string*int





(*###### LES FONCTIONS SUR LES TYPES ######*)

let rec deltaetoile_det (qq,i,f,delta) (q,m) = match m with
	|[] -> q
	|x::xs -> deltaetoile_det (qq,i,f,delta) (delta (q,x),xs)

let rec reconnaitmot_det (qq,i,f,delta) m = f (deltaetoile_det (qq,i,f,delta) (i,m))



let rec deltaetoile_nondet (qq,i,f,delta) (q,m) = match m with
	|[] -> [q]
	|x::xs -> let suivants = delta (q,x) in
		List.flatten (
			List.map (fun qs -> deltaetoile_nondet (qq,i,f,delta) (qs,xs) )
					 suivants
		)

let rec reconnaitmot_nondet (qq,i,f,delta) m =
	List.mem true (List.map (fun x->f (deltaetoile_nondet (qq,i,f,delta) (x,m)) ) i)

let rec deltetaetoile_det_quiecrit_avecmemoire (esecr,(qq,i,f,delteta)) ((q,mem),m) = match m with
    |[]->esecr.neutre
    |tete::queue->
		let ((nq,nmem),aecrire) = (delteta ((q,mem),tete)) in
		esecr.operation
			aecrire
			(
			deltetaetoile_det_quiecrit_avecmemoire
				(esecr,(qq,i,f,delteta))
				((nq,nmem),queue)
			)


let rec deltetaetoile_det_quiecrit (esecr,(qq,i,f,delteta)) (q,m) =
	let delta = (
		fun ((q,dejaecrit),l) ->
			let nq,aecrire = (delteta (q,l)) in
			(nq,esecr.operation dejaecrit aecrire)
		)
		in  let (a,b) = (deltaetoile_det (qq,i,f,delta) (q,m)) in b

let executeautomate (esecr,(qq,i,f,delteta)) m = deltetaetoile_det_quiecrit (esecr,(qq,i,f,delteta)) (i,m)

(*automate priorisant avec un puits *)
let rec deltaetoile_prio_quiecrit (qq,i,f,delta) puits ((q,aecr),m) = match m with
	|[] -> f (q,aecr)
	|x::xs ->(*print_string "banana ";*) if q=puits then Erreur else
			  let suivants = delta ((q,aecr),x) in
			  unvrai (fun unq -> deltaetoile_prio_quiecrit (qq,i,f,delta) puits (unq,xs)) suivants



(*avec l'espace d'ecriture 'sig qui a un vide et une operation de concatenation
un automate priorisant qui ecrit dans 'sig ayant un puits (etat non final sans transition externe)*)
let rec deltetaetoile_prio_quiecrit (esecr,(qq,i,f,delteta)) puits ((q,aecr),m) =
	let neutre,concat = esecr.neutre, esecr.operation in
	(* print_string (implode m);print_string " : ";print_string (implode aecr.(0));print_newline (); *)
	match m with
	|[] -> Ok(((f (q,aecr)),esecr.neutre))
	|x::xs ->
		if q=puits then Erreur else
		unvraiteretourne
			(
				fun (unq,motdureste) ->
					let a = deltetaetoile_prio_quiecrit (esecr,(qq,i,f,delteta)) puits ((unq,motdureste),xs) in
					a
			)
			(delteta ((q,aecr),x))





(*########## MES APPLICATIONS ##########*)

(*espace d'ecriture des mots*)
let espaceecriture_mots = {neutre = [] ; operation =  fun x y -> x@y}





(*##### l'espace d'ecriture autoprio #####*)

type ('q, 'sm, 's, 't) typautoprio =
	AutoVide |
	Aut of (('q, 's, 't) automate_priorisant_quiecrit) |
	AutDom of char*(('q, 's, 't) automate_priorisant_quiecrit)

(*##### la concatenation sur le l'espace d'ecriture autoprio #####*)
let rec ajoufin autoa autob lpass =
	let (especr1,(leq1,i1,f1,d1))=autoa in
	let (especr2,(leq2,i2,f2,d2))=autob in
	let d3 (q,l) =
		if leq1 q
		then
			(if ((f1 q) && (l=lpass))
			then
				(i2)@(d1 (q,l))
			else
				d1 (q,l))
		else
			d2 (q,l)
		in
	let leq3 q = (leq1 q)||(leq2 q) in
	(especr1,(leq3,i1,f2,d3))

let rec agraphefin autoa autob =
	let (especr1,(leq1,i1,f1,d1))=autoa in
	let (especr2,(leq2,i2,f2,d2))=autob in
	let phi (q,aecr) = fun (a,b)-> if a= fst (List.hd i2) then (q,b) else (a,b) in
	let d3 (q,l) =
		if (leq1 q)
		then
			if (f1 q)
			then
				(d1 (q,l))@(List.map (phi q) (d2 (List.hd i2,l)))
			else
				d1 (q,l)
		else
			List.map (phi q) (d2 (q,l)) in
	let leq3 (q,aecr) = if q=(fst (List.hd i2)) then false else (leq1 (q,aecr))||(leq2 (q,aecr)) in
  let f3 (q,aecr) = (f1 ( (q,aecr) ) ) in
	(especr1,(leq3,i1,f3,d3))

let rec concat_autoprio auta autb = match (auta,autb) with
	|AutoVide,AutoVide -> AutoVide
	|AutoVide,Aut(db,lautob) -> autb
	|AutoVide,AutDom(d,lautob) -> autb
	|Aut(lautoa),AutoVide -> auta
	|AutDom(da,lautoa),AutoVide -> auta
	|Aut(lautoa),Aut(lautob) -> auta
	|AutDom(da,lautoa),AutDom(db,lautob) -> AutDom('%', ajoufin lautoa lautob db)
	|Aut(lautoa),AutDom(db,lautob) -> AutDom('%',ajoufin lautoa lautob db)
	|AutDom(da,lautoa),Aut(lautob) -> Aut(agraphefin lautoa lautob)

(*##### l'espace d'ecriture autoprio #####*)
let autoprio = {neutre = AutoVide ; operation = concat_autoprio}




(*##### l'espace d'ecriture dansph #####*)
type type_dansph = (char list) array

let arraybase n i l = let res = Array.make n [] in res.(i) <- [l];res
let arrayvide n = Array.make n []

let concat_dansph x y =
	let n = (Array.length x) in
	let res = Array.make n [] in
	for i=0 to n-1 do res.(i)<-x.(i)@y.(i);done;
	res

let dansph n = {neutre = arrayvide n; operation = concat_dansph}





(*##### autobrique #####*)
let recoph n id a =
	Aut(
		(
			(dansph n),
			(
				(fun (q,m)->q=id),
				[id,arrayvide n],
				(fun (q,m)->q=id),
				(fun ((q,m),l)->if q=id then [(q,concat_dansph (arraybase n a l) m)] else failwith "olala")
			)
		)
	)

let autobrique n id puits lett =
	AutDom(
		lett,
		(
			(dansph n),
			(
				(fun (q,m)->q=id),
				[id,arrayvide n],
				(fun (q,m)->q=id),
				(fun ((q,m),l)->(*affichetrucs ((q,m),l);*)if q=id then [(puits,concat_dansph (arrayvide n) m)] else failwith ("olala ! "^(Char.escaped lett)^", id "^(string_of_int id)^",mais etat "^(string_of_int q)))
			)
		)
	)





(*##### l'automagique #####*)
let incr (pe,s,a,b) = (pe+1,s,a,b)

let lindice l e = let res = ref (-1) in for i=0 to (Array.length l)-1 do if l.(i) = e then (res:=i); done;!res

let chiffres = [|'0';'1';'2';'3';'4';'5';'6';'7';'8';'9'|]

let chiffre  e = lindice chiffres e

let deltadugrosautomate acc n ((q,mem),l) =
	let demande_autobrique lett =  let res = autobrique n (!acc) (-1) lett in (*print_string "ajoute autobrique ";print_int (!acc);print_string " ";print_string (Char.escaped lett);print_newline ();*)acc:=(!acc)+1;res in
	let demande_recoph a = let res = recoph n (!acc) a in (*print_string "ajoute recoph ";print_int (!acc);print_string " ";print_string (Char.escaped delim);print_newline ();*)acc:=(!acc)+1;res in
	let rec autodelim s = match s with
		|[] -> AutoVide
		|[x]-> AutoVide
		|tete::queue -> concat_autoprio (demande_autobrique tete) (autodelim queue) in
    let rec fung (s,a,b) =
        if a > b then AutoVide else
        if a = b then (demande_recoph a)
        else concat_autoprio (concat_autoprio (demande_recoph a) (autodelim s) ) (fung (s,a+1,b)) in
    let (pe,s,a,b) = mem in
    let erreur () = failwith ("ERREUR a la position "^(string_of_int pe)^", etat "^(string_of_int q)^", lettre "^(Char.escaped l)) in
    match q with
        |0 -> if l='%' then ((1,(pe+1,[],0,0)),AutoVide)
                else ((0,(incr mem)),(demande_autobrique l))
        |1 -> if l='%' then ((0,incr mem),AutoVide) else
                if l='|' then ((2,incr mem),AutoVide)
                else ((1,(pe+1,s@[l],a,b)),AutoVide)
        |2 -> if l=';' then ((2,(pe+1,s,0,0)),(demande_recoph a)) else
                let lc = (chiffre l) in if lc>=0 then ((2,(pe+1,s,10*a+lc,b)),AutoVide) else
                if l='a' then ((3,(pe+1,s,0,n)),AutoVide) else
                if l='-' then ((3,incr mem),AutoVide) else
                if l='%' then ((0,incr mem),AutoVide) else
                erreur ()
        |3 -> if l=';' then ((2,(pe+1,s,0,0)),(fung (s,a,b))) else
                let lc = (chiffre l) in if lc>=0 then ((3,(pe+1,s,a,10*b+lc)),AutoVide) else
                erreur ()
        |_ -> failwith "ETAT NON ATTEIGNABLE"

let letatinitial n = autobrique n 0 (-1) '%'
let legrosautomate n = (autoprio,((fun x->true),((0,(0,[],0,0)),letatinitial n),(fun x -> x=0),deltadugrosautomate (ref 1) n))





(*##### fonction magique #####*)

let ajoutelafin n aut puits etafin = match aut with
	|AutoVide->AutoVide
	|AutDom(d,lauto)->aut
	|Aut(d,lauto)-> concat_autoprio aut (recoph n etafin 0)

let prendlautquiecr aut = match aut with
    |AutoVide -> failwith "pas d'autoQe ici"
    |AutDom(l,levraiautoqe) -> levraiautoqe
    |Aut(levraiautoqe) -> levraiautoqe

let levaluation mot (exp_pattern,n) =
    let levraiauto = prendlautquiecr (ajoutelafin n (executeautomate (legrosautomate n) (explode exp_pattern)) (-1) (-2)) in
    deltetaetoile_prio_quiecrit levraiauto (-1) ((0,arrayvide n),mot)
