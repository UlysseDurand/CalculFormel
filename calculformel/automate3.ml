(*########## DES PETITES FONCTIONS BIEN UTILES ##########*)
let fst (x,y) = x;;
let snd (x,y) = y;;

let explode s = let res = ref [] in for i=0 to (String.length s)-1 do res:=((String.get s i)::(!res)); done;List.rev !res;;
let rec implode cl = match cl with |[]->"" |x::xs -> (Char.escaped x)^(implode xs);;

let decomp comp = (fun x->let a,b = comp x in a),(fun x->let a,b = comp x in b);;

let rec unvrai f l = match l with
	|[] -> false
	|x::xs -> if f x then true else unvrai f xs;;

let rec unvraiteretourne f l casbase =  (*print_string "ok ! ";*)match l with
	|[] -> (false,casbase)
	|x::xs ->let (a,b) = f x in if a then (a,b) else unvraiteretourne f xs casbase;;



(*########## LES TYPES ##########*)
type 'a espace_ecriture = {neutre : 'a ; operation : 'a->'a->'a}

type ('q, 's) automate_det = ('q -> bool)*'q*('q -> bool)*('q*'s -> 'q);;
(*Q \inc 'q, \Sigma = 's, structure : (Q,I,F,delta)*)

type ('q, 's) automate_nondet = ('q -> bool)*('q list)*('q -> bool)*('q*'s -> 'q list);;
(*Hyp : Q q = true \forall q \in I \union F, Q stable par delta*)

type ('q, 's) automate_priorisant = ('q, 's) automate_nondet;;



type ('q, 's, 't) automate_det_quiecrit = ('t espace_ecriture)*(('q*'t, 's ) automate_det);;
(*structure : (monsig2,(Q,I,F,delta)) 
Q \inc 'q
\Sigma = 's
\Sigma_2 = 't (o\`u qu'on ecrit)
*)

type ('q, 's, 't) automate_nondet_quiecrit = ('t espace_ecriture)*(('q*'t, 's ) automate_nondet);;

type ('q, 's, 't) automate_priorisant_quiecrit = ('t espace_ecriture)*(('q*'t, 's ) automate_nondet);;



type ('q, 'sm, 's, 't) automate_det_quiecrit_avecmemoire = ('q*'sm,'s,'t) automate_det_quiecrit;;

type ('q, 'sm, 's, 't) automate_nondet_quiecrit_avecmemoire = ('q*'sm,'s,'t) automate_nondet_quiecrit;;

type ('q, 'sm, 's, 't) automate_priorisant_quiecrit_avecmemoire = ('q*'sm,'s,'t) automate_nondet_quiecrit;;



(*########## LES FONCTIONS SUR LES TYPES ##########*)

let rec deltaetoile_det (qq,i,f,delta) (q,m) = match m with
	|[] -> q
	|x::xs -> deltaetoile_det (qq,i,f,delta) (delta (q,x),xs);;

let rec reconnaitmot_det (qq,i,f,delta) m = f (deltaetoile_det (qq,i,f,delta) (i,m));;



let rec deltaetoile_nondet (qq,i,f,delta) (q,m) = match m with
	|[] -> [q]
	|x::xs -> let suivants = delta (q,x) in
		List.flatten (
			List.map (fun qs -> deltaetoile_nondet (qq,i,f,delta) (qs,xs) )
					 suivants 
		);;

let rec reconnaitmot_nondet (qq,i,f,delta) m = 
	List.mem true (List.map (fun x->f (deltaetoile_nondet (qq,i,f,delta) (x,m)) ) i);;

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
	;;

let rec deltetaetoile_det_quiecrit (esecr,(qq,i,f,delteta)) (q,m) =
	let delta = (
		fun ((q,dejaecrit),l) -> 
			let nq,aecrire = (delteta (q,l)) in
			(nq,esecr.operation dejaecrit aecrire) 
		)
		in  let (a,b) = (deltaetoile_det (qq,i,f,delta) (q,m)) in b;;

let executeautomate (esecr,(qq,i,f,delteta)) m = deltetaetoile_det_quiecrit (esecr,(qq,i,f,delteta)) (i,m);;

(*automate priorisant avec un puits *)
let rec deltaetoile_prio_quiecrit (qq,i,f,delta) puits ((q,aecr),m) = match m with
	|[] -> f (q,aecr)
	|x::xs ->(*print_string "banana ";*) if q=puits then false else 
			  let suivants = delta ((q,aecr),x) in
			  unvrai (fun unq -> deltaetoile_prio_quiecrit (qq,i,f,delta) puits (unq,xs)) suivants;;


(*avec l'espace d'ecriture 'sig qui a un vide et une operation de concatenation
un automate priorisant qui ecrit dans 'sig ayant un puits (etat non final sans transition externe)*)
let rec deltetaetoile_prio_quiecrit (esecr,(qq,i,f,delteta)) puits ((q,aecr),m) =
	let neutre,concat = esecr.neutre, esecr.operation in(*
	print_string "je suis aaaaaaici";*)
	match m with
	|[] -> ((f (q,aecr)),esecr.neutre)
	|x::xs -> 
		if q=puits then (false,esecr.neutre) else
		unvraiteretourne
			(
				fun (unq,motdureste) -> 
					let (a,b) = deltetaetoile_prio_quiecrit (esecr,(qq,i,f,delteta)) puits ((unq,motdureste),xs) in 
					(a,concat aecr b)
			)
			(delteta ((q,aecr),x))
            neutre
            ;;



(*########## MES APPLICATIONS ##########*)

(*espace d'ecriture des mots*)
let espaceecriture_mots = {neutre = [] ; operation =  fun x y -> x@y};;



type ('q, 'sm, 's, 't) monsig2 = 
	AutoVide | 
	Aut of char*(('q, 's, 't) automate_priorisant_quiecrit) | 
	AutDom of char*(('q, 's, 't) automate_priorisant_quiecrit);;

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
	(especr1,(leq3,i1,f2,d3));;

let rec agraphefin autoa autob = 
	let (especr1,(leq1,i1,f1,d1))=autoa in 
	let (especr2,(leq2,i2,f2,d2))=autob in
	let phi (q,aecr) = (*print_string "AAAH : ";print_int (List.length aecr.(0));*) fun (a,b)-> if a= fst (List.hd i2) then (q,b) else (a,b) in
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
	(especr1,(leq3,i1,f3,d3));;

let rec ajoutefin auta autb = match (auta,autb) with
	|AutoVide,AutoVide -> AutoVide
	|AutoVide,Aut(db,lautob) -> autb
	|AutoVide,AutDom(d,lautob) -> autb
	|Aut(d,lautoa),AutoVide -> auta
	|AutDom(da,lautoa),AutoVide -> auta
	|Aut(da,lautoa),Aut(db,lautob) -> auta
	|AutDom(da,lautoa),AutDom(db,lautob) -> AutDom('%', ajoufin lautoa lautob db)
	|Aut(da,lautoa),AutDom(db,lautob) -> AutDom('%',ajoufin lautoa lautob db)
	|AutDom(da,lautoa),Aut(db,lautob) -> Aut('%',agraphefin lautoa lautob);;
		
let espaceecriture_autoprio = {neutre = AutoVide ; operation = ajoutefin};;



type dernier_sig2 = (char list) array;;

let arraybase n i l = let res = Array.make n [] in res.(i) <- [l];res;;
let arrayvide n = Array.make n [];;

let dernier_ope x y = let n = (Array.length x) in let res = Array.make n [] in for i=0 to n-1 do res.(i)<-x.(i)@y.(i);done;res;;
let dernier_espaceecriture n = {neutre = arrayvide n; operation = dernier_ope};;



 
let affichetrucs ((q,m),l) = print_string ("salut ! "^(string_of_int q)^" "^(Char.escaped l)^" au revoir ")


let recovar n id a delim = 
	Aut(
		delim,
		(
			(dernier_espaceecriture n),
			(
				(fun (q,m)->q=id),
				[id,arrayvide n],
				(fun (q,m)->q=id),
				(fun ((q,m),l)->if q=id then [(q,dernier_ope (arraybase n a l) m)] else failwith "olala")
			)
		)
	);;

let autonaze n id puits lett = 
	AutDom(
		lett,
		(
			(dernier_espaceecriture n),
			(
				(fun (q,m)->q=id),
				[id,arrayvide n],
				(fun (q,m)->q=id),
				(fun ((q,m),l)->(*print_int q;*)(*affichetrucs ((q,m),l);*)if q=id then [(puits,dernier_ope (arrayvide n) m)] else failwith ("olala ! "^(Char.escaped lett)^", id "^(string_of_int id)^",mais etat "^(string_of_int q)))
			)
		)
	);;



let incr (pe,s,a,b) = (pe+1,s,a,b);;

let lindice l e = let res = ref (-1) in for i=0 to (Array.length l)-1 do if l.(i) = e then (res:=i); done;!res;; 

let chiffres = [|'0';'1';'2';'3';'4';'5';'6';'7';'8';'9'|];;

let chiffre  e = lindice chiffres e;;

let deltadugrosautomate acc n ((q,mem),l) = 
	let demande_autonaze lett =  let res = autonaze n (!acc) (-1) lett in (*print_string "ajoute autonaze ";print_int (!acc);print_string " ";print_string (Char.escaped lett);print_newline ();*)acc:=(!acc)+1;res in 
	let demande_recovar a delim = let res = recovar n (!acc) a delim in (*print_string "ajoute recovar ";print_int (!acc);print_string " ";print_string (Char.escaped delim);print_newline ();*)acc:=(!acc)+1;res in
	let rec autodelim s = match s with
		|[] -> AutoVide
		|[x]-> AutoVide
		|tete::queue -> ajoutefin (demande_autonaze tete) (autodelim queue) in
    let rec fung (s,a,b) = 
        if a > b then AutoVide else
        if a = b then (demande_recovar a '%')
        else ajoutefin (ajoutefin (demande_recovar a (List.hd s)) (autodelim s) ) (fung (s,a+1,b)) in
    let (pe,s,a,b) = mem in
    let erreur () = failwith ("ERREUR a la position "^(string_of_int pe)^", etat "^(string_of_int q)^", lettre "^(Char.escaped l)) in
    match q with
        |0 -> if l='%' then ((1,(pe+1,[],0,0)),AutoVide)
                else ((0,(incr mem)),(demande_autonaze l))
        |1 -> if l='%' then ((0,incr mem),AutoVide) else 
                if l='|' then ((2,incr mem),AutoVide)
                else ((1,(pe+1,s@[l],a,b)),AutoVide)
        |2 -> if l=';' then ((2,(pe+1,s,0,0)),(demande_recovar a '%')) else 
                let lc = (chiffre l) in if lc>=0 then ((2,(pe+1,s,10*a+lc,b)),AutoVide) else
                if l='a' then ((3,(pe+1,s,0,n)),AutoVide) else 
                if l='-' then ((3,incr mem),AutoVide) else
                if l='%' then ((0,incr mem),AutoVide) else
                erreur ()
        |3 -> if l=';' then ((2,(pe+1,s,0,0)),(fung (s,a,b))) else 
                let lc = (chiffre l) in if lc>=0 then ((3,(pe+1,s,a,10*b+lc)),AutoVide) else 
                erreur ()
        |_ -> failwith "ETAT NON ATTEIGNABLE";;

let letatinitial n = autonaze n 0 (-1) '%';;
let legrosautomate n = (espaceecriture_autoprio,((fun x->true),((0,(0,[],0,0)),letatinitial n),(fun x -> x=0),deltadugrosautomate (ref 1) n));;


