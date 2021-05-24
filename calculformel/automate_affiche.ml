(*##### FONCTIONS UTILES #####*)

let explode s = let res = ref [] in for i=0 to (String.length s)-1 do res:=((String.get s i)::(!res)); done;!res
let rec implode cl = match cl with |[]->"" |x::xs -> (implode xs)^(Char.escaped x)

let decomp comp = (fun x->let a,b = comp x in a),(fun x->let a,b = comp x in b)

let compo f g = fun x->(f x, g x)





(*##### TYPES UTILES #####*)

type 'q automate = Aut of 'q*('q -> bool)*( ('q->char)->'q)
(*Ici, Q = 'q et \Sigma = char, on a Aut(I,F,\delta)*)

type 'q automatequiecrit = AutQe of 'q*('q -> bool)*(('q*char)->'q*char list)
(*Ici, Q = `q, \Sigma = \Sigma ' = char , on a AutQe(I,F,(\delta,\eta))*)

type ('q,'a) automatequiecritavecmemoire = (('q*'a) automatequiecrit)





(*##### FONCTIONS SUR CES TYPES #####*)

let rec delta_etoile a (q,m) = let (initiaux,finaux,delta) = a in match m with
    |[]-> q
    |x::xs->delta_etoile a ((delta q x),xs)

let rec delta_etoile_ecrit a (q,m) = let (initiaux,finaux,(delta,eta)) = a in match m with
    |[]-> q
    |x::xs->delta_etoile_ecrit a ((delta q x),xs)

let rec eta_etoile_ecrit a m (q,mem) = let AutQe(initial,finaux,grossefonc) = a in let delta,eta = decomp grossefonc in match m with
    |[]->[]
    |x::xs->let nq,nmem = (delta ((q,mem),x)) in (eta ((q,mem),x) )@(eta_etoile_ecrit a xs (nq,nmem))

let executautomate a m = let AutQe(initial,finaux,grossefonc) = a in eta_etoile_ecrit a m initial





(*##### AutoSpecial #####*)

let incr (pe,s,a,b) = (pe+1,s,a,b)

let lindice l e = let res = ref (-1) in for i=0 to (Array.length l)-1 do if l.(i) = e then (res:=i); done;!res

let a =0

let chiffres = [|'0';'1';'2';'3';'4';'5';'6';'7';'8';'9'|]

let chiffre  e = lindice chiffres e

type memoire = int*(char list)*int*int

type etat = int*memoire

let lafonct n rendph ((q,mem),l) =
    let rec fung (s,a,b) =
        if a > b then [] else
        if a = b then rendph a
        else (rendph a)@s@(fung (s,a+1,b)) in
    let (pe,s,a,b) = mem in
    let erreur () = failwith ("ERREUR a la position "^(string_of_int pe)^", etat "^(string_of_int q)^", lettre "^(Char.escaped l)) in
    match q with
        |0 -> if l='%' then (1,(pe+1,[],0,0)),[]
                else (0,incr mem),[l]
        |1 -> if l='%' then (0,incr mem),[] else
                if l='|' then (2,incr mem),[]
                else (1,(pe+1,s@[l],a,b)),[]
        |2 -> if l=';' then (2,(pe+1,s,0,0)),(rendph a) else
                let lc = (chiffre l) in if lc>=0 then (2,(pe+1,s,10*a+lc,b)),[] else
                if l='a' then (3,(pe+1,s,0,n)),[] else
                if l='-' then (3,incr mem),[] else
                if l='%' then (0,incr mem),[] else
                erreur ()
        |3 -> if l=';' then (2,(pe+1,s,0,0)),(fung (s,a,b)) else
                let lc = (chiffre l) in if lc>=0 then (3,(pe+1,s,a,10*b+lc)),[] else
                erreur ()
        |_ -> failwith "ETAT NON ATTEIGNABLE"

let rec lerendph a = ['x';'_']@['{']@(List.rev (explode (string_of_int a ) ))@['}']

let automatelatex n rendph = AutQe( (0,(0,[],0,0)) , (fun (q,mem) -> q=0) ,(lafonct n rendph))





(*##### Compile_Languinv #####*)

let compile_languinv n rendph txt = implode (List.rev (executautomate (automatelatex n rendph) (List.rev(explode txt))))
