(* let affichemieux l = l;; *)

let affichemieux l = print_newline (); List.iter (fun lal -> List.iter (fun (a,b)->print_int a;print_string " ";) lal;print_string " ; ";) l;print_newline ();;
let afficheautomate n (AutDom(l,(espp,(qq,i,f,delteta)))) l = affichemieux (List.map (fun (q,lett)->delteta ((q,arrayvide n),lett) ) l);;

let prendlaut aut = match aut with
    |AutoVide -> failwith "pas d'auto ici"
    |AutDom(l,(espp,levraiauto)) -> levraiauto
    |Aut(l,(espp,levraiauto)) -> levraiauto;;


let lepattern = "sal%|0;%";;


let sujetdetest = (executeautomate (legrosautomate 1) (explode lepattern));;

let untest x t a = let (qq,i,f,delteta) = prendlaut x in delteta (t,a);;
let unotrtest x a = let (qq,i,f,delteta) = prendlaut x in qq (a);;


(* let (AutDom(l,(espp,(qq,i,f,delteta)))) = sujetdetest in qq (3,arrayvide 2) *)

(* let autrautomate = ajoutefin (autonaze 0 (-1) 0 '%') sujetdetest ;; *)



 (* (afficheautomate 0 autrautomate [(-1,'s');(1,'a')]);; *)(*
unotrtest sujetdetest (3,[|[]|]);;
untest sujetdetest (3,[|[]|]) 'o';;
deltetaetoile_prio_quiecrit (prendlautquiecr sujetdetest) (-1) ((0,[|[]|]),(explode "salga"));;
untest sujetdetest (1,[|[]|]) 'a';;
untest sujetdetest (2,[|[]|]) 'l';;
untest sujetdetest (3,[|[]|]) 't';;
untest sujetdetest (3,[|['t']|]) 'i';;
untest sujetdetest (3,[|['i']|]) 'p';;
untest sujetdetest (4,[|[]|]) 'u';;
 *)(*
prendlautquiecr sujetdetest;;*)



(* let motareconnaitre = explode "saligo";; *)
(* levaluation motareconnaitre (lepattern,1);; *)
