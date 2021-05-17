(*onaffichelesmatrices*)
let affiche_matrix fonc m = print_newline ();Array.iter (fun l-> Array.iter (fun e -> (fonc e;print_string " ";)) l;print_newline ();) m;print_newline ();;

let print_bool b = if b then print_string "true" else print_string "false";;

let pow x y = exp (y *. (log x) );;

let array_sum a = let res = ref 0 in Array.iter (fun e->res:=(!res)+e) a;!res;;

let decoupearray ar a b = let res = Array.make (b-a) ar.(0) in for i=a to b-1 do res.(i-a)<-ar.(i);done;res;;

let array_concat l = 
	let m = Array.length l in
	let n = array_sum (Array.map Array.length l) in
	let res = Array.make n l.(0).(0) in
	let pos = ref 0 in
	for i=0 to m-1 do
		for j=0 to (Array.length l.(i))-1 do
			res.(!pos) <- l.(i).(j);
			pos:=(!pos)+1;
		done;
	done;
	res;;

let ecritdansfichier fichier txts = 
	let oc = open_out fichier in
	List.iter (
		fun txt->
			Printf.fprintf oc "%s\n" txt; 
	) txts;
	close_out oc;
	();;

(*encore d'autres*)

let decomp comp = (fun x->let a,b = comp x in a),(fun x->let a,b = comp x in b);;

let fst (x,y) = x;;
let snd (x,y) = y;;


let rec unvrai f l = match l with
	|[] -> false
	|x::xs -> if f x then true else unvrai f xs;;

let rec unvraiteretourne f l casbase =  (*print_string "ok ! ";*)match l with
	|[] -> (false,casbase)
	|x::xs ->let (a,b) = f x in if a then (a,b) else unvraiteretourne f xs casbase;;


let rec unvraiteretourneparindice f n casbase acc =
    if acc = n then (false,casbase) else
    let (a,b) = (f acc) in if a then (a,b) else unvraiteretourneparindice f n casbase (acc+1);;

let rec toutvrai f l = match l with
    |[] -> true
    |x::xs -> if f x then toutvrai f xs else false;;

let rec toutvraiteretourne f laffichage l ope casbase = match l with
    |[]-> (false,casbase)
    |x::xs -> let (a,b) = (f x) in let (c,d) = toutvraiteretourne f laffichage xs ope casbase in if (a && c) then (true, ope b d) else (false, casbase);;

let toutvraiteretourneparindice f n ope casbase laffichage = toutvraiteretourne f laffichage (let res = Array.make n 0 in for i=0 to n-1 do res.(i) <- i;done;Array.to_list res;) ope casbase;;
