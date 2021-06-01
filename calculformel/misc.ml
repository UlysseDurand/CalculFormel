type 'a unretour = Erreur | Ok of 'a

(*onaffichelesmatrices*)
let affiche_matrix fonc m = print_newline ();Array.iter (fun l-> Array.iter (fun e -> (fonc e;print_string " ";)) l;print_newline ();) m;print_newline ()

let print_bool b = if b then print_string "true" else print_string "false"

let pow x y = exp (y *. (log x) )

let array_sum a = let res = ref 0 in Array.iter (fun e->res:=(!res)+e) a;!res

let decoupearray ar a b = let res = Array.make (b-a) ar.(0) in for i=a to b-1 do res.(i-a)<-ar.(i);done;res

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
	res

let ecritdansfichier fichier txts =
	let oc = open_out fichier in
	List.iter (
		fun txt->
			Printf.fprintf oc "%s\n" txt;
	) txts;
	close_out oc;
	();;

let read_file filename =
	let lines = ref [] in
	let chan = open_in filename in
	try
	  while true; do
		lines := input_line chan :: !lines
	  done; !lines
	with End_of_file ->
	  close_in chan;
	List.rev !lines

(*encore d'autres*)

let decomp comp = (fun x->let a,b = comp x in a),(fun x->let a,b = comp x in b)

let rec unvrai f l = match l with
	|[] -> Erreur
	|x::xs -> let a = f x in match a with
		|Ok(resa) -> Ok(true)
		|Erreur -> unvrai f xs

let rec unvraiteretourne f l =  (*print_string "ok ! ";*)match l with
	|[] -> Erreur
	|[x] -> f x
	|x::xs -> match (f x) with
	 	|Ok(resa) -> (f x)
		|Erreur -> unvraiteretourne f xs


let rec unvraiteretourneparindice f n acc =
    if acc = n then Erreur else
    let a = (f acc) in match a with |Ok(resa)->a |Erreur -> unvraiteretourneparindice f n (acc+1)

let rec toutvrai f l = match l with
    |[] -> true
    |x::xs -> if f x then toutvrai f xs else false

let rec toutvraiteretourne f l ope = match l with
    |[]-> Erreur
    |[x] -> f x
    |x::xs -> let a = (f x) in let b = toutvraiteretourne f xs ope in
			match (a,b) with
				|(Ok(resa),Ok(resb))->Ok(ope resa resb)
				|_ -> Erreur

let toutvraiteretourneparindice f n ope = toutvraiteretourne f (let res = Array.make n 0 in for i=0 to n-1 do res.(i) <- i;done;Array.to_list res;) ope
