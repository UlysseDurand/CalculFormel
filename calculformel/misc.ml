(*onaffichelesmatrices*)
let affiche_matrix fonc m = print_newline ();Array.iter (fun l-> Array.iter (fun e -> (fonc e;print_string " ";)) l;print_newline ();) m;print_newline ();;

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
	List.map (
		fun txt->
			Printf.fprintf oc "%s\n" txt; 
	) txts;
	close_out oc;;
