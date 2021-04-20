
type groupe_fini = {ordre : int ; ope : int*int->int ; neutre : int };;

let verifie_groupe_fini g = let (o,ope,n) = (g.ordre,g.ope,g.neutre) in
	let res = Array.make o false in
	let lci = ref true in
	for i=0 to o-1 do
		for j=0 to o-1 do
			if ope (i,j) = n then
				(res.(i) <- true);
			if (ope (i,j) < 0 || ope (i,j) >= o) then 
				(lci:=false);
		done;
	done;
	(!lci) && (not (Array.mem false res));;

let groupezsurnz n = {ordre = n;ope = (fun (a,b) -> (a+b) mod n); neutre = 0};;

let matrice_groupe_fini g = let (o,ope,n) = (g.ordre,g.ope,g.neutre) in
	let res = Array.make_matrix o o 0 in
	for i=0 to o-1 do
		for j=0 to o-1 do
			res.(i).(j) <- ope (i,j);
		done;
	done;
	res;;