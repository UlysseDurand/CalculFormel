
(*onaffichelesmatrices*)
let affiche_matrix fonc m = print_newline ();Array.iter (fun l-> Array.iter (fun e -> (fonc e;print_string " ";)) l;print_newline ();) m;print_newline ();;

let rec pow x y = exp (y *. (log x.));;
