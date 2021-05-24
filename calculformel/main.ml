open Misc
open Analyse

let exprlatexa = List.hd (read_file "entree.txt");;
let expra = latex_en_expression exprlatexa;; (*parse le latex*)
let exprb = (derive 0 expra);; 				 (*derive l'expression par rapport à x_0*)
let exprbsimpl = snd (simplifiebis exprb);;	 (*simplifie l'expression obtenue*)
let exprlatexb = affiche exprb;;		 (*transforme l'expression en latex*)

ecritdansfichier "sortie.txt" [(affiche expra); "\\\\" ; exprlatexb];;
