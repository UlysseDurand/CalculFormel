(*open Misc;;
open Groupe;;
open Analyse;;
open Analyse3;;*)

let exprlatexa = "frac{x_0+x_1}{ln(x_0-x_1)}";;
let expra = latex_en_expression exprlatexa;;
let exprb = derive 0 expra;;
let exprlatexb = affiche exprb;;

ecritdansfichier "res.txt" [exprlatexa^"\\\\" ; exprlatexb];;