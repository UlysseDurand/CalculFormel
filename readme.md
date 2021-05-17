# Logiciel de Calcul Formel en OCaml

Il s'agit d'un projet sans se documenter, il a été réalisé juste avec les connaissances d'option informatique, notamment concernant les automates. Le but étant de forcer la réflexion et la créativité, et de redécouvrir par soi même des concepts déjà existants.

## Fonctionnalitées:
* Comprendre une expression en Latex et en faire un arbre d'expression algébrique (il y a un parser de Latex)
* Afficher un arbre d'expression algébrique en Latex (pour le côté modulable, un compilateur depuis un langage inventé a été mis au point)
* Dériver un arbre expression algébrique par rapport à une variable
* Evaluer une expression algébrique (son arbre en réalité)

## Réalisation
Le projet a été réalisé en très peu de temps (moins de 4 semaines)
Il constitue environ 120h de travail.

L'entièreté de ce qui est contenu dans le projet est le fruit de recherche personnelle. Les méthodes employées et leur implémentation sont personnelles.

## Essayer le projet :
Pour lancer le programme vous devrez vous placer dans le dossier calculformel et lancer 
```bash
make toplevel
```

Vous pourrez modifier le fichier main.ml à votre guise. Voici les fonctions caml ajoutées :
```OCaml
val evalue : expression -> float array -> float = <fun>
val affiche : expression -> string = <fun>
val derive : int -> expression -> expression = <fun>
val latex_en_expression : string -> expression = <fun>
val simplifiebis : expression -> bool * expression = <fun>
```
```simplifiebis``` retourne aussi un booléen, il sera à ```true``` si l'expression donnée est simplifiable et ```false``` sinon.

## Exemple de résultat
après execution du programme suivant : 
```OCaml
let exprlatexa = "x_0^{x_1}+x_1^{x_0}";;
let expra = latex_en_expression exprlatexa;; (*parse le latex*)
let exprb = (derive 0 expra);; 				 (*derive l'expression par rapport à x_0*)
let exprbsimpl = snd (simplifiebis exprb);;	 (*simplifie l'expression obtenue*)
let exprlatexb = affiche exprbsimpl;;		 (*transforme l'expression en latex*)

ecritdansfichier "res.txt" [exprlatexa ; "\\\\" ; exprlatexb];;
```
Voici le contenu de res.txt : 
```latex
x_0^{x_1}+x_1^{x_0}
\\
(frac{{x_{1}}}{{x_{0}}})*({x_{0}}^{x_{1}})+(ln({x_{1}}))*({x_{1}}^{x_{0}})
```
Il s'agit d'une fonction et de sa dérivée par rapport à x_0. Voici le rendu obtenu grâce à Latex :
![alt text](https://github.com/UlysseDurand/CalculFormel/blob/master/readme/resultat.png?raw=true)
