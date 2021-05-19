execute:
	@./calculformel
	
all:
	@make -s compile 
	@make -s clean 
	@make -s execute
	
compile:
	@ocamlopt misc.ml automate.ml automate3.ml analyse.ml main.ml -o calculformel
	
clean:
	@rm -f *.cmi
	@rm -f *.cmx
	@rm -f *.cmo
	@rm -f *.o

complet:
	@make -s execute
	@printf '%s\n' "\documentclass[12pt]{article}" > sortie.tex
	@printf '%s\n' "\usepackage{amsmath} \everymath{\displaystyle}" >> sortie.tex
	@printf '%s\n\n' "\begin{document} res.txt : \\\\" >> sortie.tex
	@printf '%s\n' "\Large \begin{gather*}" >> sortie.tex
	@cat sortie.txt | sed -e 's/frac/\\frac/g' -e 's/(/\\left(/g' -e 's/)/\\right)/g' >> sortie.tex
	@printf '%s\n' "\end{gather*}" >> sortie.tex
	@printf '\n%s\n' "\\end{document}" >> sortie.tex
	@pdflatex sortie.tex
	@make -s cleanlatex

cleanlatex:
	@rm -f *.aux
	@rm -f *.log