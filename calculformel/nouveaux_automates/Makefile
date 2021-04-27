simple:
	@pdflatex nouveaux_automates.tex

final:
	@pdflatex nouveaux_automates.tex
	@pdflatex nouveaux_automates.tex
	@make clean

affiche:
	@evince nouveaux_automates.pdf &

clean:
	@rm -f *.aux
	@rm -f *.fdb_latexmk
	@rm -f *.fls
	@rm -f *.log
	@rm -f *.lyx
