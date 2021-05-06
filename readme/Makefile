simple:
	@pdflatex calculformel.tex
	@make clean

final:
	@pdflatex -shell-escape calculformel.tex
	@pdflatex -shell-escape calculformel.tex
	@make clean

clean:
	@rm -f *.aux
	@rm -f *.fdb_latexmk
	@rm -f *.fls
	@rm -f *.log
	@rm -f *.lyx
	@rm -f *.lof
