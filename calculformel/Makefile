arr := "misc.ml" "analyse2.ml" "groupe.ml" "main.ml"
all: compile clean execute
compile:
	@ocamlopt misc.ml analyse2.ml groupe.ml main.ml -o sortie
execute:
	@echo "#####"
	@echo ""
	./sortie
	@echo ""
	@echo "#####"
clean:
	-rm *.cmi
	-rm *.cmx
	-rm *.cmo
	-rm *.o
toplevel:
	@echo "#####"
	@echo ""
	@ocaml < priorlist
	@echo ""
	@echo "#####"
