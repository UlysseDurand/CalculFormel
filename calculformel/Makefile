all: compile clean execute
compile:
	@ocamlopt misc.ml analyse.ml groupe.ml main.ml -o sortie
execute:
	@echo "#####"
	@echo ""
	./sortie
	@echo ""
	@echo "#####"
clean:
	@rm *.cmi
	@rm *.cmx
	@rm *.o
