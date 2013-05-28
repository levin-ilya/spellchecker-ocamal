all: spellcheck

# These must be in the right order--no forward refs
FILES =  wordDistance.ml editDistance.ml editex.ml bktree.ml spellcheck.ml main.ml 

spellcheck: $(FILES)
	ocamlc -g -o spellcheck $(FILES)

clean: 
	rm -f  *.cmi *.cmo
