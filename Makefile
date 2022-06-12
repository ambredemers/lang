COMP = ocamlopt
CARG = -c -w A-4-40-42 -cc clang -ccopt -Ofast
LARG = unix.cmxa -cc clang -ccopt -Ofast -o
PROD = main
OBJS = lex.cmx parse.cmx main.cmx

all : $(PROD)

run : clean all
	clear
	./$(PROD) < text.txt

$(PROD) : $(OBJS)
	$(COMP) $(LARG) $@ $^

%.cmx: %.ml
	$(COMP) $(CARG) $<

%.cmi: %.mli
	$(COMP) $(CARG) $<

%.cma: %.ml
	ocamlc -a $< -o $@

clean:
	rm $(PROD) *.o *.cmi *.cmx *.cma *.cmo *.mli lexer.ml parser.ml parser.output || true
