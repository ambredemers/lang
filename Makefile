COMP = ocamlopt
CARG = -c -w +a-4-6-7-9-27-29-32..42-44-45-48-50-60 -cc clang -ccopt -Ofast
LARG = unix.cmxa -cc clang -ccopt -Ofast -o
PROD = main
OBJS = lex.cmx parse.cmx interpret.cmx main.cmx

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
