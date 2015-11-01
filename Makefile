OBJS = ast.cmo parser.cmo scanner.cmo senet.cmo

# Choose one
YACC = ocamlyacc
# YACC = menhir --explain

senet : $(OBJS)
	ocamlc -o senet $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	$(YACC) parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

.PHONY : clean
clean :
	rm -f senet parser.ml parser.mli scanner.ml \
	*.cmo *.cmi *.out *.diff


parser.cmo: ast.cmo parser.cmi
