OBJS = ast.cmo parser.cmo scanner.cmo sast.cmo compile.cmo senet.cmo
VERBOSE_YACC = -v

# Choose one
# YACC = ocamlyacc
YACC = menhir --explain

senet : $(OBJS)
	ocamlc -o senet $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	$(YACC) $(VERBOSE_YACC) parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

.PHONY : clean
clean :
	@rm -f senet parser.ml parser.mli scanner.ml \
	*.cmo *.cmi *.out *.diff *.output \
	output.c testall.log


parser.cmo: ast.cmo parser.cmi

