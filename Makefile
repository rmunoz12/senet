OBJS = ast.cmo parser.cmo scanner.cmo types.cmo stdlib.cmo sast.cmo cast.cmo \
			 compile.cmo senet.cmo
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
	*.cmo *.cmi *.out *.diff *.output *.conflicts *.automaton \
	output.c testall.log

# Generated by ocamldep *.ml *.mli *.mly *.mll
# see http://caml.inria.fr/pub/docs/manual-ocaml/depend.html
ast.cmo :
ast.cmx :
cast.cmo : types.cmo sast.cmo ast.cmo
cast.cmx : types.cmx sast.cmx ast.cmx
compile.cmo : types.cmo sast.cmo ast.cmo
compile.cmx : types.cmx sast.cmx ast.cmx
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmx parser.cmi
sast.cmo : types.cmo stdlib.cmo ast.cmo
sast.cmx : types.cmx stdlib.cmx ast.cmx
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx
senet.cmo : scanner.cmo sast.cmo parser.cmi compile.cmo cast.cmo ast.cmo
senet.cmx : scanner.cmx sast.cmx parser.cmx compile.cmx cast.cmx ast.cmx
stdlib.cmo : types.cmo
stdlib.cmx : types.cmx
types.cmo :
types.cmx :
parser.cmi : ast.cmo
