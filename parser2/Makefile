TARGET = calc

mechecker.cgi:
	ocamlopt mechecker.ml calc.ml lexer.ml main.ml

calc: lexer.cmo calc.cmo main.cmo mechecker.cmo
	ocamlc -o $@ mechecker.cmo lexer.cmo calc.cmo main.cmo 

depend:
	ocamldep *.ml *.mli > .depend

clean:
	rm -rf *.cmi *.cmo $(TARGET)
	rm -rf calc.ml calc.mli lexer.ml lexer.mli

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi

.ml.cmo:
	ocamlc -c $<
.mli.cmi:
	ocamlc -c $<
.mll.ml:
	ocamllex $<
.mly.ml:
	ocamlyacc $<
.mly.mli:
	ocamlyacc $<

include .depend
