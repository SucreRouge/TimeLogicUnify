:x
:q

main: me.ml  phi_lexer.ml phi_parser.ml  me_parser.ml me_parser.mli me_lexer.ml main.ml me_parser.cmo me_parser.cmo
	ocamlc.opt -o main  me.ml phi_parser.ml phi_lexer.ml  me_lexer.ml me_parser.ml main.ml

install: mechecker.cgi
	cp mechecker.cgi /usr/lib/cgi-bin
	bash -c "cat mechecker1.html mechecker2.html > /var/www/mechecker.html"
	bash -c "(echo '<html>' ; cat mechecker2.html) > /var/www/mechecker_plain.html"


mechecker.cgi_: me.cmo me.cmx mechecker.cgi

mechecker.cgi: me.ml  phi_lexer.ml phi_parser.ml  me_parser.ml me_parser.mli me_lexer.ml main.ml phi_parser.cmi me_parser.cmi
	ocamlopt.opt -g -o mechecker.cgi  me.ml phi_parser.ml phi_lexer.ml  me_lexer.ml me_parser.ml main.ml

run: main
	rlwrap ./main

depend:
	ocamldep *.ml *.mli > .depend

clean:
	rm -rf *.cmi *.cmo $(TARGET)
	rm -rf calc.ml calc.mli lexer.ml lexer.mli

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi

.ml.cmo:
	ocamlc.opt -c $<
.mli.cmi: 
	ocamlc.opt -c $<
.mll.ml:
	ocamllex.opt $<
.mly.ml:
	ocamlyacc -v $<
.mly.mli:
	ocamlyacc -v $<

include .depend
