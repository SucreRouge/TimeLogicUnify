
main: me.ml  phi_lexer.ml phi_parser.ml  me_parser.ml me_parser.mli me_lexer.ml main.ml phi_parser.cmi me_parser.cmi me_parser.cmo me_parser.cmo
	ocamlc.opt -o main  me.ml phi_parser.ml phi_lexer.ml  me_lexer.ml me_parser.ml main.ml

www: 
	bash -c "(echo '<html>' ; cat mechecker2.html) > www/mechecker_plain.html
	bash -c "cat mechecker1.html mechecker2.html > www/mechecker.html
	git archive --prefix=mechecker HEAD -o www/mechecker_src.tar.gz

install: www mechecker.cgi
	cp mechecker.cgi /usr/lib/cgi-bin
	#bash -c "(echo '<html>' ; cat mechecker2.html) > /var/www/mechecker_plain.html"
	#bash -c "cat mechecker1.html mechecker2.html > /var/www/mechecker.html"
	cp www/* /var/www

install_home: www
	cp www/* ~/WWW/papers/
	sed -i ~/WWW/papers/mechecker.html "s/cgi-bin/http:\/\/gmatht.homelinux.net\/cgi-bin"
	sed -i ~/WWW/papers/mechecker_plain.html "s/cgi-bin/http:\/\/gmatht.homelinux.net\/cgi-bin"

mechecker.cgi_: me.cmo me.cmx mechecker.cgi

mechecker.cgi: me.ml  phi_lexer.ml phi_parser.ml  me_parser.ml me_parser.mli me_lexer.ml main.ml phi_parser.cmi me_parser.cmi
	ocamlopt.opt -g -o mechecker.cgi  me.ml phi_parser.ml phi_lexer.ml  me_lexer.ml me_parser.ml main.ml

run: main
	rlwrap ./main

depend:
	ocamldep *.ml *.mli > .depend

clean:
	rm -rf *.cmi *.cmo *.cmx $(TARGET)
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
