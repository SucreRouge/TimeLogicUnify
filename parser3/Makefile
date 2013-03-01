OCAMLC=ocamlopt

main: precheck me.ml mainlib.ml phi_lexer.ml phi_parser.ml  me_parser.ml me_parser.mli me_lexer.ml main.ml phi_parser.cmi me_parser.cmi me_parser.cmo me_parser.cmo
	$(OCAMLC) -o main me.ml phi_parser.ml phi_lexer.ml  me_lexer.ml me_parser.ml mainlib.ml main.ml  

unify: precheck me.ml do_parallel.ml mainlib.ml unify.ml phi_lexer.ml phi_parser.ml  me_parser.ml me_parser.mli me_lexer.ml main.ml phi_parser.cmi me_parser.cmi me_parser.cmo me_parser.cmo
	$(OCAMLC) -o unify me.ml unix.cmxa do_parallel.ml phi_parser.ml phi_lexer.ml  me_lexer.ml me_parser.ml mainlib.ml str.cmxa unify.ml  

www_: 
	mkdir -p www
	bash -c "(echo '<html>' ; cat mechecker2.html) > www/mechecker_plain.html"
	bash -c "cat mechecker1.html mechecker2.html > www/mechecker.html"
	(git archive --prefix=mechecker/ HEAD -o www/mechecker_src.tar && gzip -f -9 www/mechecker_src.tar) || cp ../mechecker_src.tar.gz www

precheck:
	which $(OCAMLC) || (echo "The $(OCAMLC) compiler was not found.\nUsing 'make OCAMLC=ocamlc' in place of 'make' may help (but will cause slower bytecode to be used)" && false) 


# Install as cgi-bin, to a linux machine with e.g. apache installed
install: www_ mechecker.cgi
	cp mechecker.cgi /usr/lib/cgi-bin
	#bash -c "(echo '<html>' ; cat mechecker2.html) > /var/www/mechecker_plain.html"
	#bash -c "cat mechecker1.html mechecker2.html > /var/www/mechecker.html"
	cp www/* /var/www

# Install to a non-root web-directory where cgi-bin is not allowed (link back to machine where cgi-bin is installed)
install_home: www_
	cp www/* ~/WWW/papers/
	sed  -i "s/cgi-bin/http:\/\/gmatht.homelinux.net\/cgi-bin/"   ~/WWW/papers/mechecker_plain.html ~/WWW/papers/mechecker.html


mechecker.cgi_: me.cmo me.cmx mechecker.cgi

mechecker.cgi: precheck me.ml  phi_lexer.ml phi_parser.ml  me_parser.ml me_parser.mli me_lexer.ml main.ml phi_parser.cmi me_parser.cmi
	$(OCAMLC) -o mechecker.cgi me.ml phi_parser.ml phi_lexer.ml  me_lexer.ml me_parser.ml mainlib.ml main.ml  
	#ocamlopt.opt -g -o mechecker.cgi  mainlib.ml me.ml phi_parser.ml phi_lexer.ml  me_lexer.ml me_parser.ml main.ml

run: main
	rlwrap ./main

depend:
	ocamldep *.ml *.mli > .depend

clean:
	rm -rf *.cmi *.cmo *.cmx $(TARGET)
	rm -rf calc.ml calc.mli lexer.ml lexer.mli

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi

.ml.cmo:
	ocamlc -c $<
	#ocamlc.opt -c $<
.mli.cmi: 
	ocamlc -c $<
	#ocamlc.opt -c $<
.mll.ml:
	ocamllex $<
	#ocamllex.opt $<
.mly.ml:
	ocamlyacc -v $<
.mly.mli:
	ocamlyacc -v $<

include .depend
