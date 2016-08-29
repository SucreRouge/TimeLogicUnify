OCAMLC = $(shell which ocamlopt.opt ocamlopt | head -n1)

#REMOTE = "http://gmatht.homelinux.net"
#REMOTE = "http://gmatht.mooo.com"
REMOTE = "https://matt.ucc.asn.au/~mccabedj"

all: main unify result

main: precheck me.ml mainlib.ml phi_lexer.ml phi_parser.ml  me_parser.ml me_parser.mli me_lexer.ml main.ml phi_parser.cmi me_parser.cmi me_parser.cmo me_parser.cmo
	$(OCAMLC) -o main str.cmxa me.ml phi_parser.ml phi_lexer.ml  me_lexer.ml me_parser.ml mainlib.ml main.ml  

unify_.ml: unify.ml
	sed s/NUM_CPUS/`< /proc/cpuinfo  grep ^processor | wc -l`/g < unify.ml > unify_.ml

unify: me.ml do_parallel.ml mainlib.ml unify_.ml phi_lexer.ml phi_parser.ml  me_parser.ml me_parser.mli me_lexer.ml main.ml phi_parser.cmi me_parser.cmi me_parser.cmo me_parser.cmo
	$(OCAMLC) -o unify     me.ml str.cmxa unix.cmxa do_parallel.ml phi_parser.ml phi_lexer.ml mainlib.ml unify_.ml  
unify_dbg:  me.ml do_parallel.ml mainlib.ml unify_.ml phi_lexer.ml phi_parser.ml  me_parser.ml me_parser.mli me_lexer.ml main.ml phi_parser.cmi me_parser.cmi me_parser.cmo me_parser.cmo
	ocamlc -g -o unify_dbg me.ml str.cma unix.cma do_parallel.ml phi_parser.ml phi_lexer.ml  me_lexer.ml me_parser.ml mainlib.ml str.cma unify_.ml
	mkdir -p ../.data/unify
	mkdir ../.data/unify/mark
	mkdir ../.data/unify/mark/out
	touch ../urules.txt
	
www_: 
	mkdir -p www
	#bash -c "(echo '<html>' ; cat mechecker2.html) > www/mechecker_plain.html"
	#bash -c "cat mechecker1.html mechecker2.html > www/mechecker.html"
	bash ./makehtml.sh mechecker www/mechecker
	(git archive --prefix=mechecker/ HEAD -o www/mechecker_src.tar && gzip -f -9 www/mechecker_src.tar) || cp ../mechecker_src.tar.gz www

unify_www_: 
	mkdir -p unify_www
	#bash -c "(echo '<html>' ; cat unify2.html) > unify_www/mechecker_plain.html"
	#bash -c "cat unify1.html unify2.html > unify_www/unified.html"
	./makehtml.sh unify unify_www/unified
	mkdir -p www
	(git archive --prefix=mechecker/ HEAD -o www/mechecker_src.tar && gzip -f -9 www/mechecker_src.tar) || cp ../mechecker_src.tar.gz www
	cp ../../../Waldmeister_Output_Rewrite.html unify_www

userdir: www_ unify_www_ unify mechecker.cgi
	mkdir -p ~/public-html
	#cp www/* unify_www/* ~/public-html
	install -m644 www/* unify_www/* ~/public-html
	#cp unify 
	#cp unify 	 ~/public-html/cgi-bin/unify.cgi
	install unify 	 ~/public-html/cgi-bin/unify.cgi
	install mechecker.cgi ~/public-html/cgi-bin/mechecker.cgi
	chmod 1777 ~/public-html/.data/unify/mark/out/
	chmod 1777 ~/public-html/.data/unify/out/
	chmod u+r  ~/public-html/.data/unify/log*
	chmod go-r  ~/public-html/.data/unify/log*
	chmod u+r  ~/public-html/.data/unify/mark/*java
	chmod go-r  ~/public-html/.data/unify/mark/*java
	test -e ~/public-html/.data/unify_stats.txt || echo 0 > ~/public-html/.data/unify_stats.txt
	test -e ~/public-html/.data/mechecker_stats.txt || echo 0 > ~/public-html/.data/mechecker_stats.txt
	touch  ~/public-html/.data/unify_bad.log 
	touch  ~/public-html/.data/unify_good.log 
	chmod a+w ~/public-html/.data/unify_*.log
	chmod a+w  ~/public-html/.data/*_stats.txt

precheck:
	which $(OCAMLC) || (echo "The $(OCAMLC) compiler was not found.\nUsing 'make OCAMLC=ocamlc' in place of 'make' may help (but will cause slower bytecode to be used)" && false) 


# Install as cgi-bin, to a linux machine with e.g. apache installed
install: www_ mechecker.cgi
	cp mechecker.cgi /usr/lib/cgi-bin
	#bash -c "(echo '<html>' ; cat mechecker2.html) > /var/www/mechecker_plain.html"
	#bash -c "cat mechecker1.html mechecker2.html > /var/www/mechecker.html"
	cp www/* /var/www

unify_install: unify_www_ unify
	mkdir -p  /usr/lib/cgi-bin
	mkdir -p  /var/www
	cp unify /usr/lib/cgi-bin/unify.cgi

	#bash -c "(echo '<html>' ; cat mechecker2.html) > /var/www/mechecker_plain.html"
	#bash -c "cat mechecker1.html mechecker2.html > /var/www/mechecker.html"
	(git archive --prefix=mechecker/ HEAD -o www/mechecker_src.tar && gzip -f -9 www/mechecker_src.tar) || cp ../mechecker_src.tar.gz www
	cp unify_www/* /var/www/html
	mkdir -p /var/data/unify
	touch /var/data/unify/log.3
	chmod 666  /var/data/unify/log.3
	rsync -ra --safe-links work/ --exclude work/ATL /var/data/unify
	mkdir -p /var/data/unify/out
	#chmod 777  /var/data/unify/mark_v1/out
	#chmod 777  /var/data/unify/mark/out
	chmod 777  /var/data/unify/out
	rm /var/data/unify/ATL || true
	mkdir -p /var/data/unify/ATL
	cp ../ATL/nl_bctl ../ATL/bctl /var/data/unify/ATL/
	#touch /var/data/unify/out/rules.txt
	touch /var/www/html/urules.txt
	cp work/urules.txt.trs /var/www/html/
	chmod 666 /var/www/html/urules.txt

	mkdir -p /var/www/html/.data/
	test -e /var/www/html/.data/unify_stats.txt || echo 0 > /var/www/html/.data/unify_stats.txt
	test -e /var/www/html/.data/mechecker_stats.txt || echo 0 > /var/www/html/.data/mechecker_stats.txt
	touch  /var/www/html/.data/unify_bad.log 
	touch  /var/www/html/.data/unify_good.log 
	chmod a+w /var/www/html/.data/unify_*.log
	chmod a+w  /var/www/html/.data/*_stats.txt
	chmod a+rx  /var/data/unify/ctlProver
	#mkdir -p /var/data/unify
	#cp -rv mark/ /var/data/unify/mark	
	#cp -rv mlsolver/ /var/data/unify/mlsolver
	chmod a+rwx /var/data/unify/ctl-rp.jail/
	touch /var/www/html/.data/unify_BUG.log	
	#firejail is unreliable on executables outside bin, *sometimes* marking them no-exec. 
	cp -a ctl-rp.jail/ctlrp21_x86_64 /bin/

# Install to a non-root web-directory where cgi-bin is not allowed (link back to machine where cgi-bin is installed)
install_home: www_
	cp www/* ~/WWW/papers/
	sed  -i "s,cgi-bin,$(REMOTE)/cgi-bin," ~/WWW/papers/mechecker_plain.html ~/WWW/papers/mechecker.html

install_homeu: unify_www_
	cp unify_www/* ~/WWW/papers/
	sed  -i "s,cgi-bin\|urules,$(REMOTE)/&,g"  ~/WWW/papers/unified_plain.html ~/WWW/papers/unified.html
	#bash -c "cd ~/WWW/ && ln -s papers/unified*.html papers/urules.txt mechecker_src.tar. ."
	chmod a+r ../../../Waldmeister_Output_Rewrite.html
	cp redirect_unified.html ~/WWW/unified.html

#results/unify_len.out: unify_benchmarks/len.sh
#	unify_benchmarks/len.sh

#benchmarku: results/unify_len.out


mechecker.cgi_: me.cmo me.cmx mechecker.cgi

mechecker.cgi: precheck me.ml  phi_lexer.ml phi_parser.ml  me_parser.ml me_parser.mli me_lexer.ml main.ml phi_parser.cmi me_parser.cmi
	$(OCAMLC) -o mechecker.cgi str.cmxa me.ml phi_parser.ml phi_lexer.ml  me_lexer.ml me_parser.ml mainlib.ml main.ml  
	#ocamlopt.opt -g -o mechecker.cgi  mainlib.ml me.ml phi_parser.ml phi_lexer.ml  me_lexer.ml me_parser.ml main.ml

mvroot:
	mkdir -p root
	mv `ls -l | grep -w root | sed s/.*\ //g` bak
	
run: main
	rlwrap ./main

runu: run_unify

run_unify: unify
	mkdir -p work/out
	rlwrap ./unify

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

rclean:
	tar -zcf rclean_save-`date +%F`.tgz work/mark/out/ work/out/ results/ unify_misc/ TeX && rm work/mark/out/* work/out/* results/* TeX/* unify_misc/*

cimeclean:
	mkdir -p bak
	mv unify_misc/trs_formulas.txt unify_misc/both_formulas.txt bak || true
	mv results/*both* bak || true

result: TeX/unify_len.tex TeX/unify_triv.tex TeX/benchmark.tex TableHue.pdf

results/unify_len.out: ./unify_formula_len.sh
	#./unify_formula_len.sh
	mkdir -p cime/out
	(cd work && [ -e cime ] || ln -s ../cime .)
	unify_benchmarks/len.sh

TeX/unify_len.tex: results/unify_len.out ./unify_formula_len2.sh
	./unify_formula_len2.sh
#Obsolete? Merged with unify_len.tex
#TeX/unify_len_TRS.tex: results/unify_len.out ./unify_formula_len2.sh
#	./unify_formula_len2_TRS.sh

results/triv_3_1000_both.log: ./unify_triv_solv.sh
	./unify_triv_solv.sh

TeX/unify_triv.tex: results/triv_3_1000_both.log 
	./unify_triv_solv2.sh

TeX/benchmark.tex: work/out/benchmark.tex
	cp work/out/*.tex TeX/

TableHue.pdf:
	bash ./benchpath.sh

check:
	echo '-((-Fa)U(((F((((EXAa)|(a)))|(((GFa)|(Xa)))))|(a))))' | UNIFY_SOLVERS="mlsolver CTL" make runu
	echo '-F((Fa)|(((GFa)|(a))))' | UNIFY_SOLVERS="mlsolver CTL" make runu


include .depend
