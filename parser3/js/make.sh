[ -e mechecker.ml ] || ( 
	ln -s ../mainlib.ml ../me_lexer.mll ../me_parser.mly ../phi_lexer.mll ../phi_parser.mly .
	ln -s ../main.ml mechecker.ml
)
grep -v html < ../mechecker_header.html > mechecker.text.html
../../ATL/js/webworker/ml2js.sh mechecker "U(p,-q) : [{p};{p,q};{}]+<{p,q}" "-libs str"
