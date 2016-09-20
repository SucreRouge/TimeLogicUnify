[ -e main.ml ] || ln -s ../mainlib.ml ../me_lexer.mll ../me_parser.mly ../phi_lexer.mll ../phi_parser.mly ../main.ml .
grep -v html < ../mechecker_header.html > main.text.html
SAMPLE_INPUT="U(p,-q) : [{p};{p,q};{}]+<{p}" ../../ATL/js/webworker/ml2js.sh main "U(p,-q) : [{p};{p,q};{}]+<{p}"
