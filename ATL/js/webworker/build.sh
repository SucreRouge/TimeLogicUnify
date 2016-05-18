# Cut down from:
# http://toss.sourceforge.net/ocaml.html
ocamlbuild -use-menhir -menhir "menhir --external-tokens Lexer" \
  -pp "camlp4o -I /opt/local/lib/ocaml/site-lib js_of_ocaml/pa_js.cmo" \
  -cflags -I,+js_of_ocaml,-I,+site-lib/js_of_ocaml -libs js_of_ocaml \
  -lflags -I,+js_of_ocaml,-I,+site-lib/js_of_ocaml JsClient.byte
js_of_ocaml JsClient.byte
