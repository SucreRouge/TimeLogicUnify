# Cut down from:
# http://toss.sourceforge.net/ocaml.html
ocamlbuild -use-menhir -menhir "menhir" \
  -pp "camlp4o -I /opt/local/lib/ocaml/site-lib js_of_ocaml/pa_js.cmo" \
  -cflags -I,+js_of_ocaml,-I,+site-lib/js_of_ocaml -libs js_of_ocaml \
  -lflags -I,+js_of_ocaml,-I,+site-lib/js_of_ocaml JsClient.byte
js_of_ocaml JsClient.byte

#Can minify with something like;
# nodejs node_modules/minify/bin/minify.js JsClient.bak.js | sed s/caml_raise_with_/crw/g | sed s/caml_/K/g > JsClient.js
