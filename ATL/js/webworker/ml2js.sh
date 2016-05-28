# Cut down from:
# http://toss.sourceforge.net/ocaml.html

if [ -z "$1" ]
then echo usage "$0 sample_cli"; exit 1
fi

if ! command -v js_of_ocaml
then
	echo run sudo apt-get install js-of-ocaml
	exit 1
fi

cat JsHeader.ml "$1".ml JsFooter.ml > "$1"_merged.ml

ocamlbuild -use-menhir -menhir "menhir" \
  -pp "camlp4o -I /opt/local/lib/ocaml/site-lib js_of_ocaml/pa_js.cmo" \
  -cflags -I,+js_of_ocaml,-I,+site-lib/js_of_ocaml -libs js_of_ocaml \
  -lflags -I,+js_of_ocaml,-I,+site-lib/js_of_ocaml "$1"_merged.byte
js_of_ocaml "$1"_merged.byte

mv "$1"_merged.js "$1".js

#Can minify with something like;
# nodejs node_modules/minify/bin/minify.js JsClient.bak.js | sed s/caml_raise_with_/crw/g | sed s/caml_/K/g > JsClient.js

< index.html sed s/sample_cli/"$1"/g > "$1".html

firefox "$1".html

