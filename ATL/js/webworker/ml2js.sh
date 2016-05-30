#!/bin/bash
# Orignally based on:
# http://toss.sourceforge.net/ocaml.html
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [ -z "$1" ]
then echo usage "$0 sample_cli"; exit 1
fi

SAMPLE_INPUT=SAMPLE_INPUT
if [ ! -z "$2" ]
then SAMPLE_INPUT="$2"
fi


if ! command -v js_of_ocaml
then
	echo run sudo apt-get install js-of-ocaml
	exit 1
fi

cat $DIR/JsHeader.ml "$1".ml $DIR/JsFooter.ml > "$1"_merged.ml

ocamlbuild -use-menhir -menhir "menhir" \
  -pp "camlp4o -I /opt/local/lib/ocaml/site-lib js_of_ocaml/pa_js.cmo" \
  -cflags -I,+js_of_ocaml,-I,+site-lib/js_of_ocaml -libs js_of_ocaml \
  -lflags -I,+js_of_ocaml,-I,+site-lib/js_of_ocaml "$1"_merged.byte
js_of_ocaml "$1"_merged.byte

mv "$1"_merged.js "$1".js

#Can minify with something like;
# nodejs node_modules/minify/bin/minify.js JsClient.bak.js | sed s/caml_raise_with_/crw/g | sed s/caml_/K/g > JsClient.js

< $DIR/index.html sed s/sample_cli/"$1"/g | 
	while read -r line
        do printf "%s\n" "${line/SAMPLE_INPUT/$SAMPLE_INPUT}"
	done > "$1"-plain.html
< $DIR/wrapper.html sed s/sample_cli/"$1"/g |
	perl -e 'while(<>){if(/\#include (.*)/){system("cat $1")}else{print}}' > "$1".html

firefox "$1".html
