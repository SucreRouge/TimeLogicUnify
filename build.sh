#!/bin/bash
set -e
hash make ocamlopt rlwrap javac js_of_ocaml||
	apt-get install make ocaml-nox rlwrap openjdk-7-jdk js-of-ocaml libfindlib-ocaml libfindlib-ocaml-dev git-core

cd parser3
./install_tableaux.sh
make main unify 

