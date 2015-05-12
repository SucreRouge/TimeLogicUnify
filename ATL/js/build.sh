#ocamlbuild -use-ocamlfind -pkgs js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o index.byte
#js_of_ocaml index.byte
ocamlbuild -use-ocamlfind -pkgs js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o atlweb.byte
js_of_ocaml atlweb.byte
