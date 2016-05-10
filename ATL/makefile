all:
	ocamlopt atl.ml -o atl
	#ocamlc atl.ml -o atl
	#ocamlc -c atl.ml -o atl
atl.pdf: atl.ml
	sed 's/\t/    /' < atl.ml |fmt -s > tmp/atl.ml  ; yes '' | ocamlweb tmp/atl.ml --ps > tmp/atl.ps
	ps2pdf tmp/atl.ps
fuzztest.tmp: fuzztest.sh
	bash fuzztest.sh
