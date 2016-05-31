all: batl bctl

batl:
	ocamlopt batl.ml -o batl
	#ocamlc atl.ml -o atl
	#ocamlc -c atl.ml -o atl
atl.pdf: atl.ml
	sed 's/\t/    /' < atl.ml |fmt -s > tmp/atl.ml  ; yes '' | ocamlweb tmp/atl.ml --ps > tmp/atl.ps
	ps2pdf tmp/atl.ps
fuzztest.tmp: fuzztest.sh
	bash fuzztest.sh
bctl_memo.ml: bctl.ml bits/memoize.pl
	perl bits/memoize.pl bctl.ml > bctl_memo.ml
bctl: bctl_memo.ml bctl.ml
	ocamlopt bctl_memo.ml -o bctl

