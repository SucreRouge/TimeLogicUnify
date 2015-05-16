all:
	ocamlc atl.ml -o atl
	#ocamlc -c atl.ml -o atl
atl.ps:
	sed 's/\t/    /' < ../atl.ml |fmt -s > atl.ml  ; yes '' | ocamlweb atl.ml --ps > atl.ps
