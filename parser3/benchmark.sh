for i in `seq 20` 
do
	ocaml make_formulas.ml $i | /usr/bin/time ./main 2> output/$i.err > output/$i.out
done
