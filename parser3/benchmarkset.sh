do_one () {
	ocaml make_random_formulas_$1.ml $2 $3 $4 | /usr/bin/time ./main 2> output/set_$1_$2_$3_$4.err > output/set_$1_$2_$3_$4.out

}

for i in `seq 16`
do
	e=$((2**$i))
	do_one 6s $e $e 99
done

exit
for i in 000 010 020 050  
do
	do_one 6s $i 10000 1	
done

for i in `seq 10`
do
	do_one 6s $i"00" 10000 1	
done

exit
for i in 10 100 1000 10000 100000 1000000
do
	do_one 6s 100 $i 0	
done
