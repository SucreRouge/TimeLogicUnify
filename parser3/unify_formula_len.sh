set -e
for f in urules_auto.txt              urules_manual.txt            urules_manual_then_auto.txt
do
	cp $f /var/www/urules.txt
	echo -e -n "$f\t" | sed 's/urules_//
	s/.txt//
	s/manual_then_auto/both/'
	for i in 1 2 5 10 20
	do
                len=`./make_random_ctls_formulas.ml 50 $i 1000 2999 | sed s/^/S/ | time ./unify 2>> tmp.time | grep LEN | cut -f 3 | paste -sd+ | bc`
		echo -e -n "$len\t"
	done
	echo	
done | tee results/unify_len.out
