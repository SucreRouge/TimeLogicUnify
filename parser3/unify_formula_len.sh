set -e
#nums=1 2 5 10 20
#for f in $nums
#do
#	make_random_ctls_formulas.ml 50 $i 1000 2999 > results/rand.$f.N.txt
#	sed s/^/,/ < 
touch null
for S in  . S # "S" -> Standard Simplify "." -> both standard and CiME
do
for f in null urules_auto.txt              urules_manual.txt            urules_manual_then_auto.txt
do
	cp $f /var/www/urules.txt 2> /dev/null || cp $f work/urules.txt
	echo -e -n "$f$S\t" | sed 's/urules_//
	s/.txt//
	s/manual_then_auto/both/'
	for i in 1 2 5 10 20
	do
                len=`ocaml make_random_ctls_formulas.ml 50 $i 1000 2999 | sed s/^/$S/ | tee results/len_$f$S$i.txt | time ./unify 2>> tmp.time |tee results/len_$f$S$i.out | grep LEN | cut -f 3 | paste -sd+ | bc`
		echo -e -n "$len\t"
	done
	echo	
done
done | tee results/unify_len.out

#cat len_urules_manual_then_auto.txtS1.out | grep -v subst | grep 'd.to: [10]$' -B8 | grep Input | sort > b 
#comm -23
