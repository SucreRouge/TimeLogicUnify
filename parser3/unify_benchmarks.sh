set -e
#set -x

[ -e urules_manual_then_auto.txt ] || cat urules_manual.txt urules_auto.txt > urules_manual_then_auto.txt
if [ ! -e results/unify_len.out ]
then 
mkdir -p results
for f in urules_auto.txt              urules_manual.txt            urules_manual_then_auto.txt
do
	cp $f /var/www/urules.txt || cp $f work/urules.txt 
	echo -e -n "$f\t" | sed 's/urules_//
	s/.txt//
	s/manual_then_auto/both/'
	for i in 1 2 5 10 20
	do
                len=`ocaml make_random_ctls_formulas.ml 50 $i 1000 1099 | sed s/^/S/ | make runu | grep LEN | cut -f 3 | paste -sd+ | bc`
		echo -e -n "$len\t"
	done
	echo	
done | tee results/unify_len.out.tmp
mv results/unify_len.out.tmp results/unify_len.out
fi


exit
for i in `seq 1 19`
do
	#for some reason I am sometimes getting EACCESS when attempting to kill a process, causing a abort in unify
	# So just rerun unify multiple time and hope we get a bit further each time, manually cleaning up each time.
	#for type in simp 
	##for type in simp orig
        for type in orig simp
do
	echo -- $type $i
	killall java
	killall mlsolver
	sleep 1
	killall -9 java
	killall -9 mlsolver
	mkdir -p unify_misc

	F=unify_misc/${type}_formulas.txt
	head -n 100 < unify_misc/${type}_formulas.txt | sed 's/^/<\n/' > $F.tmp 
       	UNIFY_SOLVERS="*" UNIFY_DO_NEG="N" UNIFY_TIMEOUT=60 UNIFY_CPUS=2 ./unify.exe < $F.tmp > results/triv_${type}.log
       	#UNIFY_SOLVERS="*" UNIFY_DO_NEG="N" UNIFY_CPUS=2 ./unify.exe < unify_misc/${type}_formulas.txt > results/triv_${type}.log
done
done

exit #END!!!
set -e
for f in urules_auto.txt              urules_manual.txt            urules_manual_then_auto.txt
do
	cp $f /var/www/urules.txt
	echo -e -n "$f\t" | sed 's/urules_//
	s/.txt//
	s/manual_then_auto/both/'
	for i in 1 2 5 10 20
	do
                len=`./make_random_ctls_formulas.ml 50 $i 1000 1099 | sed s/^/S/ | make runu | grep LEN | cut -f 3 | paste -sd+ | bc`
		echo -e -n "$len\t"
	done
	echo	
done | tee results/unify_len.out

exit

$ time ./make_random_ctls_formulas.ml 50 1 1000 2999 > unify_misc/input_formulas.txt

real    0m4.327s
user    0m1.761s
sys     0m0.200s
[Fri Apr 05 12:58:42 ~/uni/PhD/code/ocaml/parser3]$ cp urules_manual_then_auto.txt /var/www/urules.txt
[Fri Apr 05 12:59:26 ~/uni/PhD/code/ocaml/parser3]$ sed s/^/S/ < unify_misc/input_formulas.txt | time ./unify.exe | grep Simp | sed s/*.://g > unify_misc/simp_ormulas.t
8.72user 0.09system 0:09.42elapsed 93%CPU (0avgtext+0avgdata 578304maxresident)k

