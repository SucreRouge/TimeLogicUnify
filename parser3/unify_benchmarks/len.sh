set -e
#set -x

do_len() {
simp_type=$1
suffix=$2
mkdir -p results
for f in urules_auto.txt              urules_manual.txt            urules_manual_then_auto.txt
do
	cp $f /var/www/urules.txt || cp $f work/urules.txt 
	/bin/echo -e -n "$f\t" | sed 's/urules_//
	s/.txt//
	s/manual_then_auto/both/'
	for i in 1 2 5 10 20
	do
                len=`ocaml make_random_ctls_formulas.ml 50 $i 1000 2999 | sed s/^/$simp_type/ | make runu | grep LEN | cut -f 3 | paste -sd+ | bc`
		/bin/echo -e -n "$len\t"
	done
	echo	
done | tee results/unify_len$suffix.out.tmp
mv results/unify_len$suffix.out.tmp results/unify_len$suffix.out
}
[ -e urules_manual_then_auto.txt ] || cat urules_manual.txt urules_auto.txt > urules_manual_then_auto.txt
[ -e results/unify_len.out ] || do_len S "" 
[ -e results/unify_len_TRS.out ] || do_len , "_TRS" 
[ -e results/unify_len_both.out ] || do_len . "_both" 

if [ ! -e results/unify_01.out ]
then
	cp urules_manual_then_auto.txt work/urules.txt
	n=10000
	(
	echo out of $n single variable formulas
	echo "prove FALSE: (Knuth+Simple) `ocaml make_random_ctls_formulas.ml 50 1 1 $n  | sed s/^/./ | ./unify | grep '^SIMP: 0$'|wc -l`"
	echo "prove FALSE: (Simple) `ocaml make_random_ctls_formulas.ml 50 1 1 $n | sed s/^/S/ | ./unify | grep '^SIMP: 0$'|wc -l`" 
	echo "prove FALSE: (Knuth) `ocaml make_random_ctls_formulas.ml 50 1 1 $n | sed s/^/,/ | ./unify | grep '^SIMP: 0$'|wc -l`" 
	echo "prove TRUE: (Knuth+Simple) `ocaml make_random_ctls_formulas.ml 50 1 1 $n | sed s/^/./ | ./unify | grep '^SIMP: 1$'|wc -l`"
	echo "prove TRUE: (Simple) `ocaml make_random_ctls_formulas.ml 50 1 1 $n | sed s/^/S/ | ./unify | grep '^SIMP: 1$'|wc -l`" 
	echo "prove TRUE: (Knuth) `ocaml make_random_ctls_formulas.ml 50 1 1 $n | sed s/^/,/ | ./unify | grep '^SIMP: .0$'|wc -l`" 
	) |tee results/unify_01.out
fi

echo PRINT_TRS_TEX | make runu | grep rewrite | perl scripts/lines2matrix.pl > results/unify.tex

set -x
head -n 1000 < unify_misc/simp_formulas.txt | grep '^0$' | wc -l
grep 'UNsatisfiable: BCTLOLD(' results/triv_60_100_orig.log | wc -l
grep 'UNsatisfiable: BPATH(' results/triv_60_100_orig.log | wc -l
grep 'UNsatisfiable: BPATHUE(' results/triv_60_100_orig.log | wc -l
grep 'UNsatisfiable: BCTLNEW(' results/triv_60_100_orig.log | wc -l
grep 'UNsatisfiable: BCTLHUE(' results/triv_60_100_orig.log | wc -l
grep 'UNsatisfiable: mlsolver(' results/triv_60_100_orig.log | wc -l
grep 'UNsatisfiable: mlsolver(' results/triv_60_100_simp.log | wc -l
grep 'been' results/triv_60_100_orig.log | wc -l

#[ -e results/unify_len_both.out ] || do_len . "_both" 

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

