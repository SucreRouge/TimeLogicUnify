cp urules_manual_then_auto.txt work/urules.txt
[ -s unify_misc/orig_formulas.txt ] || ocaml ./make_random_ctls_formulas.ml 50 1 1000 2999 > unify_misc/orig_formulas.txt
[ -s unify_misc/simp_formulas.txt ] || ( < unify_misc/orig_formulas.txt sed s/^/S/ | make runu | grep Simplified.to: | sed 's/^Simplified to: //' > unify_misc/simp_formulas.txt )

triv_solv () {
export UNIFY_TIMEOUT=$1
NUM_FORMULAS=$2
for i in `seq 1 1`
do
	#for some reason I am sometimes getting EACCESS when attempting to kill a process, causing a abort in unify
	# So just rerun unify multiple time and hope we get a bit further each time, manually cleaning up each time.
	#for type in simp 
	for type in simp orig
        #for type in orig
do
	echo -- $type $i
	(killall java
	killall mlsolver
	sleep 1
	killall -9 java
	killall -9 mlsolver) 2> /dev/null

	F=unify_misc/${type}_formulas.txt
	head -n $NUM_FORMULAS < unify_misc/${type}_formulas.txt | sed 's/^/<\n/' > $F.tmp 
       	UNIFY_SOLVERS="*" UNIFY_DO_NEG="N" UNIFY_CPUS=2 ./unify < $F.tmp > results/triv_${UNIFY_TIMEOUT}_${NUM_FORMULAS}_${type}.log
       	#UNIFY_SOLVERS="*" UNIFY_DO_NEG="N" UNIFY_CPUS=2 ./unify.exe < unify_misc/${type}_formulas.txt > results/triv_${type}.log
done
done
}

triv_solv 60 100
triv_solv 3 1000
