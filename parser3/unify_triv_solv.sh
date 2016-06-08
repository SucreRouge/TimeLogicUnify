cp urules_manual_then_auto.txt work/urules.txt
[ -s unify_misc/orig_formulas.txt ] || ocaml ./make_random_ctls_formulas.ml 50 1 1000 2999 > unify_misc/orig_formulas.txt
simplify () {
[ -s unify_misc/"$1"_formulas.txt ] || ( < unify_misc/orig_formulas.txt sed s/^/"$2 "/ | make runu | grep Simplified.to: | sed 's/^Simplified to: //' > unify_misc/"$1"_formulas.txt )
}

simplify simp  S
simplify trs   ,
simplify trs2  .
simplify wald  @Sw 
simplify wald2 @SW 
simplify all   @Sa
( cd unify_misc &&
   paste all_formulas.txt  both_formulas.txt  orig_formulas.txt simp_formulas.txt  trs2_formulas.txt  trs_formulas.txt wald2_formulas.txt  wald_formulas.txt | 
   perl -ne 'chomp;$min=9999999;$f="ERROR";foreach $x (split /\t/, $_) {$nobrace=$x;$nobrace=~s/[()]*//g;$len=length $nobrace;if ($len < $min) {$min=$len;$f=$x;}} print "$f\n"' > best_formulas.txt
)

	
#[ -s unify_misc/simp_formulas.txt ] || ( < unify_misc/orig_formulas.txt sed s/^/S/ | make runu | grep Simplified.to: | sed 's/^Simplified to: //' > unify_misc/simp_formulas.txt )
#[ -s unify_misc/trs_formulas.txt  ] || ( < unify_misc/orig_formulas.txt sed s/^/,/ | make runu | grep Simplified.to: | sed 's/^Simplified to: //' > unify_misc/trs_formulas.txt )
#[ -s unify_misc/both_formulas.txt ] || ( < unify_misc/orig_formulas.txt sed s/^/./ | make runu | grep Simplified.to: | sed 's/^Simplified to: //' > unify_misc/both_formulas.txt )
#[ -s unify_misc/wald_formulas.txt ] || ( < unify_misc/orig_formulas.txt sed s/^/@w\ / | make runu | grep Simplified.to: | sed 's/^Simplified to: //' > unify_misc/both_formulas.txt )
#[ -s unify_misc/wsim_formulas.txt ] || ( < unify_misc/orig_formulas.txt sed s/^/@W\ / | make runu | grep Simplified.to: | sed 's/^Simplified to: //' > unify_misc/both_formulas.txt )
#[ -s unify_misc/all_formulas.txt  ] || ( < unify_misc/orig_formulas.txt sed s/^/@a\ / | make runu | grep Simplified.to: | sed 's/^Simplified to: //' > unify_misc/both_formulas.txt )
exit

mkdir -p work/out

triv_solv () {
export UNIFY_TIMEOUT=$1
NUM_FORMULAS=$2
for i in `seq 1 1`
do
	#for some reason I am sometimes getting EACCESS when attempting to kill a process, causing a abort in unify
	# So just rerun unify multiple time and hope we get a bit further each time, manually cleaning up each time.
	#for type in simp 
	for type in simp orig trs2 wald wald2 all
        #for type in orig
do
	if [ -e results/triv_${UNIFY_TIMEOUT}_${NUM_FORMULAS}_${type}.log ]
	then
		continue
	fi
	echo -- $type $i `date +%F.%s` | tee -a results/triv_run.log
	(killall java
	killall mlsolver
	sleep 1
	killall -9 java
	killall -9 mlsolver) 2> /dev/null

	F=unify_misc/${type}_formulas.txt
	head -n $NUM_FORMULAS < unify_misc/${type}_formulas.txt | sed 's/^/<\n/' > $F.tmp 
       	UNIFY_SOLVERS="*" UNIFY_DO_NEG="N" UNIFY_CPUS=1 ./unify < $F.tmp > results/triv_${UNIFY_TIMEOUT}_${NUM_FORMULAS}_${type}.log
       	#UNIFY_SOLVERS="*" UNIFY_DO_NEG="N" UNIFY_CPUS=2 ./unify.exe < unify_misc/${type}_formulas.txt > results/triv_${type}.log
done
done
}

# It seems that if we let the tableau run for 60 seconds, unify might crash.
# Perhaps an OOM killer thing?  

for i in `seq 1 3`
do
triv_solv 3 1000
triv_solv 60 100
triv_solv 60 1000
echo finished run $i, rerunning incase unify crashed | tee -a results/triv_run.log
done
