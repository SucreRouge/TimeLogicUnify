f()
{ if grep is.sat work/out/$1.$SOLVER$SEC work/out/$2.$SOLVER$SEC work/out/$3.$SOLVER$SEC && grep -i is.unsat work/out/$1.$SOLVER$SEC work/out/$2.$SOLVER$SEC work/out/$3.$SOLVER$SEC
  then 
	echo PROBLEM: $1 $2 $3
	sleep 1
	#exit
  fi
    
}

for SOLVER in BCTLOLD CTL mlsolver BCTLNEW BPATH  BPATHUE BCTLHUE
do
for SEC in 3
do
	export SOLVER
	export SEC
	paste <(cat results/bak/triv_3_1000_simp.log  | grep ID | sed s/ID:.//) <(cat results/bak/triv_3_1000_orig.log  | grep ID | sed s/ID:.//) <(cat results/bak/triv_3_1000_both.log  | grep ID | sed s/ID:.//) | while read f; do f $f; done
done
done | tee results/simp_correct.log

echo --- BEGIN PROBLEMS ---
grep PROBLEM results/simp_correct.log
grep RULE.Fail results/bak/*.log
echo --- END PROBLEMS ---
