set x
# triv_3_1000_orig.log 
main () {
run=$1
sec=$2
count=$3

for solver in null BCTLOLD CTL mlsolver BCTLNEW BPATH  BPATHUE BCTLHUE
do
	#for some reason I am sometimes getting EACCESS when attempting to kill a process, causing a abort in unify
	# So just rerun unify multiple time and hope we get a bit further each time, manually cleaning up each time.
	#for type in simp 
        echo "$solver" | tr "\n" "\t"
	for type in orig simp both
        #for type in orig
do
	   if [ $solver = null ] 
	   then
		grep "^[01]$" unify_misc/${type}_formulas.txt.tmp | wc -l | tr "\n" "\t"
	   else
	   	grep -i satisfiable results/triv_${sec}_${count}_${type}.log  | grep -v '(3' | grep " $solver(" | wc -l | tr "\n" "\t"
           fi 
done
echo
done | tee results/triv.out

(
cat <<EOF
%created by 'make result' and 'unify_triv_solv2.sh'
\begin{tabular}{|l|r|r|r|}
\hline
Solver & Original & Simplified & CiME+ \\\\
\hline
EOF
sort -k3 < results/triv.out | ./csv2tex.pl  #> results/unify_len.out 
cat <<EOF
\hline
\end{tabular}
EOF
) | tee results/unify_triv$run.tex
cp results/unify_triv$run.tex ~/uni/PhD || true
}

main "" 3  1000
main 2  60 1000

