for solver in null BCTLOLD CTL mlsolver BCTLNEW BPATH  BPATHUE BCTLHUE
do
	#for some reason I am sometimes getting EACCESS when attempting to kill a process, causing a abort in unify
	# So just rerun unify multiple time and hope we get a bit further each time, manually cleaning up each time.
	#for type in simp 
        echo "$solver" | tr "\n" "\t"
	for type in orig simp 
        #for type in orig
do
	   if [ $solver = null ] 
	   then
		grep "^[01]$" unify_misc/${type}_formulas.txt.tmp | wc -l | tr "\n" "\t"
	   else
	   	grep -i satisfiable results/triv_${type}.log  | grep -v '(3' | grep " $solver(" | wc -l | tr "\n" "\t"
           fi 
done
echo
done | tee results/triv.out

(
cat <<EOF
\begin{tabular}{|l|r|r|}
\hline
Solver & Original & Simplified \\\\
\hline
EOF
sort -k3 < results/triv.out | csv2tex.pl  #> results/unify_len.out 
cat <<EOF
\hline
\end{tabular}
EOF
) | tee results/unify_triv.tex
cp results/unify_triv.tex ~/uni/PhD || true

