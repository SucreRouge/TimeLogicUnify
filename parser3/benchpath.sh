

if [ 1 -gt "0$1"]
then
(echo '<'; cat  ../../4mlsolver/formulas.txt) | UNIFY_OFFLINE=y UNIFY_SOLVERS="BPATH BCTLNEW BCTLHUE BPATHf BPATHUE BPATHUEf" make runu
#(echo '<'; cat  ../../4mlsolver/formulas.txt) | UNIFY_OFFLINE=y UNIFY_SOLVERS="BCTLNEW" make runu
true && (echo '<'; cat  ../../4mlsolver/formulas.txt | sed 's/p/Ap/g
s/q/Aq/g
s/r/Ar/g' ) | UNIFY_OFFLINE=y UNIFY_SOLVERS="BPATH BPATHUE" make runu
fi

echo -n '' > var/out/benchmark.log
echo -n '' > var/out/benchmarkhue.log
(echo '<'; 
  while read -r L
  do
	  echo "B$L"
	  echo "B-($L)"
  done
  cat
) < ../../4mlsolver/formulas.txt | make runu


echo '\begin{tabular}{|r|'
LOGICS='\mathcal{N} \mathcal{N}^{AE} \mathcal{N}^A \mathcal{L}'
FIELDS="Satisfiable Time   Colours Hues"
FTYPES="c           r@{.}l r       r   "

for t in $FTYPES do
	echo -n '|'
	for l in $LOGICS do
		echo -n "$FTYPES|"
	done
done



|c|c|c||c||r@.l|r@{.}l|r@{.}l|r@{.}l|||r|r|r|r||r|r|r|} \hline
sed 's/^/-(//' < ../../4mlsolver/formulas.txt;
cat var/out/benchmark.log
  #| UNIFY_OFFLINE=y UNIFY_SOLVERS="BPATH BCTLNEW BCTLHUE BPATHf BPATHUE BPATHUEf" make runu
