formulas=mark_formulas.txt

#mkdir -p var/out

grep 'eg\[' work/mark/src/formulas/Examples.java  | grep '"' | sed 's/";//' | sed 's/.*"//' > $formulas 


if [ 1 -gt "0$1" ]
then
(echo '<'; cat  $formulas) | UNIFY_OFFLINE=y UNIFY_SOLVERS="nl_bctl bctl nl_bctlf mlsolver BPATH BCTLNEW BCTLHUE BPATHf BPATHUE BPATHUEf" make runu
#(echo '<'; cat  $formulas) | UNIFY_OFFLINE=y UNIFY_SOLVERS="BCTLNEW" make runu
true && (echo '<'; cat  $formulas | sed 's/p/Ap/g
s/q/Aq/g
s/r/Ar/g' ) | UNIFY_OFFLINE=y UNIFY_SOLVERS="BPATH BPATHUE nl_bctl" make runu | tee benchpath.log
fi

#exit

echo -n '' > work/out/benchmark.tex
echo -n '' > work/out/benchmarkhue.tex
echo -n '' > work/out/benchmark_simple.tex
(echo '<'; 
  while read -r L
  do
	  echo "B$L"
	  echo "B-($L)"
  done
  cat
) < $formulas | make runu

#exit

cp work/out/benchmark*.tex .

(
LOGICS='\mathcal{N} \mathcal{N}^{AE} \mathcal{N}^A \mathcal{L}'
FIELDS="Satisfiable Time   Colours Hues"
FTYPES="c           r@{.}l r       r   "

echo '%Made by benchpath.sh and unify.ml (the unified theorem prover)
\begin{tabular}{|r||c|c|c||c||r@.l|r@{.}l|r@{.}l|r@{.}l||r|r|r|r||r|r|r|r|} \hline
Formula &
\multicolumn{4}{c|}{Sat} &
\multicolumn{8}{c|}{CPU Time} &
\multicolumn{4}{c|}{Colours} &
\multicolumn{4}{c|}{Hues} \\ 
\hline
'

LOGICS_TEX=`
	for l in $LOGICS 
	do
		echo -n "& \$ $l\$ "
	done`

LOGICS_TEX2C=`
	for l in $LOGICS 
	do
		echo -n "& \multicolumn{2}{c|}{\$ $l \$}"
	done`

echo "$LOGICS_TEX $LOGICS_TEX2C $LOGICS_TEX $LOGICS_TEX \\\\ \\hline"
) > table_header.tex

#AEFGNUXY |from| grep '^[$]' <  benchpath_table_simple.tex | grep '[[:upper:]]' -o | sort -u | tr -d '\n'


for e in "" hue _simple
do 
(echo '%Made by benchpath.sh and unify.ml (the unified theorem prover)'
echo -n '%'
cat /proc/cpuinfo  | grep model.name | head -n1
sed 's/[.]/\&/g
s/[$].neg\(................................*\)vee\(...................................*\)[$]/$\\neg ( \\ldots )$/
s/[$]\(................................*\\vee\)\(...................................*\)[$]/\\pbox{10cm}{$\1 \\ldots $\\\\    $\\quad\2 $}/
s/\([AEFGUX]\)/\\\1/g') <  work/out/benchmark$e.tex > benchpath_table$e.tex
#sed 's/[.]/\&/g' <  var/out/benchmarkhue.tex > benchpathhue_table.tex
  #| UNIFY_OFFLINE=y UNIFY_SOLVERS="BPATH BCTLNEW BCTLHUE BPATHf BPATHUE BPATHUEf" make runu
done

exit
sed 's/table.tex/tablehue.tex/' < Table.tex > TableHue.tex
sed 's/table.tex/table_simple.tex/' < Table.tex > Table_simple.tex

pdflatex Table.tex    < /dev/null
pdflatex TableHue.tex < /dev/null
pdflatex Table_simple.tex < /dev/null

exit

cat var/out/benchmark.log
cat var/out/benchmark.log

echo '\begin{tabular}{|r|'
false && (for t in $FTYPES do
	echo -n '|'
	for l in $LOGICS do
		echo -n "$FTYPES|"
	done
done
echo -n '}'
echo)

