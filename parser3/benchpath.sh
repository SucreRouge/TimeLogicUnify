formulas=mark_formulas.txt

grep 'eg\[' var/mark/formulas/Examples.java  | grep '"' | sed 's/";//' | sed 's/.*"//' > $formulas 


if [ 1 -gt "0$1" ]
then
(echo '<'; cat  $formulas) | UNIFY_OFFLINE=y UNIFY_SOLVERS="BPATH BCTLNEW BCTLHUE BPATHf BPATHUE BPATHUEf" make runu
#(echo '<'; cat  $formulas) | UNIFY_OFFLINE=y UNIFY_SOLVERS="BCTLNEW" make runu
true && (echo '<'; cat  $formulas | sed 's/p/Ap/g
s/q/Aq/g
s/r/Ar/g' ) | UNIFY_OFFLINE=y UNIFY_SOLVERS="BPATH BPATHUE" make runu
fi

echo -n '' > var/out/benchmark.tex
echo -n '' > var/out/benchmarkhue.tex
(echo '<'; 
  while read -r L
  do
	  echo "B$L"
	  echo "B-($L)"
  done
  cat
) < $formulas | make runu

(
LOGICS='\mathcal{N} \mathcal{N}^{AE} \mathcal{N}^A \mathcal{L}'
FIELDS="Satisfiable Time   Colours Hues"
FTYPES="c           r@{.}l r       r   "

echo '\begin{tabular}{|r||c|c|c||c||r@.l|r@{.}l|r@{.}l|r@{.}l||r|r|r|r||r|r|r|r|} \hline
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

for e in "" hue
do
sed 's/[.]/\&/g
s/[$].neg\(................................*\)vee\(...................................*\)[$]/$\\neg ( \\ldots )$/
s/[$]\(................................*\\vee\)\(...................................*\)[$]/\\pbox{10cm}{$\1 \\ldots $\\\\    $\\quad\2 $}/' <  var/out/benchmark$e.tex > benchpath_table$e.tex
#sed 's/[.]/\&/g' <  var/out/benchmarkhue.tex > benchpathhue_table.tex
  #| UNIFY_OFFLINE=y UNIFY_SOLVERS="BPATH BCTLNEW BCTLHUE BPATHf BPATHUE BPATHUEf" make runu
done

sed 's/table.tex/tablehue.tex/' < Table.tex > TableHue.tex

pdflatex Table.tex
pdflatex TableHue.tex

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

