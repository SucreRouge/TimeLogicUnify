

orig () {
	echo $1
}

pow10 () {
	echo set_6sX_100_$((10**$i))_0
}

pow10f () {
	echo set_6sX_$((10**$i))_100_2
}

squarepow2 () {
	pow2=$((2**$i))
	echo set_6sX_${pow2}_${pow2}_99
}

do_table () {
F=output/table_$1.txt
rm $F || true

(
echo -n "i "
/usr/bin/time true 2>&1 |  tr '\n+' '  '  | sed 's/[().:0-9]//g
s/maxresidentk/max_mb/
s/pagefaults/pf/'
echo origme finalme formula

for i in $2
do
echo -n "$i " 
f=`$1 $i`
 < output/$f.err  tr '\n+' '  '  | sed 's/...XXXmaxr/maxr/
s/[()[:alpha:]]//g
s/0://g'
 < output/$f.out  grep ME.size | sed 's/.*:.//g
s/ -> / /g' | tr '\n' ' '
echo " $((`cat output/$f.out | grep ^a | tail -n1 | sed s/a// | sed 's/ .*//'` + 1))"
done | python fix_time.py
) | tr ' ' '\t' | tee output/table_$1.txt

#cut -f1-5,8,11,14- < output/table.txt > output/table_cut.txt
#cut -f1,2,8,14,15,16 < output/table_$1.txt | awk '{ print $2 "\t" $1 }' > output/table_cut_$1.txt
< output/table_$1.txt awk '{ print $1 "\t" $16 "\t" $14 "\t" $15 "\t" $8 "\t" $2 }' > output/table_cut_$1.txt
#cut -f1,16,14,15,8,2 < output/table_$1.txt > output/table_cut_$1.txt

#(echo '\begin{tabular}{|c|r@{.}l|c|c|c|c|} \hline'
(echo '\begin{tabular}{|c|c|c|c|c|r@{.}l|} \hline'
tr '\t' '&' <  output/table_cut_$1.txt | sed 's/&/ & /g
s/finalme/FinalME/g
s/^i/$i$/g
s/formula/Formula/g
s/[.]/\&/g
s/max_mb/MB/g
s/origme/InputME/g
s/_/\\_/g
s/$/\\\\/g
2i\\\\hline 
s/user/\\multicolumn{2}{c|}{CPU}/' 
echo \\hline
echo '\end{tabular}') > output/table_cut_$1.tex
}

do_table orig "`seq 21`"
do_table pow10 "`seq 7`"
do_table pow10f "`seq 6`"
do_table squarepow2 "`seq 1 15`"

#cat < 
#cut -f1-5,8,11,14- output/table.txt
