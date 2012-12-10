

orig () {
	echo $1
}

pow10 () {
	echo set_6sX_100_$((10**$i))_0
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
cut -f1,2,8,14- < output/table_$1.txt > output/table_cut_$1.txt

(echo '\begin{tabular}{|c|r@{.}l|c|c|c|c|} \hline'
tr '\t' '&' <  output/table_cut_$1.txt | sed 's/&/ & /g
s/[.]/\&/g
s/max_mb/MB/g
s/_/\\_/g
s/$/\\\\/g
2i\\\\hline 
s/user/\\multicolumn{2}{c|}{CPU}/' 
echo \\hline
echo '\end{tabular}') > output/table_cut_$1.tex
}

do_table orig "`seq 21`"
do_table pow10 "`seq 7`"

#cat < 
#cut -f1-5,8,11,14- output/table.txt
