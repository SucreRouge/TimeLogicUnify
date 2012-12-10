F=output/table.txt
rm $F
(
echo -n "i "
/usr/bin/time true 2>&1 |  tr '\n+' '  '  | sed 's/[().:0-9]//g
s/maxresidentk/max_mb/
s/pagefaults/pf/'
echo origme finalme formula

for i in `seq 21`
do
echo -n "$i " 
 < output/$i.err  tr '\n+' '  '  | sed 's/...XXXmaxr/maxr/
s/[()[:alpha:]]//g
s/0://g'
 < output/$i.out  grep ME.size | sed 's/.*:.//g
s/ -> / /g' | tr '\n' ' '
echo " $((`cat output/$i.out | grep ^a | tail -n1 | sed s/a// | sed 's/ .*//'` + 1))"
done | python fix_time.py
) | tr ' ' '\t' | tee output/table.txt

#cut -f1-5,8,11,14- < output/table.txt > output/table_cut.txt
cut -f1,2,8,14- < output/table.txt > output/table_cut.txt

tr '\t' '&' <  output/table_cut.txt | sed 's/&/ & /g
s/\./&/g
2i\\\\hline ' 

#cat < 
#cut -f1-5,8,11,14- output/table.txt
