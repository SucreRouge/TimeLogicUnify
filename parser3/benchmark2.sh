F=output/table.txt
rm $F
(
echo -n "i "
/usr/bin/time true 2>&1 |  tr '\n+' '  '  | sed 's/[().:0-9]//g
s/maxresidentk/max_mb/
s/pagefaults/pf/'
echo origme finalme

for i in `seq 18`
do
echo -n "$i " 
 < output/$i.err  tr '\n+' '  '  | sed 's/...XXXmaxr/maxr/
s/[()[:alpha:]]//g
s/0://g'
 < output/$i.out  grep ME.size | sed 's/.*:.//g
s/ -> / /g'
done | python fix_time.py
) | tr ' ' '\t' | tee output/table.txt

cut -f1-5,8,11,14- < output/table.txt > output/table_cut.txt
#cut -f1-5,8,11,14- output/table.txt
