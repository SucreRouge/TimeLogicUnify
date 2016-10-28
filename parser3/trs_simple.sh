i=1
set -x
cp urules_trs_good.txt urules_trs_good_filtered.txt
cp urules_trs_good.txt urules_trs_good_filtered_new.txt

make_trs(){
        (
	echo -n BEGIN
        #echo undo
        echo '(VAR x y z a b c)'
        echo '(RULES'
        #cat work/urules.txt | 
echo T | ./unify | sed 's/^/  /
s/=/ -> /
s/0/0()/g
s/1/1()/g
'
        echo ')'
        ) > /tmp/mytrs.trs
        mv /tmp/out.trs /tmp/out.trs.bak
        ( cd /home/john/.data/kbcv/kbcv-bundled-2.0.0.2
        #./kbcv -t -a -c  /tmp/mytrs.txt
	echo -n .
        ./kbcv -t -a  /tmp/mytrs.trs  -o /tmp/out.trs 2> /tmp/err.trs > /tmp/stdout.trs
        )
	echo DONE
}

make_trs
exit
cp /tmp/out.trs	/tmp/out_orig.trs

cat urules_trs_good.txt | while read -r L 
do
	echo $L
	fgrep -v -x -- "$L" < urules_trs_good_filtered.txt > urules_trs_good_filtered_new.txt
	make_trs
	
        if diff /tmp/out.trs /tmp/out_orig.trs
        then
                echo "$L" >> urules_trs2_redundant.txt
                echo "NULL: $L" 
		mv urules_trs_good_filtered_new.txt urules_trs_good_filtered.txt
        else
                echo "GOOD: $L"
        fi
done



#while [ $i -lt `wc -l urules_trs_good_filtered.txt ]
#do
#while [ $i -lt `wc -l urules_trs_good_filtered.txt` ]
#do
#done
	
