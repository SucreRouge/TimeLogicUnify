#echo 'T' | ./unify | grep = | sed s/^/add\ /  > urules_trs.txt
if [ "$1" = "c" ]
then
	D=trs_out/
else
	D=trs_out_d/`date +%s`
	echo D: $D
	mkdir -p $D
	rm trs_out; ln -s $D trs_out
fi
D=`readlink -f $D`
WD=`pwd`
set -x

echo D: $D
echo 'T' | ./unify | grep = > $D/urules_trs.txt

mkdir -p $D/bad
mkdir -p $D/null


#rm $D/urules_trs_good.txt
#rm $D/urules_trs_bad.txt
#rm $D/urules_trs_redundant.txt
#touch $D/urules_trs_good.txt

echo "$D"/urules_trs.txt
#cat "$D"/urules_trs.txt | grep -F -x -f $D/urules_trs_good.txt -f $D/urules_trs_bad.txt $D/urules_trs_redundant.txt |  while read -r L
grep -F -x -v -f $D/urules_trs_good.txt -f $D/urules_trs_bad.txt -f $D/urules_trs_redundant.txt "$D"/urules_trs.txt |  while read -r L
do
	echo L: "$L" > $D/urules_trs_new.txt
	
	T=`date +%s`
	echo "$L" > $D/urules_trs_new.txt
	(
	#echo undo
	#echo '(VAR x y z)'
	echo '(VAR x y z d)'
	echo '(RULES'
 	cat $D/urules_trs_good.txt $D/urules_trs_new.txt | sed 's/^/  /
s/=/ -> /
s/0/0()/g
s/1/1()/g
s/a/x/g
s/b/y/g
s/c/z/g
' 
	echo ')'
	) > "$D"/mytrs.trs
	mv "$D"/out.trs "$D"/out.trs.bak
	( cd /home/john/.data/kbcv/kbcv-bundled-2.0.0.2
	#./kbcv -t -a -c  "$D"/mytrs.txt
	./kbcv -t -a  "$D"/mytrs.trs  -o "$D"/out.trs 2> "$D"/err.trs > "$D"/stdout.trs
	)
	if diff "$D"/out.trs "$D"/out.trs.bak
	then
		echo "$L" >> "$D"/urules_trs_redundant.txt
		echo "NULL: $L" 
		echo "NULL: $L" >> "$D/log"
	else
		#if grep VAR.x "$D"/out.trs
		if grep YES "$D"/stdout.trs
		then 
			echo "$L" >> "$D"/urules_trs_good.txt
			echo "GOOD: $L"
			echo "GOOD: $L" >> "$D/log"
			for f in mytrs.trs out.trs err.trs stdout.trs
			do
				cp "$D/$f" "$D/sav.$f"
				cp "$D/$f" "$D/$T.$f"
			done
		else	
			echo "$L" >> "$D"/urules_trs_bad.txt
			echo "BAD:  $L" >> "$D/log"
		fi
	fi
done
cat "$D"/out.trs
