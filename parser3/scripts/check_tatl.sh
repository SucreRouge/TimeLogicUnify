#first run scripts/fuzztest.sh
for d in 'mlsolver>tatl' 'tatl>mlsolver'
do
	echo
	echo "--- $d ---"
	#grep "$d" -B7 fuzztest.out fuzztest1.out | egrep 'Nega|fied.to' | sed s/.*:\ // | sed -f scripts/2tatl.sed
	cat fuzztest.out fuzztest1.out | egrep 'RULE|Simplified.to|Negation' | grep "$d" -B1 | egrep 'Nega|fied.to' | sed s/.*:\ // | sed -f scripts/2tatl.sed | sed 's/^/<<1>>(/' | sed 's/$/)/'
done

