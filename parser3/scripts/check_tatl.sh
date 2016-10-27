#first run scripts/fuzztest.sh
cd scripts
cd ..
if [ ! -e work/tatl ]
then
	echo run ./install_tableaux.sh THEN run scripts/check_tatl.sh
	exit
fi
if [ ! -e fuzztest1.out ]
then
	echo first generate some results by runnign scripts/fuzztest.sh THEN run scripts/check_tatl.sh
	exit
fi

for d in 'mlsolver>tatl' 'tatl>mlsolver'
do
	echo
	echo "--- $d ---"
	#grep "$d" -B7 fuzztest.out fuzztest1.out | egrep 'Nega|fied.to' | sed s/.*:\ // | sed -f scripts/2tatl.sed
	cat fuzztest.out fuzztest1.out | egrep 'RULE|Simplified.to|Negation' | grep "$d" -B1 | egrep 'Nega|fied.to' | sed s/.*:\ // | sed -f scripts/2tatl.sed | sed 's/^/<<1>>(/' | sed 's/$/)/'
done

