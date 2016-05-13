# Download from http://cime.lri.fr/  or more directly http://a3pat.ensiie.fr/pub/index.en.html
filter () {
	#sed '0,/^Completion succeeded/d' | grep '[-][>]'
	< $1 sed '0,/^Completion succeeded/d' | sed '0,/trs A_CTL/g' | grep '[-][>]' 
}

mkdir out
echo > out/good.cime
echo asdasf fdsa > out/good.output
echo > out/bad.cime
echo > out/redundant.cime
#( ./gen_cime.sh; ./gen_cime.sh) | while read L
./gen_cime.sh | while read L
do
	cp out/good.cime out/new.cime
	echo $L >> out/new.cime
	cat cime_header.txt out/new.cime cime_footer.txt > out/whole.cime
	if timeout 1 ./c3_2605_stat2.opt < out/whole.cime > out/new.output && grep 'Completion succeeded' out/new.output
	then
		if [ "X`filter out/good.output`" = "X`filter out/new.output`" ]
		#if [ "X`< out/new.output | grep \; | grep '^  [^ ]'`" = "X`< out/good.output | grep \; | grep '^  [^ ]'`" ]
		then
			echo $L >> out/redundant.cime
			echo "UNEEDED: $L"
		else
			echo $L >> out/good.cime
			echo "GOOD:    $L"
			cp out/new.output out/good.output
		fi
	else
		echo $L >> out/bad.cime
		echo "BAD:     $L"
	fi			
done
