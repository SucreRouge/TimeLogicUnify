./main < test_in.txt | grep 'formula' | sed s/.*:.// > test_out.tmp

if [ "$1" = v ]
then
	while read -u 4 a
	do
		read -u 5 b
		echo "$a" '->' "$b"
	done
fi 4< test_in.txt 5< test_out.tmp

diff test_out.tmp test_out.txt


