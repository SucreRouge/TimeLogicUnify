# grep size output/*99.out |/usr/bin/sort -n -k6 ; grep user output/*99.err | /usr/bin/sort -t_ -n -k3
# grep size output/*6s_100_*.out |/usr/bin/sort -n -k6 ; grep user output/*6s_100_*.err | /usr/bin/sort -t_ -n -k3


#for f in output/set_6sX_1000000_100_2.out
for f in output/set_6sX_*.out
#for f in output/set_6sX_32768_32768_99.out
do
	siz0=`< $f grep '^a0' | cut -f2 -d' '`
	echo $siz0
	#grep '^a' output/set_6sX_1000000_100_2.out | sed s/^a// | awk '{ print ( ( '$siz0' * ( ( $1 + 6 ) ^ 0.5 ) ) / $2)  "\t" $1 "\t" $2 }'
	grep '^a' $f | sed s/^a// | awk '{ print ( $2 / ( '$siz0' * ( ( $1 + 6 ) ^ 0.5 ) ) )  "\t" $1 "\t" $2 }'
done | tee output/ratios.txt

