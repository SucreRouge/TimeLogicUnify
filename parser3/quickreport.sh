# grep size output/*99.out |/usr/bin/sort -n -k6 ; grep user output/*99.err | /usr/bin/sort -t_ -n -k3
# grep size output/*6s_100_*.out |/usr/bin/sort -n -k6 ; grep user output/*6s_100_*.err | /usr/bin/sort -t_ -n -k3

cd output || true
#for f in set_6sX_32768_32768_99.out
#for f in output/set_6sX_100_1000000_0.out
for f in set_6sX_1000000_100_2.out
do
	siz0=`< $f grep '^a0' | cut -f2 -d' '`
	ff=`echo $f | sed "s/.out//
s/_/-/g"`
	echo $siz0
	#grep '^a' output/set_6sX_1000000_100_2.out | sed s/^a// | awk '{ print ( ( '$siz0' * ( ( $1 + 6 ) ^ 0.5 ) ) / $2)  "\t" $1 "\t" $2 }'
	grep '^a' $f | grep "0 " | sed s/^a// | awk '{ print ($1^0.5) " " ( $2 / ( '$siz0' )) }' > tmp_
	#gnuplot -p -e "f(x) = m*sqrt(x+c);
	cd ..
	
	gnuplot -p -e "f(x) = m*x+b;
fit f(x) 'output/tmp_' via m,b;
set terminal postscript enhanced 'Times-Roman';
set output '"graphs/$ff.eps"';
set xlabel 'Square root of # of added atoms';
set ylabel 'Increase in size of ME (ratio)';
plot 'output/tmp_' title 'observed' with points, f(x) title 'fitted line'  ;"
#set terminal png;
#set output 'foo.png';"
#plot 'tmp_' title 'TITLE' with points, f(x) title 'fitted line'  "
#set terminal epslatex enhanced 'Times-Roman';
	#ps2pdf14 "../graphs/$f.ps"
	#cd graphs && ps2pdf -dEPSCrop $ff.eps $ff.pdf
	cd graphs && ps2pdf -dEPSCrop $ff.eps $ff.pdf
	evince $ff.pdf
	
done #| tee output/ratios.txt

exit

#for f in output/set_6sX_1000000_100_2.out
for f in output/set_6sX_*.out
#for f in output/set_6sX_32768_32768_99.out
do
	siz0=`< $f grep '^a0' | cut -f2 -d' '`
	echo $siz0
	#grep '^a' output/set_6sX_1000000_100_2.out | sed s/^a// | awk '{ print ( ( '$siz0' * ( ( $1 + 6 ) ^ 0.5 ) ) / $2)  "\t" $1 "\t" $2 }'
	grep '^a' $f | sed s/^a// | awk '{ print ( $2 / ( '$siz0' * ( ( $1 + 6 ) ^ 0.5 ) ) )  "\t" $1 "\t" $2 }'
done | tee output/ratios.txt
$ff.pdf
