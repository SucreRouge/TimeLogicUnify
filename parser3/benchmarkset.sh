do_one () {
	f=output/set_$1_$2_$3_$4.err

	if [ ! \( -e "$f" -a -z "$BENCHMARK_FORCE_RECOMPUTE" \) ]
	then
		echo Making $f
		echo "$1 $2 $3 $4" > output/inprogress
		ocaml make_random_formulas_$1.ml $2 $3 $4 | /usr/bin/time ./main 2> output/set_$1_$2_$3_$4.err > output/set_$1_$2_$3_$4.out
		rm output/inprogress
	fi
}
mkdir -p output 

sudo cpufreq-set -g performance
#cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_available_governors

if [ -e output/inprogress ]
then 
	( BENCHMARK_FORCE_RECOMPUTE=y
	do_one `cat output/inprogress` )
fi

for type in 6sX 6s
do

for i in 10 100 1000 10000 100000 1000000 5000000 10000000
#for i in 10 100 1000 10000 100000 1000000 10000000 20000000 50000000 100000000
do
	do_one $type 100 $i 0	
done

for i in 10 100 1000 10000 100000 1000000
#for i in 10 100 1000 10000 100000 1000000 10000000 20000000 50000000 100000000
do
	do_one $type $i 100 2	
done


for i in 000 010 020 050  
do
	do_one $type $i 10000 1	
done

for i in `seq 10`
do
	do_one $type $i"00" 10000 1	
done

for i in `seq 16`
do
	e=$((2**$i))
	do_one $type $e $e 99
done
done

exit
for i in `seq 30`
do
        ocaml make_formulas.ml $i | /usr/bin/time ./main 2> output/$i.err > output/$i.out
done


sudo cpufreq-set -g ondemand
