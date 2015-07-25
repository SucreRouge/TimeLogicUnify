bash ../atl_formulas.sh

for WAIT in 600 3600
do

echo >> cache.txt
echo "--- CTL* formulas: $WAIT ---" >> cache.txt
for F in `cat ../../parser3/mark_formulas.txt`
do
	timeout $WAIT ../do.pl "$F"
	timeout $WAIT ../do.pl "~($F)"
	timeout $WAIT ../do.pl "~($F)" 9999
done

echo >> cache.txt
echo "--- NEW formulas: $WAIT ---" >> cache.txt

timeout $WAIT ../do.pl "(Xp&-{1}Xp)" 99
timeout $WAIT ../do.pl "({1}P&-P)"   4
timeout $WAIT ../do.pl "({1}{2}P&-{2}P)"   4
timeout $WAIT ../do.pl "(({1}P&{2}Q)>{12}(P&Q))" 
timeout $WAIT ../do.pl "~((~{1}~P&~{2}~Q)>{12}(P&Q))" 
timeout $WAIT ../do.pl "~(({1}P&{2}Q)>(P&Q))" 2
timeout $WAIT ../do.pl "~(({1}P&{2}Q)>(P&Q))" 3
(timeout $WAIT ../do.pl "({1}{2}P&-{2}P)" 64)
#) 2> add.err > add.out

#(
timeout $WAIT ../do.pl "(Xp&-{1}Xp)"  
timeout $WAIT ../do.pl "({1}{2}Xp&-{2}Xp)"  
timeout $WAIT ../do.pl "({1}{2}Xp&-{2}Xp)" 4
timeout $WAIT ../do.pl "({1}{2}P&-{2}P)"   4
#) 2> unsat.err > unsat.out

#(
timeout $WAIT ../do.pl "-w&{}G({1}X-w)&{2}Fw"
timeout $WAIT ../do.pl '{12}Xp&-{1}{2}Xp'
timeout $WAIT ../do.pl "-w&{}G({1}X-w)&{2}Fw"
#) 2> sat.err > sat.out 

timeout $WAIT ../do.pl 'p' # RESULT: SATISFIABLE #Hues=2 #Colours=2 #Remaining=2
timeout $WAIT ../do.pl 'p&p' # RESULT: SATISFIABLE #Hues=2 #Colours=2 #Remaining=2
timeout $WAIT ../do.pl 'p&-p' # RESULT: UNsatisfiable #Hues=2 #Colours=2 #Remaining=2
timeout $WAIT ../do.pl 'Xp&-Xp' # RESULT: UNsatisfiable #Hues=4 #Colours=6 #Remaining=6
timeout $WAIT ../do.pl 'Xp&-{}Xp' # RESULT: SATISFIABLE #Hues=5/12 #Colours=22 #Remaining=18
timeout $WAIT ../do.pl 'Xp&-{1}Xp' # RESULT: UNKNOWN #Hues=5/12 #Colours=18 #Remaining=18
timeout $WAIT ../do.pl 'Xp&-{2}Xp' # RESULT: SATISFIABLE #Hues=3/32 #Colours=182 #Remaining=132
timeout $WAIT ../do.pl 'Xp&-{1}Xp' 12 # RESULT: UNsatisfiable #Hues=12 #Colours=18 #Remaining=18
timeout $WAIT ../do.pl 'Xp&-{1}Xp&-{2}q' # RESULT: SATISFIABLE #Hues=2/256 #Colours=324 #Remaining=152
timeout $WAIT ../do.pl 'P&-{}P' # RESULT: SATISFIABLE #Hues=6 #Colours=11 #Remaining=9
timeout $WAIT ../do.pl '(-w&{}G({1}-Xw)&{2}Fw)' 2 # ==> RESULT: SATISFIABLE #Hues=2/360 #Colours=1117 #Remaining=422
timeout $WAIT ../do.pl '(-w&{}G({1}-Xw)&{2}XXw)' 2 # ==> RESULT: UNKNOWN #Hues=2/576 #Colours=2276 #Remaining=780
timeout $WAIT ../do.pl '(-w&{}G({1}-Xw)&{2}XXXw)' 2 # ==> RESULT: UNKNOWN #Hues=2/1152 #Colours=9074 #Remaining=2688

done
exit 

../atl '-{2}XXp&{}X{2}Xp' 3
(BATL_USE_WEAK=N /usr/bin/time ../atl '{1}(XXp|XXq)&{}X({2}-Xp&{2}Xp)' 2)
(BATL_USE_WEAK=N /usr/bin/time ../atl '{1}(XXp|XXq)&{}X({2}-Xp&{2}Xp)')

exit

grep SAT unsat.out; grep UNsat sat.out
