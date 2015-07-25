timeout 1600 ../do.pl "(Xp&-{1}Xp)" 99
timeout 1600 ../do.pl "(Xp&-{2}Xp)" 
timeout 1600 ../do.pl "(Xp&-{1}Xp)&{2}p" 
(
timeout 1600 ../do.pl "-w&{}G({1}X-w)&{2}Fw" 2
timeout 1600 ../do.pl "-w&{}G({1}X-w)&{2}Xw" 2
timeout 1600 ../do.pl "-w&{}G({1}X-w)&{2}XXw" 2
timeout 1600 ../do.pl '{12}Xp&-{1}{2}Xp'
timeout 1600 ../do.pl "-w&{}G({1}X-w)&{2}Fw"
(BATL_USE_WEAK=N /usr/bin/time ../atl '{1}(XXp|XXq)&{}X({2}-Xp&{2}Xp)')
) #2> sat2.err #> sat.out 
 
