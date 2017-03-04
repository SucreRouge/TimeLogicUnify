cd ctlcomparison/benchmarks
echo '<'
#grep -v 'p25\|px\|q1' `find | grep ctl` | perl -pe 's/.*://; # remove find garbage
find | grep '[.]ctl$' | while read f; do cat $f; echo; done |
perl -pe 's/.*://; # remove find garbage
$i=0;%h=();s/\b([[:lower:]0-9]+)\b/if(not defined $h{$1}){$h{$1}=$i++};chr(ord(a)+$h{$1})/eg; #rename atoms to single letters
if($i>26){s/.*/ERROR:TOO MANY ATOMS/}; #But we cant have more than 26 ASCII letters
s/False/0/g;s/True/1/g; 
s/ => / > /g;
s/^/~(/g;
s/$/)/;'  | grep -v ATOMS | grep -v B | (cd ../.. ; ./unify --filter-ctl)

#Relace last three lines with following line for original benchmarks.
#s/ => / > /g;'  | grep -v ATOMS | grep -v B | (cd ../.. ; ./unify --filter-ctl)
#s/p([0-9]*)/chr(ord(a)+$1)/eg;
