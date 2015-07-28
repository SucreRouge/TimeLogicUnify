#!/usr/bin/perl
print "hi\n";
open(my $fd, "<", "cache.txt");

my %res1 = (); 

#'(Xp&-{1}Xp)' 99 ==> 0.00user 0.01system 0:00.03elapsed 25%CPU (0avgtext+0avgdata 4232maxresident)k	0inputs+0outputs (0major+635minor)pagefaults 0swaps	RESULT: UNsatisfiable #Hues=12 #Colours=18 #Remaining=18
while(<$fd>) {
	print $_;
	if (/('[^']*')( [0-9]+)?.*(\d+[.]\d\d)user .* (\d+)maxresident.*RESULT: (\w+) #Hues=([0-9\/]+) #Colours=(\d+)/) {
		$res= "$1, $2, $3, $4, $5, $6, $7";
		$key = $1;
		print "$1, $2, $3, $4, $5, $6, $7\n";
		if ($res1{$key} =~ /sat/i) {
			print "match\n";
			# Already computed
		} else {
			$res1{$key}=$res;
			print "RES: $key ==> " . $res1{$key} . "\n";
			print "RESX: $res\n";
		}
	}
}

close $fd;

my %res2 = (); 

open(my $fd, "<", "cache2.txt");

#'-(~{1}p U q)' ==> 0.00user 0.00system 0:00.00elapsed 33%CPU (0avgtext+0avgdata 3548maxresident)k	0inputs+0outputs (0major+271minor)pagefaults 0swaps	[[]](G~q \/ (~qU(<<1>>p /\ ~q)))	Fin de la construction du tableau	0.001233	nb_prestate initial tableau: 4	nb_state initial tableau:8	Fin de la phase d'elimination	0.001305	nb_prestate final tableau: 4	nb_state final tableau:8	* The formula is satisfiable.	Fin de la procedure : 0.001326	0.001329

while(<$fd>) {
	print $_;
	if (/('[^']*')( [0-9]+)?.*(\d+[.]\d\d)user .* (\d+)maxresident.* the formula is (\w+)/) {
		$res= "$1, $2, $3, $4, $5, $6, $7";
		$key = $1;
		print "$1, $2, $3, $4, $5, $6, $7\n";
		if ($res2{$key} =~ /sat/i) {
			print "match\n";
			# Already computed
		} else {
			$res2{$key}=$res;
			print "RES: $key ==> " . $res2{$key} . "\n";
			print "RESX: $res\n";
		}
	}
}



close $fd;


exit 1;
my $entry="'$ARGV[0]'";
$entry=~s/ *$//;
#print "[$entry]\n";
my $cache=`touch cache2.txt; cat cache2.txt`;
#print $cache;
for (split /^/, $cache){
	if (/(.*) ==> (.*)/) {
		if ($1 eq $entry) {
			print $_;
			print "$2\n";
			exit 0;
		}
	}
}
my $trans=$ARGV[0];
$trans=~s,P,p,g;
$trans=~s,Q,q,g;
$trans=~s,R,r,g;
$trans=~s,-,~,g;
$trans=~s,>,->,g;
$trans=~s,[\^],/\\,g;
$trans=~s,[&],/\\,g;
$trans=~s,[|],\\/,g;
$trans=~s,{,<<,g;
$trans=~s,},>>,g;
$trans=~s,=,<->,g;
$trans="[[]]($trans)";
print "TRANSLATED: $trans\n";
#1print "(nice -19 sudo -u johnlow /usr/bin/time ./tatl -o -f '$trans' | grep formula.is) 2>&1 | tr '\n' '\t'\n";
#print "[$entry]\n";
#$result = `(/usr/bin/time nice -19 ./atl '$ARGV[0]' $ARGV[1] $ARGV[2] | bash -c "tee >(cat 1>&2)" | tail -n1) 2>&1 | tr '\n' '\t'`;
#$result = `(nice -19 sudo -u johnlow /usr/bin/time ./tatl -o -f '$trans' | grep 'formula.is\|[0-9]' ) 2>&1 | tr '\n' '\t'`;
#$result = `(nice -19 sudo -u johnlow /usr/bin/time ../tatl -o -f '$trans' | grep -v '|' | grep -v '.---' | grep ... ) 2>&1 | tr '\n' '\t'`;
$result = `(nice -19 /usr/bin/time ../tatl -o -f '$trans' | grep -v '|' | grep -v '.---' | grep ... ) 2>&1 | tr '\n' '\t'`;
#$result = `(nice -19 sudo -u johnlow /usr/bin/time ./tatl -o -f '$trans' ) 2>&1 '`;
$result=~s/\t$//;
open(my $fd, ">>cache2.txt");
print $fd "$entry ==> $result\n";
print "$result\n";
