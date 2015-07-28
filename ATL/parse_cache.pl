#!/usr/bin/perl
open(my $fd, "<", "cache.txt");

my %res1 = (); 

#'(Xp&-{1}Xp)' 99 ==> 0.00user 0.01system 0:00.03elapsed 25%CPU (0avgtext+0avgdata 4232maxresident)k	0inputs+0outputs (0major+635minor)pagefaults 0swaps	RESULT: UNsatisfiable #Hues=12 #Colours=18 #Remaining=18
while(<$fd>) {
	#print $_;
	if (/('[^']*').*(\d+[.]\d\d)user .* (\d+)maxresident.*RESULT: (\w+) #Hues=([0-9\/]+) #Colours=(\d+)/) {
		$key = $1;
		$sat = $4; {$sat =~ s/unsatisfiable/N/i; $sat =~ s/satisfiable/Y/i; $sat =~ s/unknown/?/i;}
		$res= "$2 &	$3 &	$sat	&	$6 &	$5";
		#print "$1, $2, $3, $4, $sat, $6, $7\n";
		if ($res1{$key} =~ /sat/i) {
			#print "match\n";
			# Already computed
		} else {
			$res1{$key}=$res;
			#print "RES: $key ==> " . $res1{$key} . "\n";
			#print "RESX: $res\n";
		}
	} else {
		if (/==>/) { print "ERROR!\n"; exit 1;}
	}
}

close $fd;

my %res2 = (); 

open(my $fd, "<", "cache2.txt");

#'-(~{1}p U q)' ==> 0.00user 0.00system 0:00.00elapsed 33%CPU (0avgtext+0avgdata 3548maxresident)k	0inputs+0outputs (0major+271minor)pagefaults 0swaps	[[]](G~q \/ (~qU(<<1>>p /\ ~q)))	Fin de la construction du tableau	0.001233	nb_prestate initial tableau: 4	nb_state initial tableau:8	Fin de la phase d'elimination	0.001305	nb_prestate final tableau: 4	nb_state final tableau:8	* The formula is satisfiable.	Fin de la procedure : 0.001326	0.001329

while(<$fd>) {
	#print $_;
	if (/('[^']*').*(\d+[.]\d\d)user .*(\d+)maxresident.*nb_state.?initial.?tableau:(\d+).*[Tt]he formula is (\w+)/) {
	#if (/('[^']*')( [0-9]+)?.*(\d+[.]\d\d)user .*(\d+)maxresident.*[Tt]he formula is (\w+)/) {
		$key = $1;
		$sat = $5; {$sat =~ s/unsatisfiable/N/i; $sat =~ s/satisfiable/Y/i; $sat =~ s/unknown/?/i;}
		$res= "$2 &	$3 &	$sat	&	$4";
		#print "$1, $2, $3, $4, $sat, $6, $7\n";
		if ($res2{$key} =~ /sat/i) {
			#print "match\n";
			# Already computed
		} else {
			$res2{$key}=$res;
			#print "RES: $key ==> " . $res2{$key} . "\n";
			#print "RESX: $res\n";
		}
	} else {
		if (/==>/) { print "ERROR!\n"; exit 1;}
	}
}
#exit 1;

close $fd;

use strict;


foreach my $key (keys %res1) {
	my $out = ""; #$key;
	my @a = split ("&	", $res1{$key});
	my @b = split ("&	", $res2{$key});
	for (my $i=0;$i<5;$i++) {
		$out .= "\t" . $a[$i];
		$out .= "\t" . $b[$i];
	}
	$key=~s/ //g;

	$out .= "$key"; 
	print "$key & $res1{$key} & $res2{$key} \\\\\n";
	print "$out\n";
}  

exit 1;

