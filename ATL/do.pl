#!/usr/bin/perl
`if [ atl.ml -nt atl ]; then ocamlopt atl.ml -o atl; fi`;
my $entry="'$ARGV[0]' $ARGV[1] $ARGV[2]";
$entry=~s/ *$//;
#print "[$entry]\n";
my $cache=`touch cache.txt; cat cache.txt`;
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

system("perl ../do_tatl.pl '$ARGV[0]'");

#print "[$entry]\n";
#$result = `(/usr/bin/time nice -19 ./atl '$ARGV[0]' $ARGV[1] $ARGV[2] | bash -c "tee >(cat 1>&2)" | tail -n1) 2>&1 | tr '\n' '\t'`;
$result = `(nice -19 /usr/bin/time ../atl '$ARGV[0]' $ARGV[1] $ARGV[2] | tail -n1) 2>&1 | tr '\n' '\t'`;
$result=~s/\t$//;

open(my $fd, ">>cache.txt");
print $fd "$entry ==> $result\n";
print "$result\n";
close $fd;
