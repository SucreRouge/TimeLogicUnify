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

#print "[$entry]\n";
$result = `./atl '$ARGV[0]' $ARGV[1] $ARGV[2] | bash -c "tee >(cat 1>&2)" | grep RESULT`;

open(my $fd, ">>cache.txt");
print $fd "$entry ==> $result";
print $result;
