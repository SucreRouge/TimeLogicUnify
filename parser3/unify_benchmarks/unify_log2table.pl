%results=();
$ID="";
while (<>) {
	if (/^ID: ([[:alnum:]]*)/) {
		if ($ID > "") {
my @keys = sort { $a cmp $b } keys %results;  my @vals = @results{@keys}; print "$ID @vals\n#ID @keys\n";
		}
		#print "ID -- $1\n";
		$ID=$1;
	} elsif (/^  Satisfiable: ([[:alpha:]_-]*)\(([0-9.]*)\)/) {
		#print "Y $1 $2\n";
		$results{$1}="Y";
	} elsif (/^  UNsatisfiable: ([[:alpha:]_-]*)\(([0-9.]*)\)/) {
		#print "N $1 $2\n";
		$results{$1}="N";
	} elsif (/^UNKNOWN ([[:alpha:]_-]*)/) {
		#print "? $1\n";
		$results{$1}="?";
		
	}
}
			

