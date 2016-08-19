%results=();
$IN="";
$ID="";
while (<>) {
	if (/^ID: ([[:alnum:]]*)/) {
		print "|| $IN\n ";
		$ID=$1;
	}elsif (/^Negation: (.*)/) {
		$IN=$1;
		print "|| $ID\n "
	} elsif (/^Input formula: (.*)/) {
		$IN=$1;
	} elsif (/^  Satisfiable: ([[:alpha:]_-]*)\(([0-9.]*)\)/) {
		print " Y:$1:"
	} elsif (/^  UNsatisfiable: ([[:alpha:]_-]*)\(([0-9.]*)\)/) {
		print " N:$1:"
	} elsif (/^UNKNOWN ([[:alpha:]_-]*)/) {
		print " _:$1:"
		#print "? $1\n";
	}
}
			

