while(my $L=<>) {
	if ($L=~/^(\s+)((?:assert|print).*);;/) {
		print "$1 let () = $2\n";
	} elsif ($L=~/^((?:assert|print).*)/) {
		print "let () = $1\n";
	} else {
		$L=~s/;;//g;
		$L=~s/^end$/end;;/g;
		print $L
	}
}
