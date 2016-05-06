@unused=();
while (my $L=<>) {
	@unused = map { $e =$_; $L =~ /$e/ ? () : $e } @unused;
	if ($L =~ /let\s+(?:rec\s+)?(\w\w+)/) {
		push @unused, $1;
	}
}
foreach (@unused) { print "$_\n" };
