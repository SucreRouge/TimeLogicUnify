@unused=();
while (my $L=<>) {
	@unused = map { $e =$_; $L =~ /\b$e\b/ ? () : $e } @unused;
	if ($L =~ /let\s+(?:rec\s+)?(\w+).*=/) {
		push @unused, $1;
	}
}
foreach (@unused) { print "$_\n" };
