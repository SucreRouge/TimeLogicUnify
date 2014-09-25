#!/usr/bin/perl -w

use strict;
use encoding 'utf8';
use utf8;
use Encode;
 
my $inf;
my $outf;
my $sep;
my $in;
my $out;

$inf= shift or $inf = "-";
$outf= shift or $outf = "-";
$sep = shift or $sep = "\t";

if ($inf eq "-") {
  $in = *STDIN;
}
else {
  open IN, "<:utf8", $inf or die $!;
  $in = *IN;
}

if ($outf eq "-") {
  $out = *STDOUT;
}
else {
  open OUT, ">:utf8", $outf or die $!;
  $out = *OUT;
}

while (my $s = <$in>) {
	$s =~ s/\s*[\r\n]+$//g;	
	next if $s eq "";
	
	if ( $s =~ /^---+\s*$/ ) {
		print $out "\\hline\n";
	}
	else {
	    #if ( ! $s =~ /\{.*\}|[\\]/ ) {
			$s =~ s/([&#\$^@%])/\\$1/g;
			$s =~ s/"/\\textquotedbl{}/g;
			$s =~ s/'/\\textquotesingle{}/g;
		#}
		
		$s =~ s/\Q$sep\E/ \t& /g;

		$s =~ s/((?:^| \t& )\d{1,3})(\d{3})(\d{3})(\d{3})(\d{3})/$1\\thinspace$2\\thinspace$3\\thinspace$4\\thinspace$5/g;
		$s =~ s/((?:^| \t& )\d{1,3})(\d{3})(\d{3})(\d{3})/$1\\thinspace$2\\thinspace$3\\thinspace$4/g;
		$s =~ s/((?:^| \t& )\d{1,3})(\d{3})(\d{3})/$1\\thinspace$2\\thinspace$3/g;
		$s =~ s/((?:^| \t& )\d{1,3})(\d{3})/$1\\thinspace$2/g;

		$s =~ s/(?<![^ ])(\d+)\.(\d+)(?=$| \t& )/$1,$2/g;
		
		print $out "$s \\\\\n";
	}
	
}

close $in;
close $out;
