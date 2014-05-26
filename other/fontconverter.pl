#!/usr/bin/perl

use strict;
use warnings;

my $row = 0;
my $array;
while (<>)
{
	chomp;
	
	$array->[$row] = ();
	my $col = 0;
	foreach my $line ( $_ =~ m/......./g ) {
		$line = substr($line, 0, 6);
		push @{$array->[int($row / 8)]->[$col]}, $line;
		$col++;
	}
	$row++;
}

foreach my $row (0..2) {
	foreach my $col (0..31) {
		foreach my $line (0..7) {
			my $pixels = $array->[$row]->[$col]->[$line];
			my $bin = $pixels;
			$bin =~ s/ /0/g;
			$bin =~ s/\#/1/g;
			
			print  "\t\t.byte 0b" . $bin . "00 ;     " . $pixels . "\n";
		}
		print "\n";
	}
}