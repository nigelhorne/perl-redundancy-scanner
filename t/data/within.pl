#!/usr/bin/perl

my $number = <STDIN>;
chomp $number;

if($number > 1900 && $number <= 2000) {
	print "20th Century\n";
}
