use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);

my $scanner = "$Bin/../scanner.pl";
my $out = `$scanner $Bin/data/fp.pl`;

cmp_ok($out, 'eq', "\nScanning /Users/njh/src/njh/perl-redundancy-scanner/t/data/fp.pl …\n", 'no false positives');

done_testing();
