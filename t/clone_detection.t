use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);

my $scanner = "$Bin/../scanner.pl";
my $out     = `$scanner $Bin/data/clone.pl`;

# should warn exactly once, listing both line numbers (3 and 8)
my @warns = $out =~ qr/clone of 4-line block at lines/s;
is(scalar @warns, 1, 'one clone warning for the repeated block');

done_testing();
