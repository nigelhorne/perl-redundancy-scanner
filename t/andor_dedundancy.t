use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);

my $scanner = "$Bin/../scanner.pl";
my $out     = `$scanner $Bin/data/and_or.pl`;

like($out, qr/redundant in AND with "\$x > 5"/, 'AND redundancy');
like($out, qr/redundant in OR  with "\$y < 10"/, 'OR redundancy');

done_testing();
