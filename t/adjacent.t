use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);

my $scanner = "$Bin/../scanner.pl";
my $out     = `$scanner $Bin/data/adjacent.pl`;

like($out, qr/adjacent complement conditions:/);
like($out, qr/adjacent complement conditions: "\(\$age >= 18\)"/);
like($out, qr/adjacent complement conditions: "\(\$score >= 60\)"/);

done_testing();
