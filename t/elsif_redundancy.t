use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);

my $scanner = "$Bin/../scanner.pl";
my $out     = `$scanner $Bin/data/elsif.pl`;

like($out, qr/redundant elsif "\(\$x > 3\)" implied by "\(\$x > 5\)"/,  'elsif #1');
like($out, qr/redundant elsif "\(\$x > 2\)" implied by "\(\$x > 5\)"/,  'elsif #2');

done_testing();
