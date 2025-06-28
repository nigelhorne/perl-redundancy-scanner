use strict;
use warnings;

use Test::More;
use FindBin qw($Bin);

my $scanner = "$Bin/../scanner.pl";
my $out = `$scanner $Bin/data/nested.pl`;

like($out, qr/redundant "\$z > 1" under "\$z >= 2"/, 'nested threshold');
like($out, qr/nested identical if "\(\$a == 0\)"/, 'identical nested');
like($out, qr/duplicate regex match foo/, 'duplicate regex');

done_testing();
