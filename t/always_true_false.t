use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);

my $scanner = "$Bin/../scanner.pl";
my $out     = `$scanner $Bin/data/always.pl`;

like($out, qr/always-true test "\$x > 3"/,    'always-true $x>3');
like($out, qr/always-false test "\$x < 1" /,  'always-false $x<1');
like($out, qr/always-true test "foo\(\) == 10"/,'inlined foo()');
unlike($out, qr/@INC/,                        'skipped non-numeric @INC');

done_testing();
