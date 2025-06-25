use Test::More tests => 1;
my $out = `perl scanner.pl t/data/sample.pl`;
like($out, qr/redundant/, 'finds at least one redundancy');
