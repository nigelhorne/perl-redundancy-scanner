#!/usr/bin/env perl
use strict;
use warnings;
use Test::More;

my $file = 't/data/or_equalities.pl';

# 1. Run the scanner on your test file
my $output = `./scanner.pl $file 2>&1`;

# 2. Check scanner launched
like($output, qr/Scanning $file/, 'scanner invoked');

# 3. Confirm no false "redundant in OR" between equalities
unlike($output, qr/redundant in OR/, 'no redundancy falsely flagged for multiple equality ORs');

done_testing;
