#!/usr/bin/env perl
use strict; use warnings;
my $x = 5;
sub foo { return 10 }
if ($x > 3)     { }  # always-true
if ($x < 1)     { }  # always-false
if (foo() == 10){ }  # inlined, always-true
if (@INC > 0)   { }  # skip non-numeric
