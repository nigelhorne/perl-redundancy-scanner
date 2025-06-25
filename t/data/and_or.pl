#!/usr/bin/env perl
use strict; use warnings;
my ($x,$y) = (10,5);
if ($x > 5 && $x > 3) { }    # AND redundancy
if ($y < 10 || $y < 7) { }   # OR redundancy
