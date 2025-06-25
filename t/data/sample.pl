#!/usr/bin/env perl
use strict;
use warnings;
use DateTime;    # to test DateTime coercion in scanner

# sample data
my ($x, $y, $z, $a, $s) = (10, 5, 2, 0, 'foobar');
my $dt1 = DateTime->now->subtract( days => 1 );
my $dt2 = DateTime->now;

# 1) Boolean‐AND redundancy: $x>5 ⇒ $x>3
if ($x > 5 && $x > 3) {
    warn "AND redundancy\n";
}

# 2) Boolean‐OR redundancy: $y<10 ⇒ $y<7
if ($y < 10 || $y < 7) {
    warn "OR redundancy\n";
}

# 3) DateTime coercion test: dt1 <= dt2 && dt1 < dt2
if ($dt1 <= $dt2 && $dt1 < $dt2) {
    warn "DateTime AND redundancy\n";
}

# 4) Nested‐threshold redundancy: z>=2 ⇒ z>1
if ($z >= 2) {
    if ($z > 1) {
        warn "nested threshold\n";
    }
}

# 5) Identical nested structure: a==0 inside a==0
if ($a == 0) {
    if ($a == 0) {
        warn "identical nested\n";
    }
}

# 6) Duplicate regex match
if ($s =~ /foo/) {
    if ($s =~ /foo/) {
        warn "duplicate regex\n";
    }
}

1;
