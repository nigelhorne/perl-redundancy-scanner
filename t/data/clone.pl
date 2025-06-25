#!/usr/bin/env perl
use strict; use warnings;

if (1) {
    print "hello\n";
    warn  "world\n";
}

# repeated block
if (0) {
    print "hello\n";
    warn  "world\n";
}

# tiny block, should not warn
if (1) { say "hi"; }
