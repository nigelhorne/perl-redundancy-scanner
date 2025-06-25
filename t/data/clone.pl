#!/usr/bin/env perl
use strict;
use warnings;

# large block #1
if (1) {
    print "hello\n";
    warn  "world\n";
    my $foo = "foobar";
    my $bar = $foo . "baz\n";
}

# repeated block #2
if (0) {
    print "hello\n";
    warn  "world\n";
    my $foo = "foobar";
    my $bar = $foo . "baz\n";
}

# tiny block, should not warn
if (1) { say "hi"; }
