#!/usr/bin/env perl
use strict; use warnings;

my $age = 10;
my $score = 79;
my $grade;

# Detect adjacent-complements:
if ($age >= 18) {
    print "Adult";
} elsif ($age < 18) {  # Redundant complement
    print "Minor"; 
}

# detect exhaustive-without-else:
if ($score >= 60) {
    $grade = 'Pass';
} elsif ($score < 60) {  # Should just be 'else'
    $grade = 'Fail';
}
