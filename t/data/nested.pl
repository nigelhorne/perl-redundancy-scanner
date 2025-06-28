#!/usr/bin/env perl
use strict; use warnings;
my ($z,$a,$s) = (2,0,'foo');
if ($z >= 2) { 
  if ($z > 1) { print __LINE__, "\n" }            # nested-threshold
}
if ($a == 0) {
  if ($a == 0) { print __LINE__, "\n" }           # identical nested
}
if ($s =~ /foo/) {
  if ($s =~ /foo/) { print __LINE__, "\n" }       # duplicate regex
}
