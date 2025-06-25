#!/usr/bin/env perl
use strict; use warnings;
my ($z,$a,$s) = (2,0,'foo');
if ($z >= 2) { 
  if ($z > 1) { }            # nested-threshold
}
if ($a == 0) {
  if ($a == 0) { }           # identical nested
}
if ($s =~ /foo/) {
  if ($s =~ /foo/) { }       # duplicate regex
}
