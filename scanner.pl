#!/usr/bin/env perl
use strict;
use warnings;
use PPI;
use Scalar::Util qw(blessed looks_like_number);
use JSON::MaybeXS;    # JSON::XS, Cpanel::JSON::XS or JSON fallback

#
# Helper: coerce any DateTime or numeric‐looking value to a plain number
#
sub coerce_number {
  my ($val) = @_;
  if (blessed($val) && $val->isa('DateTime')) {
    return $val->epoch;
  }
  return 0 + $val if looks_like_number($val);
  return int($val)   if $val =~ /^\s*([+-]?\d+)\s*$/;
  return;
}

#
# parse “$var op val” or “val op $var” → ($var, $op, $val)
#
sub parse_cond {
  my $s = shift;
  $s =~ s/^\s*\(//;  $s =~ s/\)\s*$//;
  return ($1,$2,$3) if $s =~ /^\s*\$(\w+)\s*(>=|>|==|<=|<|eq|ne)\s*(.+?)\s*$/;
  if ($s =~ /^\s*(.+?)\s*(>=|>|==|<=|<|eq|ne)\s*\$(\w+)\s*$/) {
    my ($v,$o,$var) = ($1,$2,$3);
    my %flip = (
      '<'  => '>', '<=' => '>=',
      '>'  => '<', '>=' => '<=',
      '==' => '==','eq' => 'eq','ne'=>'ne',
    );
    return ($var, $flip{$o}, $v);
  }
  return;
}

#
# ∀x: (x opA valA) ⇒ (x opB valB)?
#
sub implies {
  my ($opA,$vA,$opB,$vB) = map { "" . $_ } @_;  # stringify ops
  return unless $opA =~ /^(?:>=?|<=?|==|eq|ne)$/ && $opB =~ /^(?:>=?|<=?|==|eq|ne)$/;
  return unless defined( my $nA = coerce_number($vA) )
             && defined( my $nB = coerce_number($vB) );
  if ($opA =~ /^>=?$/ && $opB =~ /^>=?$/) { return $nA >= $nB }
  if ($opA =~ /^<=?$/ && $opB =~ /^<=?$/) { return $nA <= $nB }
  return;
}

#
# SARIF boilerplate
#
my $SARIF_VERSION = "2.1.0";
my $SARIF_SCHEMA  = "https://schemastore.azurewebsites.net/schemas/json/sarif-2.1.0.json";

# define your rule metadata
my @RULE_DEFS = (
  { id => "boolean-redundancy",
    shortDescription => { text => "Redundant comparison in boolean expression" },
    helpUri         => "" },
  { id => "nested-threshold",
    shortDescription => { text => "Nested numeric threshold redundant" },
    helpUri         => "" },
  { id => "identical-structure",
    shortDescription => { text => "Identical nested condition" },
    helpUri         => "" },
  { id => "duplicate-test",
    shortDescription => { text => "Exact duplicate test" },
    helpUri         => "" },
  { id => "duplicate-regex",
    shortDescription => { text => "Duplicate regex match" },
    helpUri         => "" },
);

# gather SARIF results here
my @sarif_results;

# detect --sarif switch
my $do_sarif = 0;
if (@ARGV && $ARGV[0] eq '--sarif') {
  shift @ARGV;
  $do_sarif = 1;
}

die "Usage: $0 [--sarif] file1.pl [file2.pl ...]\n" unless @ARGV;

for my $file (@ARGV) {
  my $doc = PPI::Document->new($file)
    or warn "✗ Failed to parse $file: $@\n" and next;

  # find all conditioning statements
  my $cmps = $doc->find('PPI::Statement::Compound') || [];
  for my $st (@$cmps) {
    my $type = $st->type;
    next unless $type =~ /^(?:if|elsif|unless|while|until)$/;

    my $raw = $st->schild(1)->content;         # "(…)"
    my $ln  = $st->line_number;

    #
    # 1) Boolean AND/OR redundancy
    #
    {
      my @parts = map { s/^\s+|\s+$//gr }
                  split /(?:&&|\|\|)/, $raw;
      my @comps = grep { parse_cond($_) } @parts;
      my @terms = map { [ parse_cond($_) ] } @comps;

      if (@terms > 1) {
        my $bool_op = $raw =~ /&&/ ? 'AND' : 'OR';
        for my $i (0 .. $#terms-1) {
          for my $j ($i+1 .. $#terms) {
            my ($v1,$o1,$x1) = @{ $terms[$i] };
            my ($v2,$o2,$x2) = @{ $terms[$j] };
            next unless defined $v1 && defined $v2 && $v1 eq $v2;

            if (implies($o1,$x1,$o2,$x2)) {
              my $msg = qq{"$comps[$j]" redundant in $bool_op with "$comps[$i]"};
              if ($do_sarif) {
                push @sarif_results, {
                  ruleId    => "boolean-redundancy",
                  level     => "warning",
                  message   => { text => $msg },
                  locations => [{
                    physicalLocation => {
                      artifactLocation => { uri => $file },
                      region           => { startLine => $ln }
                    }
                  }],
                };
              }
              else {
                printf "[%s:%d] %s\n", $file, $ln, $msg;
              }
            }
            elsif (implies($o2,$x2,$o1,$x1)) {
              my $msg = qq{"$comps[$i]" redundant in $bool_op with "$comps[$j]"};
              if ($do_sarif) {
                push @sarif_results, {
                  ruleId    => "boolean-redundancy",
                  level     => "warning",
                  message   => { text => $msg },
                  locations => [{
                    physicalLocation => {
                      artifactLocation => { uri => $file },
                      region           => { startLine => $ln }
                    }
                  }],
                };
              }
              else {
                printf "[%s:%d] %s\n", $file, $ln, $msg;
              }
            }
          }
        }
      }
    }

    #
    # 2) Nested-block redundancy
    #
    {
      my ($ov,$oo,$ox) = parse_cond($raw) || ();
      my $blk = $st->schild(2);
      next unless $blk && $blk->isa('PPI::Structure::Block');
      my $inners = $blk->find('PPI::Statement::Compound')||[];

      for my $in (@$inners) {
        my $t2 = $in->type;
        next unless $t2 =~ /^(?:if|elsif|unless|while|until)$/;
        my $c2 = $in->schild(1)->content;
        my $l2 = $in->line_number;
        my ($iv,$io,$ix) = parse_cond($c2) or next;
        next unless defined $ov && $iv eq $ov;

        # a) nested threshold
        if (implies($oo,$ox,$io,$ix)) {
          my $msg = qq{redundant "$c2" under "$raw"};
          if ($do_sarif) {
            push @sarif_results, {
              ruleId  => "nested-threshold",
              level   => "warning",
              message => { text => $msg },
              locations => [{
                physicalLocation => {
                  artifactLocation => { uri => $file },
                  region           => { startLine => $l2 }
                }
              }],
            };
          }
          else {
            printf "[%s:%d] %s\n", $file, $l2, $msg;
          }
          next;
        }

        # b) identical structure
        if ($type eq $t2 && $raw eq $c2) {
          my $msg = qq{nested identical $t2("$c2")};
          if ($do_sarif) {
            push @sarif_results, {
              ruleId  => "identical-structure",
              level   => "warning",
              message => { text => $msg },
              locations => [{
                physicalLocation => {
                  artifactLocation => { uri => $file },
                  region           => { startLine => $l2 }
                }
              }],
            };
          }
          else {
            printf "[%s:%d] %s\n", $file, $l2, $msg;
          }
          next;
        }

        # c) exact duplicate
        if ($raw eq $c2) {
          my $msg = qq{duplicate test "$c2"};
          if ($do_sarif) {
            push @sarif_results, {
              ruleId  => "duplicate-test",
              level   => "warning",
              message => { text => $msg },
              locations => [{
                physicalLocation => {
                  artifactLocation => { uri => $file },
                  region           => { startLine => $l2 }
                }
              }],
            };
          }
          else {
            printf "[%s:%d] %s\n", $file, $l2, $msg;
          }
          next;
        }

        # d) duplicate regex match
        if ($raw =~ /\Q\$$ov =~\E\s*(.+)$/ &&
            $c2  =~ /\Q\$$ov =~\E\s*\1/) {
          my $msg = qq{duplicate regex match $1};
          if ($do_sarif) {
            push @sarif_results, {
              ruleId  => "duplicate-regex",
              level   => "warning",
              message => { text => $msg },
              locations => [{
                physicalLocation => {
                  artifactLocation => { uri => $file },
                  region           => { startLine => $l2 }
                }
              }],
            };
          }
          else {
            printf "[%s:%d] %s\n", $file, $l2, $msg;
          }
        }
      }
    }
  }
}

#
# Emit SARIF if requested
#
if ($do_sarif) {
  my $sarif = {
    version => $SARIF_VERSION,
    $SARIF_SCHEMA ? ( $SARIF_SCHEMA => $SARIF_SCHEMA ) : (),
    runs => [
      {
        tool => {
          driver => {
            name => "Perl Redundancy Scanner",
            version => "1.0.0",
            rules   => \@RULE_DEFS,
          }
        },
        results => \@sarif_results,
      }
    ],
  };
  print JSON::MaybeXS->new( canonical => 1, pretty => 1 )
        ->encode($sarif);
}
