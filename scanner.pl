#!/usr/bin/env perl
use strict;
use warnings;
use PPI;
use Scalar::Util qw(blessed looks_like_number);
use JSON::MaybeXS;    # JSON::XS, Cpanel::JSON::XS or JSON fallback

#------------------------------------------------------------------------------
# SARIF definitions
#------------------------------------------------------------------------------
my $SARIF_VERSION = "2.1.0";
my $SARIF_SCHEMA  = "https://schemastore.azurewebsites.net/schemas/json/sarif-2.1.0.json";

my @RULE_DEFS = (
  { id => "boolean-redundancy",
    shortDescription => { text => "Redundant comparison in boolean expression" } },
  { id => "nested-threshold",
    shortDescription => { text => "Nested numeric threshold redundant" } },
  { id => "identical-structure",
    shortDescription => { text => "Identical nested condition" } },
  { id => "duplicate-test",
    shortDescription => { text => "Exact duplicate test" } },
  { id => "duplicate-regex",
    shortDescription => { text => "Duplicate regex match" } },
  { id => "elsif-redundancy",
    shortDescription => { text => "Redundant elsif implied by earlier condition" } },
  { id => "always-true-test",
    shortDescription => { text => "Condition always true based on known constant" } },
  { id => "always-false-test",
    shortDescription => { text => "Condition always false based on known constant" } },
  { id => "clone-detection",
    shortDescription => { text => "Duplicate code block" } },
);

my @sarif_results;

#------------------------------------------------------------------------------
# Helpers
#------------------------------------------------------------------------------

# Turn DateTime or numeric‐looking scalar into a plain number
sub coerce_number {
  my ($v) = @_;
  if (blessed($v) && $v->isa('DateTime')) {
    return $v->epoch;
  }
  return 0 + $v           if looks_like_number($v);
  return int($v)          if $v =~ /^\s*([+-]?\d+)\s*$/;
  return;
}

# Parse “$var op value” or “value op $var” → ($var, $op, $value)
sub parse_cond {
  my $s = shift;
  $s =~ s/^\s*\(//;  $s =~ s/\)\s*$//;
  if ($s =~ /^\s*\$(\w+)\s*(>=|>|==|<=|<|eq|ne)\s*(.+?)\s*$/) {
    return ($1, $2, $3);
  }
  if ($s =~ /^\s*(.+?)\s*(>=|>|==|<=|<|eq|ne)\s*\$(\w+)\s*$/) {
    my ($val, $op, $var) = ($1, $2, $3);
    my %flip = (
      '<'  => '>', '<=' => '>=',
      '>'  => '<', '>=' => '<=',
      '==' => '==','eq' => 'eq','ne'=>'ne'
    );
    return ($var, $flip{$op}, $val);
  }
  return;
}

# ∀x: (x opA vA) ⇒ (x opB vB)?
sub implies {
  my ($oA,$vA,$oB,$vB) = map { "" . $_ } @_;
  return unless $oA =~ /^(?:>=?|<=?|==|eq|ne)$/ && $oB =~ /^(?:>=?|<=?|==|eq|ne)$/;
  return unless defined( my $nA = coerce_number($vA) )
             && defined( my $nB = coerce_number($vB) );

  no warnings 'numeric';
  if ($oA =~ /^>=?$/ && $oB =~ /^>=?$/) { return $nA >= $nB }
  if ($oA =~ /^<=?$/ && $oB =~ /^<=?$/) { return $nA <= $nB }
  use warnings 'numeric';
  return;
}

#------------------------------------------------------------------------------
# Entry point
#------------------------------------------------------------------------------
my $do_sarif = 0;
if (@ARGV && $ARGV[0] eq '--sarif') {
  shift @ARGV;
  $do_sarif = 1;
}
die "Usage: $0 [--sarif] file1.pl [file2.pl ...]\n" unless @ARGV;

for my $file (@ARGV) {
  print "\nScanning $file …\n";
  my $doc = PPI::Document->new($file)
    or warn "✗ Failed to parse $file: $@\n" and next;

  #
  # Pre-scan: collect simple constants and trivial subs
  #
  my %CONST;
  # scalar assignments
  for my $assign ( @{ $doc->find('PPI::Statement') || [] } ) {
    # guard just in case
    next unless ref $assign
             && blessed($assign)
             && $assign->can('content');

    my $src = $assign->content;
    if ($src =~ /(?:my|our)\s+\$(\w+)\s*=\s*([+-]?\d+)(?:\s*[;,])?/) {
      $CONST{$1} = 0 + $2;
      next;
    }
    if ($src =~ /\$(\w+)\s*=\s*(['"])(.*?)\2/) {
      $CONST{$1} = $3;
      next;
    }
  }
  # trivial subs returning literal
  my $subs = $doc->find('PPI::Statement::Sub') || [];
	for my $snode (@$subs) {
    # optional extra guard
    next unless blessed($snode) && $snode->isa('PPI::Statement::Sub');

    my $name = $snode->name or next;   # safe now
    my $block = $snode->block or next;
    next unless $block->isa('PPI::Structure::Block');
    my $body = $block->content;
    if ($body =~ /\breturn\s+([+-]?\d+)\s*;/) {
      $CONST{"__SUB__$name"} = 0 + $1;
    }
  }

  #
  # Main scans: per‐statement heuristics
  #
  my $cmps = $doc->find('PPI::Statement::Compound') || [];
  for my $st (@$cmps) {
    my $type = $st->type;
    next unless $type =~ /^(?:if|elsif|unless|while|until)$/;
    my $raw = $st->schild(1)->content;  
    my $ln  = $st->line_number;

    # Inline known sub calls: foo() → constant
    $raw =~ s{\b([A-Za-z_]\w*)\(\)}{
      exists $CONST{"__SUB__$1"} ? $CONST{"__SUB__$1"} : "$1()"
    }eg;

    #
    # 1) ALWAYS‐TRUE / ALWAYS‐FALSE tests
    #
    if ( my ($var,$op,$val) = parse_cond($raw) ) {
      if ( exists $CONST{$var} ) {
        my $c = $CONST{$var};
        if ($op =~ />=?/ && $c >= $val) {
          my $msg = qq{always-true test "\$$var $op $val" (known \$$var == $c)};
          if ($do_sarif) {
            push @sarif_results, {
              ruleId    => "always-true-test",
              level     => "warning",
              message   => { text => $msg },
              locations => [{ physicalLocation => {
                artifactLocation => { uri => $file },
                region           => { startLine => $ln },
              }}],
            };
          } else {
            printf "[%s:%d] %s\n", $file, $ln, $msg;
          }
          next;
        }
        elsif ($op =~ /<=?/ && $c <= $val) {
          my $msg = qq{always-false test "\$$var $op $val" (known \$$var == $c)};
          if ($do_sarif) {
            push @sarif_results, {
              ruleId    => "always-false-test",
              level     => "warning",
              message   => { text => $msg },
              locations => [{ physicalLocation => {
                artifactLocation => { uri => $file },
                region           => { startLine => $ln },
              }}],
            };
          } else {
            printf "[%s:%d] %s\n", $file, $ln, $msg;
          }
          next;
        }
      }
    }

    #
    # 2) BOOLEAN‐AND/OR redundancy
    #
    {
      my @parts = map { s/^\s+|\s+$//gr } split /(?:&&|\|\|)/, $raw;
      my @comps = grep { parse_cond($_) } @parts;
      my @terms = map { [ parse_cond($_) ] } @comps;
      if (@terms > 1) {
        my $op = $raw =~ /&&/ ? 'AND' : 'OR';
        for my $i (0 .. $#terms-1) {
          for my $j ($i+1 .. $#terms) {
            my ($v1,$o1,$x1) = @{ $terms[$i] };
            my ($v2,$o2,$x2) = @{ $terms[$j] };
            next unless $v1 && $v1 eq $v2;
            if (implies($o1,$x1,$o2,$x2)) {
              my $msg = qq{"$comps[$j]" redundant in $op with "$comps[$i]"};
              if ($do_sarif) {
                push @sarif_results, {
                  ruleId    => "boolean-redundancy",
                  level     => "warning",
                  message   => { text => $msg },
                  locations => [{ physicalLocation => {
                    artifactLocation => { uri => $file },
                    region           => { startLine => $ln },
                  }}],
                };
              } else {
                printf "[%s:%d] %s\n", $file, $ln, $msg;
              }
            }
            elsif (implies($o2,$x2,$o1,$x1)) {
              my $msg = qq{"$comps[$i]" redundant in $op with "$comps[$j]"};
              if ($do_sarif) {
                push @sarif_results, {
                  ruleId    => "boolean-redundancy",
                  level     => "warning",
                  message   => { text => $msg },
                  locations => [{ physicalLocation => {
                    artifactLocation => { uri => $file },
                    region           => { startLine => $ln },
                  }}],
                };
              } else {
                printf "[%s:%d] %s\n", $file, $ln, $msg;
              }
            }
          }
        }
      }
    }

    #
    # 3) NESTED‐BLOCK redundancy
    #
    {
      my ($ov,$oo,$ox) = parse_cond($raw);
      my $blk = $st->schild(2);
      next unless $blk && $blk->isa('PPI::Structure::Block');
      my $inners = $blk->find('PPI::Statement::Compound') || [];

      # normalize outer cond text
      (my $outer = $raw) =~ s/^\s*\(//;  $outer =~ s/\)\s*$//;  
      $outer =~ s/^\s+|\s+$//g;

      for my $in (@$inners) {
        next unless $in->type =~ /^(?:if|elsif|unless|while|until)$/;
        my $c2 = $in->schild(1)->content;
        my $l2 = $in->line_number;
        # normalize inner cond
        (my $inner = $c2) =~ s/^\s*\(//;  $inner =~ s/\)\s*$//;  
        $inner =~ s/^\s+|\s+$//g;

        # a) numeric/string comparisons
        if ( my ($iv,$io,$ix) = parse_cond($c2) ) {
          next unless defined $ov && $iv eq $ov;
          if (implies($oo,$ox,$io,$ix)) {
            my $msg = qq{redundant "$inner" under "$outer"};
            if ($do_sarif) {
              push @sarif_results, {
                ruleId    => "nested-threshold",
                level     => "warning",
                message   => { text => $msg },
                locations => [{ physicalLocation => {
                  artifactLocation => { uri => $file },
                  region           => { startLine => $l2 },
                }}],
              };
            } else {
              printf "[%s:%d] %s\n", $file, $l2, $msg;
            }
            next;
          }
          if ($type eq $in->type && $inner eq $outer) {
            my $msg = qq{nested identical $type($inner)};
            if ($do_sarif) {
              push @sarif_results, {
                ruleId    => "identical-structure",
                level     => "warning",
                message   => { text => $msg },
                locations => [{ physicalLocation => {
                  artifactLocation => { uri => $file },
                  region           => { startLine => $l2 },
                }}],
              };
            } else {
              printf "[%s:%d] %s\n", $file, $l2, $msg;
            }
            next;
          }
          if ($inner eq $outer) {
            my $msg = qq{duplicate test "$inner"};
            if ($do_sarif) {
              push @sarif_results, {
                ruleId    => "duplicate-test",
                level     => "warning",
                message   => { text => $msg },
                locations => [{ physicalLocation => {
                  artifactLocation => { uri => $file },
                  region           => { startLine => $l2 },
                }}],
              };
            } else {
              printf "[%s:%d] %s\n", $file, $l2, $msg;
            }
            next;
          }
        }
        # d) duplicate regex match
	if ( $raw =~ /^\s*\$(\w+)\s*=\~\s*(\/.+?\/)/ ) {
		my ($var,$pat) = ($1,$2);
		if ( $c2 =~ /^\s*\Q\$$var\E\s*=\~\s*\Q$pat\E/ ) {
			# strip the slashes for display
			my $msg = qq{duplicate regex match $pat};
			( my $inner = $pat ) =~ s{^/|/$}{}g;
			if ($do_sarif) {
				push @sarif_results, {
					ruleId    => "duplicate-regex",
					level     => "warning",
					message   => { text => $msg },
					locations => [{ physicalLocation => {
					artifactLocation => { uri => $file },
					region           => { startLine => $l2 },
				}}],
				};
			} else {
				printf "[%s:%d] duplicate regex match %s\n",
					$file, $l2, $inner;
			}
		}
	      }
	   }
    }

    #
    # 4) REDUNDANT-ELSEIF chain
    #
    if ($type eq 'if') {
      my @chain = ($st);
      my $sib = $st->snext_sibling;
      while ($sib && $sib->isa('PPI::Statement::Compound') && $sib->type eq 'elsif') {
        push @chain, $sib;
        $sib = $sib->snext_sibling;
      }
      if (@chain > 1) {
        my ($v0,$o0,$x0) = parse_cond($chain[0]->schild(1)->content) or ();
        for my $el (@chain[1..$#chain]) {
          my $raw2 = $el->schild(1)->content;
          my $ln2   = $el->line_number;
          if (my ($v1,$o1,$x1) = parse_cond($raw2)) {
            next unless $v0 && $v0 eq $v1;
            if (implies($o0,$x0,$o1,$x1)) {
              my $msg = qq{redundant elsif "$raw2" implied by "$chain[0]->schild(1)->content"};
              if ($do_sarif) {
                push @sarif_results, {
                  ruleId    => "elsif-redundancy",
                  level     => "warning",
                  message   => { text => $msg },
                  locations => [{ physicalLocation => {
                    artifactLocation => { uri => $file },
                    region           => { startLine => $ln2 },
                  }}],
                };
              } else {
                printf "[%s:%d] %s\n", $file, $ln2, $msg;
              }
            }
          }
        }
      }
    }
  }

  #
  # 5) CLONE DETECTION (once per file)
  #
  {
    my $blocks = $doc->find('PPI::Structure::Block') || [];
    my %buckets;
    for my $blk (@$blocks) {
      next unless $blk->isa('PPI::Structure::Block');
      my $t = $blk->content;
      $t =~ s/^\s*\{//;
      $t =~ s/\}\s*$//;
      $t =~ s/^\s+|\s+$//g;
      my $nlines = ($t =~ tr/\n//) + 1;
      next if $nlines <= 2 || length($t) < 40;
      push @{ $buckets{$t} }, $blk->line_number;
    }
    for my $txt (keys %buckets) {
      my $lines = $buckets{$txt};
      next unless @$lines > 1;
      my @sorted = sort { $a <=> $b } @$lines;
      my $msg = sprintf "clone of %d-line block at lines %s",
                        (($txt =~ tr/\n//)+1), join(", ", @sorted);
      if ($do_sarif) {
        push @sarif_results, {
          ruleId    => "clone-detection",
          level     => "warning",
          message   => { text => $msg },
          locations => [{ physicalLocation => {
            artifactLocation => { uri => $file },
            region           => { startLine => $sorted[0] },
          }}],
        };
      } else {
        printf "[%s] %s\n", $file, $msg;
      }
    }
  }
}

#------------------------------------------------------------------------------
# Emit SARIF if requested
#------------------------------------------------------------------------------
if ($do_sarif) {
  my $sarif = {
    version => $SARIF_VERSION,
    $SARIF_SCHEMA ? ( $SARIF_SCHEMA => $SARIF_SCHEMA ) : (),
    runs => [
      {
        tool => {
          driver => {
            name    => "Perl Redundancy Scanner",
            version => "1.0.0",
            rules   => \@RULE_DEFS,
          }
        },
        results => \@sarif_results,
      }
    ],
  };
  print JSON::MaybeXS->new( canonical => 1, pretty => 1 )->encode($sarif);
}
