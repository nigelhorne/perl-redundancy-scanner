#!/usr/bin/env perl
use strict;
use warnings;
use PPI;
use Scalar::Util qw(blessed looks_like_number);
use JSON::MaybeXS;	# JSON::XS, Cpanel::JSON::XS or JSON fallback

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
  my ($opA,$vA,$opB,$vB) = map { '' . $_ } @_;  # stringify ops
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
	helpUri		 => '' },
  { id => "nested-threshold",
	shortDescription => { text => "Nested numeric threshold redundant" },
	helpUri		 => '' },
  { id => "identical-structure",
	shortDescription => { text => "Identical nested condition" },
	helpUri		 => '' },
  { id => "duplicate-test",
	shortDescription => { text => "Exact duplicate test" },
	helpUri		 => '' },
  { id => "duplicate-regex",
	shortDescription => { text => "Duplicate regex match" },
	helpUri		 => '' },
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

my %CONST;   # var → numeric or string constant

# find all simple assignments
for my $stmt ( @{ $doc->find('PPI::Statement') || [] } ) {
  my $src = $stmt->content;

  # my $x = 5;
  if ( $src =~ /(?:my|our)\s+\$(\w+)\s*=\s*([+-]?\d+)(?:\s*[;,])?/ ) {
	$CONST{$1} = 0 + $2;
	next;
  }

  # $y = 'foobar';
  if ( $src =~ /\$(\w+)\s*=\s*(['"])(.*?)\2/ ) {
	$CONST{$1} = $3;
	next;
  }

	# sub foo { return 10 }
	if ( $stmt->isa('PPI::Statement::Sub') ) {
		my $name  = $stmt->name or next;
		my $block = $stmt->block or next;              # guard here
		next unless $block->isa('PPI::Structure::Block');
		my $body  = $block->content;
		# trivial return literal?
		if ( $body =~ /\breturn\s+([+-]?\d+)\s*;/ ) {
			$CONST{"__SUB__$name"} = 0 + $1;
		}
	}
}

  # find all conditioning statements
  my $cmps = $doc->find('PPI::Statement::Compound') || [];
  for my $st (@$cmps) {
	my $type = $st->type;
	next unless $type =~ /^(?:if|elsif|unless|while|until)$/;

	my $raw = $st->schild(1)->content;		 # "(…)"
	my $ln  = $st->line_number;

	# replace foo() with its known constant if we saw
	#   sub foo { return 42 }
	# in the pre‐scan and stored $CONST{"__SUB__foo"} = 42.
	#
	$raw =~ s{
	  \b([A-Za-z_]\w*)	  # capture the sub name
	  \(\)				  # literal empty parens
	}{
	  exists $CONST{"__SUB__$1"}   # do we know a constant?
		? $CONST{"__SUB__$1"}	  # yes: substitute that number
		: "$1()"					# no: leave it alone
	}egx;

		# Try to parse a simple scalar comparison
	if ( my ($var,$op,$val) = parse_cond($raw) ) {
	  # did we see “my $var = constant” or “sub var(){ return const }”?
	  if ( exists $CONST{$var} ) {
		my $c = $CONST{$var};

		# always‐true?
		if ( $op =~ />=?/ && $c >= $val ) {
		  printf "[%s:%d] always-true test \"%s %s %s\" (we know \$%s == %d)\n",
				 $file, $ln, "\$$var", $op, $val, $var, $c;
		  next;   # skip other heuristics for this st
		}
		# always-false?
		elsif ( $op =~ /<=?/ && $c <= $val ) {
		  printf "[%s:%d] always-false test \"%s %s %s\" (we know \$%s == %d)\n",
				 $file, $ln, "\$$var", $op, $val, $var, $c;
		  next;
		}
	  }
	}

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
				  ruleId	=> "boolean-redundancy",
				  level	 => "warning",
				  message   => { text => $msg },
				  locations => [{
					physicalLocation => {
					  artifactLocation => { uri => $file },
					  region		   => { startLine => $ln }
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
				  ruleId	=> "boolean-redundancy",
				  level	 => "warning",
				  message   => { text => $msg },
				  locations => [{
					physicalLocation => {
					  artifactLocation => { uri => $file },
					  region		   => { startLine => $ln }
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
				  region		   => { startLine => $l2 }
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
				  region		   => { startLine => $l2 }
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
				  region		   => { startLine => $l2 }
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
				  region		   => { startLine => $l2 }
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

	# Redundant elsif chain
		# For each `if`, gather its following `elsif` siblings and compare.
	#
	my $cmps = $doc->find('PPI::Statement::Compound') || [];
	for my $st (@$cmps) {
		next unless $st->type eq 'if';
		my $chain = [ $st ];
		# walk siblings at the same level
		my $sib = $st->snext_sibling;
		while ($sib) {
			if ($sib->isa('PPI::Statement::Compound')
			 && $sib->type eq 'elsif')
			{
				push @$chain, $sib;
				$sib = $sib->snext_sibling;
				next;
			}
			last;
		}
		next unless @$chain > 1;

		# parse and compare head ⇒ each elsif
		my ($v0,$o0,$x0) = parse_cond( $chain->[0]->schild(1)->content ) or next;
		for my $i (1..$#$chain) {
			my $el = $chain->[$i];
			my $raw = $el->schild(1)->content;
			my ($v1,$o1,$x1) = parse_cond($raw) or next;
			next unless $v0 eq $v1;
			if (implies($o0,$x0,$o1,$x1)) {
				printf "[%s:%d] redundant elsif “%s” implied by “%s”\n",
				  $file, $el->line_number, $raw, $chain->[0]->schild(1)->content;
			}
		}
	}

	# Clone detection
	  # Collect all block‐texts, bucket by trimmed content, then report duplicates.
	#
	my %buckets;
	my $blocks = $doc->find('PPI::Structure::Block') || [];
	for my $blk (@$blocks) {
		next unless ref $blk;			# just in case
		my $t = $blk->content;
		$t =~ s/^\s*\{//;   $t =~ s/\}\s*$//;
		$t =~ s/^\s+|\s+$//g;
		next unless length $t > 40;   # only sizable blocks
		push @{ $buckets{$t} }, $blk->line_number;
	}
	for my $txt (keys %buckets) {
		my $lines = $buckets{$txt};
		next unless @$lines > 1;
		my $nlines = ($txt =~ tr/\n//) + 1;
		next if $nlines <= 5;	# skip small blocks - TODO: make configurable

		my @sorted = sort { $a <=> $b } @$lines;
		printf "[%s] clone of %d-line block at lines %s\n",
			$file, $nlines, join(", ", @sorted);
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
