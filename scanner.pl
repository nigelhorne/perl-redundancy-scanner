#!/usr/bin/env perl

use strict;
use warnings;
use PPI;
use Scalar::Util qw(blessed looks_like_number);
use JSON::MaybeXS;	# JSON::XS, Cpanel::JSON::XS or JSON fallback

#------------------------------------------------------------------------------
# SARIF definitions
#------------------------------------------------------------------------------
my $SARIF_VERSION = '2.1.0';
my $SARIF_SCHEMA = 'https://schemastore.azurewebsites.net/schemas/json/sarif-2.1.0.json';

my @RULE_DEFS = (
	{ id => 'boolean-redundancy',   shortDescription => { text => 'Redundant comparison in boolean expression' } },
	{ id => 'nested-threshold',     shortDescription => { text => 'Nested numeric threshold redundant' } },
	{ id => 'identical-structure',  shortDescription => { text => 'Identical nested condition' } },
	{ id => 'duplicate-test',       shortDescription => { text => 'Exact duplicate test' } },
	{ id => 'duplicate-regex',      shortDescription => { text => 'Duplicate regex match' } },
	{ id => 'elsif-redundancy',     shortDescription => { text => 'Redundant elsif implied by earlier condition' } },
	{ id => 'always-true-test',     shortDescription => { text => 'Condition always true based on known constant' } },
	{ id => 'always-false-test',    shortDescription => { text => 'Condition always false based on known constant' } },
	{ id => 'clone-detection',      shortDescription => { text => 'Duplicate code block' } },
	{ id => 'complement-with-else',    shortDescription => { text => 'Redundant elsif - complement with else present' } },
	{ id => 'adjacent-complements',    shortDescription => { text => 'Adjacent mutually exclusive conditions' } },
	{ id => 'complement-conditions',   shortDescription => { text => 'Complement condition detected' } },
	{ id => 'exhaustive-without-else', shortDescription => { text => 'Exhaustive conditions should use else clause' } },
);

my $do_sarif = 0;
my @sarif_results;

#------------------------------------------------------------------------------
# Helpers
#------------------------------------------------------------------------------

sub coerce_number {
	my $v = shift;

	return $v->epoch if blessed($v) && $v->isa('DateTime');
	return 0 + $v if looks_like_number($v);
	return int($v) if defined $v && $v =~ /^\s*([+-]?\d+)\s*$/;
	return;
}

sub parse_cond {
	my $s = shift;

	$s =~ s/^\s*\(//;
	$s =~ s/\)\s*$//;
	if($s =~ /^\s*\$(\w+)\s*(>=|>|==|<=|<|eq|ne|=~|!~)\s*(.+?)\s*$/) {
		return ($1, $2, $3);
	}
	if($s =~ /^\s*(.+?)\s*(>=|>|==|<=|<|eq|ne)\s*\$(\w+)\s*$/) {
		my ($val, $op, $var) = ($1, $2, $3);
		my %flip = ( '<'=>'>', '<='=>'>=', '>' =>'<','>='=>'<=', '=='=>'==','eq'=>'eq','ne'=>'ne' );
		return ($var, $flip{$op}, $val);
	}
	return;
}

# Determine whether one numeric threshold condition implies another
#
# This function checks if Condition 1 ($var OP1 X1) logically implies
# Condition 0 ($var OP0 X0), meaning that whenever Condition 1 is true,
# Condition 0 must also be true.
#
# Parameters:
#   $op1 : Operator from Condition 1 (must be '>', '>=', '<', or '<=')
#   $x1  : Threshold value from Condition 1 (must be numeric)
#   $op0 : Operator from Condition 0 (must be '>', '>=', '<', or '<=')
#   $x0  : Threshold value from Condition 0 (must be numeric)
#
# Returns:
#   1 if Condition 1 implies Condition 0 (all values satisfying Condition 1
#      also satisfy Condition 0)
#   0 if implication doesn't hold, or if invalid/non-numeric input is detected
#
# Logic Overview:
#   1. Only handles threshold operators (>, >=, <, <=) - returns 0 for others
#   2. Requires both conditions to test the same variable
#   3. Requires both conditions to have the same direction (> vs <)
#   4. Compares threshold values according to operator strictness:
#
#   For '>' direction:
#     Condition 1  | Condition 0  | Implication holds when
#     -------------|--------------|--------------------------
#        $var > x1 | $var > x0   | x1 >= x0
#        $var > x1 | $var >= x0  | x1 >= x0
#        $var >= x1| $var > x0   | x1 > x0
#        $var >= x1| $var >= x0  | x1 >= x0
#
#   For '<' direction:
#     Condition 1  | Condition 0  | Implication holds when
#     -------------|--------------|--------------------------
#        $var < x1 | $var < x0   | x1 <= x0
#        $var < x1 | $var <= x0  | x1 <= x0
#        $var <= x1| $var < x0   | x1 < x0
#        $var <= x1| $var <= x0  | x1 <= x0
#
# Examples:
#   implies('>', 5, '>', 4)   # 1 (v>5 ⇒ v>4)
#   implies('>=', 5, '>', 5)   # 0 (v=5 satisfies ≥5 but not >5)
#   implies('<', 3, '<=', 3)   # 1 (v<3 ⇒ v≤3)
#   implies('>', 5, '<', 4)    # 0 (different directions)
#   implies('==', 5, '>', 4)   # 0 (equality not handled)

sub implies
{
	my ($op1, $x1, $op0, $x0) = @_;

	if(($op0 eq '=~') && ($op1 eq '=~')) {
		return $x0 eq $x1;
	}

	if(($op0 eq '!~') && ($op1 eq '!~')) {
		return $x0 eq $x1;
	}

	# Return 0 if values aren't numeric
	return 0 unless looks_like_number($x1) && looks_like_number($x0);

	if($op0 eq '==' && $op1 eq '==') { return $x1 == $x0 };

	# Skip non-relational operators (==, !=, eq, ne)
	return 0 if $op1 =~ /^(?:==|!=|eq|ne)$/ || $op0 =~ /^(?:==|!=|eq|ne)$/;

	# Get operator directions
	my $dir1 = ($op1 =~ /^([<>])/)[0];
	my $dir0 = ($op0 =~ /^([<>])/)[0];

	# Different directions can't imply
	return 0 if $dir1 ne $dir0;

	# For '>' direction
	if ($dir1 eq '>') {
		if ($op1 eq '>') {
			if ($op0 eq '>')	{ return $x1 >= $x0 }
			elsif ($op0 eq '>=')	{ return $x1 >= $x0 }
		} elsif ($op1 eq '>=') {
			if ($op0 eq '>')	{ return $x1 > $x0 }
			elsif ($op0 eq '>=')	{ return $x1 >= $x0 }
		}
	}
	# For '<' direction
	elsif ($dir1 eq '<') {
		if ($op1 eq '<') {
			if ($op0 eq '<')	{ return $x1 <= $x0 }
			elsif ($op0 eq '<=')	{ return $x1 <= $x0 }
		} elsif ($op1 eq '<=') {
			if ($op0 eq '<')	{ return $x1 < $x0 }
			elsif ($op0 eq '<=')	{ return $x1 <= $x0 }
		}
	}

	return 0;
}

sub _emit {
	my ($rule, $msg, $file, $ln) = @_;

	if($do_sarif) {
		push @sarif_results, {
			ruleId => $rule,
			level => 'warning',
			message => { text => $msg },
			locations => [{
				physicalLocation => {
					artifactLocation => { uri => $file },
					region => { startLine => $ln   },
				}
			}],
		};
	} else {
		printf "[%s:%d] %s\n", $file, $ln, $msg;
	}
}

#------------------------------------------------------------------------------
# Entry point
#------------------------------------------------------------------------------
if (@ARGV && $ARGV[0] eq '--sarif') {
	shift @ARGV;
	$do_sarif = 1;
}

die "Usage: $0 [--sarif] file1.pl [file2.pl ...]" unless @ARGV;

for my $file (@ARGV) {
	if(-d $file) {
		print "\nNot scanning directory $file …\n";
		next;
	}
	print "\nScanning $file …\n";
	my $doc = PPI::Document->new($file) or warn "✗ Failed to parse $file: $PPI::Document::errstr" and next;

	# Pre-scan constants
	my %CONST;
	for my $assign (@{ $doc->find('PPI::Statement') || [] }) {
		next unless blessed($assign) && $assign->can('content');
		my $src = $assign->content;
		if ($src =~ /(?:my|our)\s+\$(\w+)\s*=\s*([+-]?\d+)\s*;/) {
			$CONST{$1} = 0 + $2;
			next;
		}
		if ($src =~ /\$(\w+)\s*=\s*(['"])(.*?)\2/) {
			$CONST{$1} = $3;
			next;
		}
	}
	for my $snode (@{ $doc->find('PPI::Statement::Sub') || [] }) {
		next unless blessed($snode) && $snode->isa('PPI::Statement::Sub');
		my $name = $snode->name  or next;
		my $block = $snode->block or next;
		next unless blessed($block) && $block->isa('PPI::Structure::Block');

		my $body  = $block->content;
		if($body =~ /\breturn\s+([+-]?\d+)\b/) {
			$CONST{"__SUB__$name"} = 0 + $1;
		}
	}

	# print __LINE__, ': ', Data::Dumper->new([\%CONST])->Dump();

	# Main loop: compound statements
	my $cmps = $doc->find('PPI::Statement::Compound') || [];
	# Find all the "if" statements that start an if–elsif chain

	# Now iterate by index so we can tell who’s a chain head
	CMP: for my $i (0 .. $#$cmps) {
		my $st = $cmps->[$i];
		# print __LINE__, ': ', $st->type(), "\n";
		# print __LINE__, ': ', $st->content(), "\n";
		if($st->type eq 'while') {
			my $blks = $st->find('PPI::Structure::Block') || [];
			foreach my $blk (@{$blks}) {
				# print Data::Dumper->new([$blk])->Dump();
				my @inners = grep {
					blessed($_)
					&& $_->isa('PPI::Statement')
				} $blk->children;
				# print Data::Dumper->new([\@inners])->Dump();

				# FIXME: only goes in one level
				for my $in (@inners) {
					my @statements = $in->children;
					if(scalar(@statements) == 3) {
						my $op = $in->child(1)->content();
						if($op =~ /(?:--|\+\+)/) {
							# Found a variable that is being changed
							my $lhs = $in->child(0)->content();
							$lhs =~ s/^\$//;
							# print "$lhs $op\n";
							delete $CONST{$lhs};
						}
						# TODO: assignment operator
					}
				}
			}
			next;
		}
		next unless blessed($st)
			&& $st->can('schild')
			&& $st->type =~ /^(?:if|elsif|unless|while|until)$/;

		my $cond_node = $st->schild(1);
		next unless blessed($cond_node) && $cond_node->can('content');
		my $raw = $cond_node->content;
		my $ln  = $st->line_number;

		#
		# STEP 0: if+elsif chain handling via PPI::Structure::Condition
		#
		# RIGHT: only skip when you actually have an if–elsif chain
		if($st->type eq 'if') {
			my $conds = $st->find('PPI::Structure::Condition') || [];

			# print __LINE__, ': ', @$conds, "\n";
			# only handle >1 conditions (i.e. if + at least one elsif)
			if(@$conds > 1) {
				# if($conds->[0]->content() eq $conds->[1]->content()) {
					# _emit('nested-if-redundancy',
						# qq{redundant duplicate if "}
						# . $conds->[0]->content()
						# . qq{"},
						# $file, $conds->[1]->line_number()
					# );
					# next CMP;
				# }
				detect_negated_condition_redundancy($st, $file);
				# print __LINE__, ': ', $conds->[0]->content(), "\n";
				my ($v0,$o0,$x0) = parse_cond($conds->[0]->content) or next CMP;
				# print __LINE__, ": $v0 - $o0 - $x0\n";
				for my $cn (@$conds[1..$#$conds]) {
					my $raw2 = $cn->content;
					my $ln2  = $cn->line_number;
					my ($v1,$o1,$x1) = parse_cond($raw2) or next;
					# print __LINE__, ": $o0 - $x0 - $o1 - $x1\n";
					next unless $v1 eq $v0;
					# check head ⇒ branch (not the other way around)
					if(implies($o0,$x0,$o1,$x1)) {
						if($raw2 eq $conds->[0]->content()) {
							_emit('if-redundancy',
							      qq{nested identical if "$raw2" implied by "}
								. $conds->[0]->content
								. qq{"},
							      $file, $ln2);
						} else {
							_emit('elsif-redundancy',
							      qq{redundant elsif "$raw2" implied by "}
								. $conds->[0]->content
								. qq{"},
							      $file, $ln2);
						}
					}
				}
				# print "next CMP\n";
				# next CMP;
			}
		}

		# ————————————————————————————————————————————————————————————————————————
		# STEP 1: always-true / always-false detection for standalone statements
		# ————————————————————————————————————————————————————————————————————————
		{
			# print "Step 1: $raw\n";

			# A) strip outer parens
			(my $expr = $raw) =~ s/^\s*[(\[]\s*//;
			$expr =~ s/\s*[)\]]\s*$//;

			# B) subcall() OP literal
			if ($expr =~ /^\s*([A-Za-z_]\w*)\(\)\s*
				(==|eq|!=|ne|>=|<=|>|<)\s*
				([+-]?\d+)\s*$/x) {
				my ($sub,$op,$lit) = ($1,$2,$3);

				next unless exists $CONST{"__SUB__$sub"};
				my $lhs_txt = "$sub()";
				my $lhs_val = $CONST{"__SUB__$sub"};
				my $true =
					$op eq '=='||$op eq 'eq'?($lhs_val == $lit):
					$op eq '!='||$op eq 'ne'?($lhs_val != $lit):
					$op eq '>'             ?($lhs_val >  $lit):
					$op eq '>='            ?($lhs_val >= $lit):
					$op eq '<'             ?($lhs_val <  $lit):
					$op eq '<='            ?($lhs_val <= $lit):0;
				my $kind = $true?"always-true-test":"always-false-test";
				my $msg  = qq{always-} . ($true?"true":"false")
					. qq{ test "$lhs_txt $op $lit"};
					_emit($kind,$msg,$file,$ln);
				next CMP;
			}

			# C) inline remaining subs
			(my $cmp = $expr) =~ s{\b([A-Za-z_]\w*)\(\)}
			{ exists $CONST{"__SUB__$1"} ? $CONST{"__SUB__$1"} : "$1()" }eg;

			# (D) $var OP literal
			if ($cmp =~ /^\s*\$(\w+)\s*
			   (==|eq|!=|ne|>=|<=|>|<)\s*
			   ([+-]?\d+)\s*$/x)
			{
				my ($v,$op,$lit) = ($1,$2,$3);

				# <<< new guard: only if we really have a defined constant >>>
				next unless exists $CONST{$v} && defined $CONST{$v};

				my $lhs_txt = "\$$v";
				my $lhs_val = $CONST{$v};
				# Prevent 'Argument "0$month" isn't numeric in numeric le (<=)'
				if(looks_like_number($lhs_val))  {
					my $true =
					$op eq '==' || $op eq 'eq' ? ($lhs_val == $lit)
						: $op eq '!=' || $op eq 'ne' ? ($lhs_val != $lit)
						: $op eq '>'                ? ($lhs_val >  $lit)
						: $op eq '>='               ? ($lhs_val >= $lit)
						: $op eq '<'                ? ($lhs_val <  $lit)
						: $op eq '<='               ? ($lhs_val <= $lit)
						:                              0;
					my $kind = $true ? "always-true-test" : "always-false-test";
					my $msg  = qq{always-} . ($true ? "true" : "false")
						. qq{ test "$lhs_txt $op $lit"};
						_emit($kind, $msg, $file, $ln);
				} else {
					delete $CONST{$v};
				}
				next CMP;
			}

			# anything else (e.g. @INC > 0) falls through
		}

		#
		# STEP 2: boolean AND/OR redundancy
		#
		{
			# print "Step 2: $raw\n";

			my @c = split /(?:&&|\|\|)/, $raw;
			my (@comps, @terms);

			for my $p (@c) {
				(my $t = $p) =~ s/^\s*\(//;  $t =~ s/\)\s*$//;
				$t =~ s/^\s+|\s+$//g;
				if(my @pp = parse_cond($t)) {
					push @comps, $t;
					push @terms, [ @pp ];
				}
			}
			if(@terms > 1) {
				my $bool_op = $raw =~ /&&/ ? 'AND' : $raw =~ /\|\|/ ? 'OR' : undef;
				next unless $bool_op; # Skip if neither found

				for my $i (0 .. $#terms-1) {
					for my $j ($i+1 .. $#terms) {
						my ($v1,$o1,$x1) = @{ $terms[$i] };
						my ($v2,$o2,$x2) = @{ $terms[$j] };
						next unless $v1 eq $v2;
						next if $o1 eq '==' || $o2 eq '==';   # no implication between equalities

						if(implies($o1,$x1,$o2,$x2)) {
							_emit("boolean-redundancy",
								qq{"$comps[$j]" redundant in $bool_op with "$comps[$i]"},
								$file, $ln);
						} elsif (implies($o2,$x2,$o1,$x1)) {
							_emit("boolean-redundancy",
								qq{"$comps[$i]" redundant in $bool_op with "$comps[$j]"},
								$file, $ln);
						}
					}
				}
			}
		}

		# STEP 3: nested-block redundancy
		{
			# print "Step 3: $raw\n";

			my ($ov, $oo, $ox) = parse_cond($raw);
			my ($blk) = $st->find_first('PPI::Structure::Block');
			next unless $blk && blessed($blk) && $blk->isa('PPI::Structure::Block');

			# print __LINE__, "\n";
			# Get direct child statements inside block that are if/elsif/unless/while/until
			my @inners = grep {
				blessed($_)
				&& $_->isa('PPI::Statement')
				&& $_->schild(0)
				&& $_->schild(0)->content =~ /^(if|elsif|unless|while|until)$/
			} $blk->schildren;

			# print __LINE__, ": $raw\n", scalar(@inners), "\n";  # Debug print
			# print Data::Dumper->new([$blk])->Dump();

			(my $outer = $raw) =~ s/^\s*\(//;  $outer =~ s/\)\s*$//;  $outer =~ s/^\s+|\s+$//g;
			# print __LINE__, ": $outer\n";

			for my $in (@inners) {
				# print __LINE__, "\n";
				my $c2 = $in->schild(1)->content;
				my $l2 = $in->line_number;
				(my $inner = $c2) =~ s/^\s*\(//;  $inner =~ s/\)\s*$//;  $inner =~ s/^\s+|\s+$//g;

				if (my ($iv, $io, $ix) = parse_cond($c2)) {
					next unless defined $ov && $iv eq $ov;

					if (implies($oo, $ox, $io, $ix)) {
						_emit("nested-threshold",
							qq{redundant "$inner" under "$outer"},
							$file, $l2);
						next;
					}
					if ($st->type eq $in->type && $inner eq $outer) {
						_emit("identical-structure",
							qq{nested identical $in->type($inner)},
							$file, $l2);
						next;
					}
					if ($inner eq $outer) {
						_emit("duplicate-test",
							qq{duplicate test "$inner"},
							$file, $l2);
						next;
					}
				}
				# print __LINE__, ": $c2\n";
				next unless defined $ov;  # Skip regex check if $ov undefined

				# print __LINE__, ": $ov\n";
				if ($raw =~ /\Q\$$ov\s*=\~\s*(\/.+?\/)/ && $c2 =~ /\Q\$$ov\s*=\~\s*\Q$1\E/) {
					(my $pat = $1) =~ s{^/|/$}{}g;
					_emit('duplicate-regex',
						qq{duplicate regex match $pat},
						$file, $l2);
				}
			}
		}

		#
		# STEP 4: Enhanced elsif-chain redundancy
		#
		{
		    next unless $st->type eq 'if';
		    
		    detect_negated_condition_redundancy($st, $file);
		    
		    my @chain = ($st);
		    my $sib   = $st->snext_sibling;

		    # Collect entire if-elsif chain
		    while (blessed($sib) && $sib->isa('PPI::Statement::Compound') && $sib->type eq 'elsif') {
			push @chain, $sib;
			$sib = $sib->snext_sibling;
		    }
		    next if @chain <= 1;  # Skip if no elsifs

		    my @conds;
		    # Parse all conditions in the chain
		    for my $node (@chain) {
			my $raw_cond = $node->schild(1)->content;
			my ($var, $op, $val) = parse_cond($raw_cond);
			push @conds, {
			    raw  => $raw_cond,
			    var  => $var,
			    op   => $op,
			    val  => $val,
			    node => $node,
			    num  => looks_like_number($val) ? 0+$val : undef
			};
		    }

		    # Check each elsif against all previous conditions (existing logic)
		    for my $j (1 .. $#conds) {  # Start from first elsif
			next unless $conds[$j]{var};  # Skip unparsed conditions
			
			for my $i (0 .. $j-1) {  # Compare against all previous
			    next unless $conds[$i]{var};
			    next unless $conds[$j]{var} eq $conds[$i]{var};
			    
			    # Skip complement checking here since we handle it separately
			    next if defined $conds[$j]{num} && defined $conds[$i]{num} &&
				    are_complements($conds[$i]{op}, $conds[$i]{num}, 
						  $conds[$j]{op}, $conds[$j]{num});
			    
			    # Handle numeric comparisons (existing implication logic)
			    if (defined $conds[$j]{num} && defined $conds[$i]{num}) {
				if (implies($conds[$j]{op}, $conds[$j]{num}, 
					   $conds[$i]{op}, $conds[$i]{num})) 
				{
				    _emit("elsif-redundancy",
					qq{redundant elsif "$conds[$j]{raw}" } .
					qq{implied by "$conds[$i]{raw}"},
					$file, $conds[$j]{node}->line_number);
				    last;  # Break after first redundancy found
				}
			    }
			    # Handle string comparisons (existing logic)
			    elsif ($conds[$j]{op} =~ /^(?:eq|ne)$/ && 
				   $conds[$i]{op} =~ /^(?:eq|ne)$/) 
			    {
				# Skip string complements since we handle them separately
				next if are_complements($conds[$i]{op}, $conds[$i]{val}, 
						      $conds[$j]{op}, $conds[$j]{val});
				
				my $j_val = $conds[$j]{val};
				my $i_val = $conds[$i]{val};
				
				# Check string implication
				if ($conds[$j]{op} eq 'eq' && $conds[$i]{op} eq 'eq') {
				    if ($j_val eq $i_val) {
					_emit("duplicate-test",
					    qq{duplicate test "$conds[$j]{raw}"},
					    $file, $conds[$j]{node}->line_number);
					last;
				    }
				}
				elsif ($conds[$j]{op} eq 'ne' && $conds[$i]{op} eq 'eq') {
				    if ($j_val ne $i_val) {
					_emit("elsif-redundancy",
					    qq{redundant elsif "$conds[$j]{raw}" } .
					    qq{implied by "$conds[$i]{raw}"},
					    $file, $conds[$j]{node}->line_number);
					last;
				    }
				}
			    }
			}
		    }
		}	# End of Step 4
  } # end CMP loop

  #
  # STEP 5: clone detection (once per file)
  #
  {
    my $blocks = $doc->find('PPI::Structure::Block') || [];
    my %buckets;
    for my $blk (@$blocks) {
      next unless blessed($blk) && $blk->isa('PPI::Structure::Block');
      my $t = $blk->content;
      $t =~ s/^\s*\{//;  $t =~ s/\}\s*$//;  $t =~ s/^\s+|\s+$//g;
      my $nlines = ($t =~ tr/\n//) + 1;
      next if $nlines <= 1;
      push @{ $buckets{$t} }, $blk->line_number;
    }

    for my $txt (keys %buckets) {
      my $lines = $buckets{$txt};
      next unless @$lines > 1;
      my @sorted = sort { $a <=> $b } @$lines;
      my $msg = sprintf "clone of %d-line block at lines %s",
                        (($txt =~ tr/\n//)+1), join(", ", @sorted);
      _emit("clone-detection", $msg, $file, $sorted[0]);
    }
  }
}

#------------------------------------------------------------------------------
# Emit SARIF if requested
#------------------------------------------------------------------------------
if($do_sarif) {
	my $sarif = {
		version => $SARIF_VERSION,
		$SARIF_SCHEMA ? ( '$schema' => $SARIF_SCHEMA ) : (),
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
	print JSON::MaybeXS->new( canonical => 1, pretty => 1 )->encode($sarif);
}

# determine if two conditions are logical complements
sub are_complements {
    my ($op1, $x1, $op2, $x2) = @_;
    
    # Return 0 if values aren't numeric for relational operators
    return 0 unless looks_like_number($x1) && looks_like_number($x2);
    
    # Convert to numbers for comparison
    $x1 = 0 + $x1;
    $x2 = 0 + $x2;
    
    # Check for direct complements with same threshold
    if ($x1 == $x2) {
        return 1 if ($op1 eq '>' && $op2 eq '<=');
        return 1 if ($op1 eq '<=' && $op2 eq '>');
        return 1 if ($op1 eq '<' && $op2 eq '>=');
        return 1 if ($op1 eq '>=' && $op2 eq '<');
        return 1 if ($op1 eq '==' && $op2 eq '!=');
        return 1 if ($op1 eq '!=' && $op2 eq '==');
        return 1 if ($op1 eq 'eq' && $op2 eq 'ne');
        return 1 if ($op1 eq 'ne' && $op2 eq 'eq');
    }
    
    # Check for adjacent threshold complements
    # e.g., $x > 5 and $x <= 5 are complements
    # but also $x >= 6 and $x <= 5 are complements (no gap between)
    if ($op1 eq '>' && $op2 eq '<=') {
        return ($x1 == $x2);  # $x > 5 complements $x <= 5
    }
    if ($op1 eq '<=' && $op2 eq '>') {
        return ($x1 == $x2);  # $x <= 5 complements $x > 5
    }
    if ($op1 eq '<' && $op2 eq '>=') {
        return ($x1 == $x2);  # $x < 5 complements $x >= 5
    }
    if ($op1 eq '>=' && $op2 eq '<') {
        return ($x1 == $x2);  # $x >= 5 complements $x < 5
    }
    
    # Check for off-by-one complements with integer values
    if ($x1 == int($x1) && $x2 == int($x2)) {  # Both are integers
        if ($op1 eq '>' && $op2 eq '<=' && $x2 == $x1) {
            return 1;  # $x > 5 and $x <= 5
        }
        if ($op1 eq '>=' && $op2 eq '<' && $x1 == $x2) {
            return 1;  # $x >= 5 and $x < 5
        }
        # Additional integer-specific cases
        if ($op1 eq '>' && $op2 eq '<=' && $x1 + 1 == $x2) {
            return 1;  # $x > 4 and $x <= 5 (covers all integers)
        }
        if ($op1 eq '>=' && $op2 eq '<' && $x1 == $x2 + 1) {
            return 1;  # $x >= 6 and $x < 6
        }
    }
    
    return 0;
}

# Enhanced version of the elsif chain analysis from STEP 4
sub detect_negated_condition_redundancy {
    my ($st, $file) = @_;
    
    return unless $st->type eq 'if';
    
    # Get all children of the compound statement
    my @children = $st->children;
    
    # Find condition structures and their positions
    my @conditions;
    my $has_else = 0;
    
    for my $i (0 .. $#children) {
        my $child = $children[$i];
        
        # Look for condition structures
        if (blessed($child) && $child->isa('PPI::Structure::Condition')) {
            # Find the preceding keyword (if/elsif)
            my $keyword = '';
            for my $j (reverse 0 .. $i-1) {
                if (blessed($children[$j]) && $children[$j]->isa('PPI::Token::Word')) {
                    $keyword = $children[$j]->content;
                    last if $keyword =~ /^(?:if|elsif)$/;
                }
            }
            
            # Parse the condition
            my $raw_cond = $child->content;
            my ($var, $op, $val) = parse_cond($raw_cond);
            
            push @conditions, {
                raw      => $raw_cond,
                var      => $var,
                op       => $op,
                val      => $val,
                keyword  => $keyword,
                line     => $child->line_number,
                num      => (defined $val && looks_like_number($val)) ? 0+$val : undef
            };
        }
        # Check for else keyword
        elsif (blessed($child) && $child->isa('PPI::Token::Word') && $child->content eq 'else') {
            $has_else = 1;
        }
    }
    
    # Need at least 2 conditions (if + elsif)
    return if @conditions < 2;
    
    # Check for complement conditions
    for my $i (0 .. $#conditions-1) {
        next unless $conditions[$i]{var};  # Skip unparsed conditions
        
        for my $j ($i+1 .. $#conditions) {
            next unless $conditions[$j]{var};
            next unless $conditions[$j]{var} eq $conditions[$i]{var};  # Same variable
            
            # Check numeric complements
            if (defined $conditions[$i]{num} && defined $conditions[$j]{num}) {
                if (are_complements($conditions[$i]{op}, $conditions[$i]{num}, 
                                  $conditions[$j]{op}, $conditions[$j]{num})) {
                    
                    my ($msg, $rule_id);
                    
                    if ($has_else && $j == $#conditions) {
                        $rule_id = 'complement-with-else';
                        $msg = qq{redundant elsif "$conditions[$j]{raw}" - } .
                               qq{complement of "$conditions[$i]{raw}" with else clause present};
                    } elsif ($j == $i + 1) {
                        $rule_id = 'adjacent-complements';
                        $msg = qq{adjacent complement conditions: "$conditions[$i]{raw}" } .
                               qq{and "$conditions[$j]{raw}" are mutually exclusive};
                    } else {
                        $rule_id = 'complement-conditions';
                        $msg = qq{complement condition "$conditions[$j]{raw}" } .
                               qq{is opposite of "$conditions[$i]{raw}"};
                    }
                    
                    _emit($rule_id, $msg, $file, $conditions[$j]{line});
                }
            }
            # Check string complements
            elsif ($conditions[$i]{op} =~ /^(?:eq|ne)$/ && $conditions[$j]{op} =~ /^(?:eq|ne)$/) {
                if (are_complements($conditions[$i]{op}, $conditions[$i]{val}, 
                                  $conditions[$j]{op}, $conditions[$j]{val})) {
                    _emit('complement-conditions',
                          qq{complement string condition "$conditions[$j]{raw}" } .
                          qq{is opposite of "$conditions[$i]{raw}"},
                          $file, $conditions[$j]{line});
                }
            }
        }
    }
}
