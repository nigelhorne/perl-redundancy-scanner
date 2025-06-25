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
  { id => "boolean-redundancy",   shortDescription => { text => "Redundant comparison in boolean expression" } },
  { id => "nested-threshold",     shortDescription => { text => "Nested numeric threshold redundant" } },
  { id => "identical-structure",  shortDescription => { text => "Identical nested condition" } },
  { id => "duplicate-test",       shortDescription => { text => "Exact duplicate test" } },
  { id => "duplicate-regex",      shortDescription => { text => "Duplicate regex match" } },
  { id => "elsif-redundancy",     shortDescription => { text => "Redundant elsif implied by earlier condition" } },
  { id => "always-true-test",     shortDescription => { text => "Condition always true based on known constant" } },
  { id => "always-false-test",    shortDescription => { text => "Condition always false based on known constant" } },
  { id => "clone-detection",      shortDescription => { text => "Duplicate code block" } },
);

my $do_sarif = 0;
my @sarif_results;

#------------------------------------------------------------------------------
# Helpers
#------------------------------------------------------------------------------

sub coerce_number {
  my ($v) = @_;
  return $v->epoch if blessed($v) && $v->isa('DateTime');
  return 0 + $v if looks_like_number($v);
  return int($v) if defined $v && $v =~ /^\s*([+-]?\d+)\s*$/;
  return;
}

sub parse_cond {
  my $s = shift;
  $s =~ s/^\s*\(//;  $s =~ s/\)\s*$//;
  if ($s =~ /^\s*\$(\w+)\s*(>=|>|==|<=|<|eq|ne)\s*(.+?)\s*$/) {
    return ($1, $2, $3);
  }
  if ($s =~ /^\s*(.+?)\s*(>=|>|==|<=|<|eq|ne)\s*\$(\w+)\s*$/) {
    my ($val, $op, $var) = ($1, $2, $3);
    my %flip = ( '<'=>'>', '<='=>'>=', '>' =>'<','>='=>'<=', '=='=>'==','eq'=>'eq','ne'=>'ne' );
    return ($var, $flip{$op}, $val);
  }
  return;
}

sub implies {
  my ($oA,$vA,$oB,$vB) = map { "" . $_ } @_;
  return unless $oA =~ /^(?:>=?|<=?|==|eq|ne)$/ && $oB =~ /^(?:>=?|<=?|==|eq|ne)$/;
  return unless defined( my $nA = coerce_number($vA) ) && defined( my $nB = coerce_number($vB) );

  no warnings 'numeric';
  if ($oA =~ /^>=?$/ && $oB =~ /^>=?$/) { return $nA >= $nB }
  if ($oA =~ /^<=?$/ && $oB =~ /^<=?$/) { return $nA <= $nB }
  use warnings 'numeric';
  return;
}

sub _emit {
  my ($rule, $msg, $file, $ln) = @_;
  if ($do_sarif) {
    push @sarif_results, {
      ruleId  => $rule,
      level   => "warning",
      message => { text => $msg },
      locations => [{
        physicalLocation => {
          artifactLocation => { uri       => $file },
          region           => { startLine => $ln   },
        }
      }],
    };
  }
  else {
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
die "Usage: $0 [--sarif] file1.pl [file2.pl ...]\n" unless @ARGV;

for my $file (@ARGV) {
  print "\nScanning $file …\n";
  my $doc = PPI::Document->new($file)
    or warn "✗ Failed to parse $file: $@\n" and next;

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
    my $name  = $snode->name  or next;
    my $block = $snode->block or next;
    next unless blessed($block) && $block->isa('PPI::Structure::Block');
    my $body  = $block->content;
    if ($body =~ /\breturn\s+([+-]?\d+)\b/) {
      $CONST{"__SUB__$name"} = 0 + $1;
    }
    
  }

  # Main loop: compound statements
  my $cmps = $doc->find('PPI::Statement::Compound') || [];
  # Find all the "if" statements that start an if–elsif chain

  # Now iterate by index so we can tell who’s a chain head
  CMP: for my $i (0 .. $#$cmps) {
    my $st = $cmps->[$i];
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
  if ($st->type eq 'if') {
    # grab the if‐condition plus any elsif-conditions
    my $conds = $st->find('PPI::Structure::Condition') || [];
    # only interested when there’s at least one elsif
    if (@$conds > 1) {
      # parse the very first (the 'if')
      my ($v0,$o0,$x0) = parse_cond($conds->[0]->content) or next CMP;

      # now walk each elsif
# … after you’ve detected @conds, and parsed ($v0,$o0,$x0) …

for my $cn (@$conds[1..$#$conds]) {
  my $raw2 = $cn->content;
  my $ln2  = $cn->line_number;

  # extract into lexicals first
  my ($v1,$o1,$x1) = parse_cond($raw2);

  # only if it’s the same var *and* implied by the if-cond
  if (defined $v1
      && $v1 eq $v0
      && implies($o0,$x0,$o1,$x1)
  ) {
    _emit(
      "elsif-redundancy",
      qq{redundant elsif "$raw2" implied by "} 
        . $conds->[0]->content
        . qq{"},
      $file, $ln2
    );
  }
}
      

      # skip everything else for this compound
      next CMP;
    }
  }
    

    # ————————————————————————————————————————————————————————————————————————
    # STEP 1: always-true / always-false detection for standalone statements
    # ————————————————————————————————————————————————————————————————————————
    {
      # A) strip outer parens
      (my $expr = $raw) =~ s/^\s*[(

\[]\s*//;
      $expr           =~ s/\s*[)\]

]\s*$//;

      # B) subcall() OP literal
      if ($expr =~ /^\s*([A-Za-z_]\w*)\(\)\s*
                     (==|eq|!=|ne|>=|<=|>|<)\s*
                     ([+-]?\d+)\s*$/x)
      {
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

      # D) literal  OP  literal
      if ($cmp =~ /^\s*([+-]?\d+)\s*
                     (==|eq|!=|ne|>=|<=|>|<)\s*
                     ([+-]?\d+)\s*$/x)
      {
        my ($L,$op,$R) = ($1,$2,$3);
        my $true =
           $op eq '=='||$op eq 'eq'?($L == $R):
           $op eq '!='||$op eq 'ne'?($L != $R):
           $op eq '>'             ?($L >  $R):
           $op eq '>='            ?($L >= $R):
           $op eq '<'             ?($L <  $R):
           $op eq '<='            ?($L <= $R):0;
        my $kind = $true?"always-true-test":"always-false-test";
        my $msg  = qq{always-} . ($true?"true":"false")
                   . qq{ test "$L $op $R"};
        _emit($kind,$msg,$file,$ln);
        next CMP;
      }

      # E) $var  OP  literal
      if ($cmp =~ /^\s*\$(\w+)\s*
                     (==|eq|!=|ne|>=|<=|>|<)\s*
                     ([+-]?\d+)\s*$/x)
      {
        my ($v,$op,$lit) = ($1,$2,$3);
        next unless exists $CONST{$v};
        my $lhs_txt = "\$$v";
        my $lhs_val = $CONST{$v};
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

      # anything else (e.g. @INC > 0) falls through
    }

    #
    # STEP 2: boolean AND/OR redundancy
    #
    {
      my @c = split /(?:&&|\|\|)/, $raw;
      my (@comps, @terms);
      for my $p (@c) {
        (my $t = $p) =~ s/^\s*\(//;  $t =~ s/\)\s*$//;
        $t =~ s/^\s+|\s+$//g;
        if ( my @pp = parse_cond($t) ) {
          push @comps, $t;
          push @terms, [ @pp ];
        }
      }
      if (@terms > 1) {
        my $bool_op = $raw =~ /&&/ ? 'AND' : 'OR';
        for my $i (0 .. $#terms-1) {
          for my $j ($i+1 .. $#terms) {
            my ($v1,$o1,$x1) = @{ $terms[$i] };
            my ($v2,$o2,$x2) = @{ $terms[$j] };
            next unless $v1 eq $v2;
            if    (implies($o1,$x1,$o2,$x2)) {
              _emit("boolean-redundancy",
                    qq{"$comps[$j]" redundant in $bool_op with "$comps[$i]"},
                    $file, $ln);
            }
            elsif (implies($o2,$x2,$o1,$x1)) {
              _emit("boolean-redundancy",
                    qq{"$comps[$i]" redundant in $bool_op with "$comps[$j]"},
                    $file, $ln);
            }
          }
        }
      }
    }

    #
    # STEP 3: nested-block redundancy
    #
    {
      my ($ov,$oo,$ox) = parse_cond($raw);
      my $blk = $st->schild(2);
      next unless blessed($blk) && $blk->isa('PPI::Structure::Block');

      (my $outer = $raw) =~ s/^\s*\(//;  $outer =~ s/\)\s*$//;  $outer =~ s/^\s+|\s+$//g;
      my $inners = $blk->find('PPI::Statement::Compound') || [];

      for my $in (@$inners) {
        next unless blessed($in) && $in->type =~ /^(?:if|elsif|unless|while|until)$/;
        my $c2 = $in->schild(1)->content;
        my $l2 = $in->line_number;
        (my $inner = $c2) =~ s/^\s*\(//;  $inner =~ s/\)\s*$//;  $inner =~ s/^\s+|\s+$//g;

        if ( my ($iv,$io,$ix) = parse_cond($c2) ) {
          next unless defined $ov && $iv eq $ov;
          if (implies($oo,$ox,$io,$ix)) {
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
	next CMP unless defined $ov; 
        if ($raw =~ /\Q\$$ov\s*=\~\s*(\/.+?\/)/ && $c2 =~ /\Q\$$ov\s*=\~\s*\Q$1\E/) {
          (my $pat = $1) =~ s{^/|/$}{}g;
          _emit("duplicate-regex",
                qq{duplicate regex match $pat},
                $file, $l2);
        }
      }
    }

    #
    # STEP 4: elsif-chain redundancy
    #
    {
      next unless $st->type eq 'if';
      my @chain = ($st);
      my $sib   = $st->snext_sibling;
      while (blessed($sib) && $sib->isa('PPI::Statement::Compound') && $sib->type eq 'elsif') {
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
              _emit("elsif-redundancy",
                    qq{redundant elsif "$raw2" implied by "$chain[0]->schild(1)->content"},
                    $file, $ln2);
            }
          }
        }
      }
    }
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
