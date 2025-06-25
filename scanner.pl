#!/usr/bin/env perl
use strict;
use warnings;
use PPI;
use Scalar::Util qw(blessed looks_like_number);

# Try to turn “val” into a plain number, or return undef
sub coerce_number {
    my ($val) = @_;
    if (blessed($val) && $val->isa('DateTime')) {
        return $val->epoch;
    }
    return 0 + $val if looks_like_number($val);
    return int($val)   if $val =~ /^\s*([+-]?\d+)\s*$/;
    return;
}

# parse “$var op val” or flipped “val op $var” → ($var,op,val)
sub parse_cond {
    my $s = shift;
    $s =~ s/^\s*\(//;  $s =~ s/\)\s*$//;
    return ($1,$2,$3) if $s =~ /^\s*\$(\w+)\s*(>=|>|==|<=|<|eq|ne)\s*(.+?)\s*$/;
    if ($s =~ /^\s*(.+?)\s*(>=|>|==|<=|<|eq|ne)\s*\$(\w+)\s*$/) {
        my ($v,$o,$var) = ($1,$2,$3);
        my %flip = ('<' => '>', '<=' => '>=','>' => '<','>='=>'<=','=='=>'==','eq'=>'eq','ne'=>'ne');
        return ($var, $flip{$o}, $v);
    }
    return;
}

# ∀x: (x opA valA) ⇒ (x opB valB)?
sub implies {
    my ($opA,$vA,$opB,$vB) = map { "" . $_ } @_;
    return unless $opA =~ /^(?:>=?|<=?|==|eq|ne)$/ && $opB =~ /^(?:>=?|<=?|==|eq|ne)$/;
    return unless defined( my $nA = coerce_number($vA) )
               && defined( my $nB = coerce_number($vB) );
    if ($opA =~ /^>=?$/ && $opB =~ /^>=?$/) { return $nA >= $nB }
    if ($opA =~ /^<=?$/ && $opB =~ /^<=?$/) { return $nA <= $nB }
    return;
}

die "Usage: $0 file1.pl [file2.pl ...]\n" unless @ARGV;
for my $file (@ARGV) {
    print "\nScanning $file …\n";
    my $doc = PPI::Document->new($file)
      or warn "  ✗ parse failed: $@\n" and next;

    my $stmts = $doc->find('PPI::Statement::Compound') || [];
    for my $st (@$stmts) {
        my $type = $st->type;
        next unless $type =~ /^(?:if|elsif|unless|while|until)$/;
        my $raw = $st->schild(1)->content;

        # 1) AND/OR redundancies
        my @parts = map { s/^\s+|\s+$//gr } split /(?:&&|\|\|)/, $raw;
        my @cmp   = grep { parse_cond($_) } @parts;
        my @terms = map { [ parse_cond($_) ] } @cmp;
        if (@terms > 1) {
            my $op = $raw =~ /&&/ ? 'AND' : 'OR';
            for my $i (0 .. $#terms-1) {
              for my $j ($i+1 .. $#terms) {
                my ($v1,$o1,$x1)=@{ $terms[$i] };
                my ($v2,$o2,$x2)=@{ $terms[$j] };
                next unless $v1 && $v1 eq $v2;
                if (implies($o1,$x1,$o2,$x2)) {
                  printf "  [%s:%d] “%s” redundant in %s with “%s”\n",
                    $file, $st->line_number, $cmp[$j], $op, $cmp[$i];
                }
                elsif (implies($o2,$x2,$o1,$x1)) {
                  printf "  [%s:%d] “%s” redundant in %s with “%s”\n",
                    $file, $st->line_number, $cmp[$i], $op, $cmp[$j];
                }
              }
            }
        }

        # 2) Nested-block redundancies
        my ($ov,$oo,$ox) = parse_cond($raw) || ();
        my $blk = $st->schild(2);
        next unless $blk && $blk->isa('PPI::Structure::Block');
        my $inners = $blk->find('PPI::Statement::Compound')||[];
        for my $in (@$inners) {
            my $t2 = $in->type;
            next unless $t2 =~ /^(?:if|elsif|unless|while|until)$/;
            my $c2 = $in->schild(1)->content;
            my ($iv,$io,$ix) = parse_cond($c2) or next;
            next unless $ov && $iv eq $ov;
            if (implies($oo,$ox,$io,$ix)) {
                printf "  [%s:%d] redundant “%s” under “%s”\n",
                  $file, $in->line_number, $c2, $raw;
                next;
            }
            if ($type eq $t2 && $raw eq $c2) {
                printf "  [%s:%d] nested identical %s(%s)\n",
                  $file, $in->line_number, $t2, $c2;
                next;
            }
            if ($raw eq $c2) {
                printf "  [%s:%d] duplicate test “%s”\n",
                  $file, $in->line_number, $c2;
                next;
            }
            if ($raw =~ /\Q\$$ov =~\E\s*(.+)$/ && $c2 =~ /\Q\$$ov =~\E\s*\1/) {
                printf "  [%s:%d] duplicate regex match %s\n",
                  $file, $in->line_number, $1;
            }
        }
    }
}
