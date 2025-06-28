#!/usr/bin/env perl
use strict;
use warnings;
use PPI;

my $filename = shift or die "Usage: $0 file.pl\n";
my $doc = PPI::Document->new($filename) or die "Could not parse $filename\n";

# Track known scalar values and function returns
my %scalar_values;
my %function_values;

# Gather simple scalar assignments like: my $x = 5;
for my $stmt (@{ $doc->find('PPI::Statement::Variable') || [] }) {
    my $code = $stmt->content;
    if ($code =~ /my\s+(\$\w+)\s*=\s*(\d+)/) {
        $scalar_values{$1} = $2 + 0;
    }
}

# Gather simple subs returning constants: sub foo { return 10 }
for my $sub (@{ $doc->find('PPI::Statement::Sub') || [] }) {
    my $name = $sub->name or next;
    my $block = $sub->block or next;
    my $content = $block->content;
    if ($content =~ /return\s+(\d+)\s*;/) {
        $function_values{"${name}()"} = $1 + 0;
    }
}

sub eval_condition {
    my ($cond_text) = @_;

    # Substitute known scalar values
    for my $var (keys %scalar_values) {
        $cond_text =~ s/\Q$var\E/$scalar_values{$var}/g;
    }

    # Substitute known function calls
    for my $func (keys %function_values) {
        $cond_text =~ s/\Q$func\E/$function_values{$func}/g;
    }

    # Try eval safely
    my $result = eval $cond_text;
    return undef if $@;
    return $result;
}

my @tokens = $doc->tokens;
my @seen;  # track conditions in current if/elsif chain

for (my $i = 0; $i < @tokens; $i++) {
    my $tok = $tokens[$i];
    next unless $tok->isa('PPI::Token::Word');
    next unless $tok->content eq 'if' || $tok->content eq 'elsif';

    # reset chain on 'if'
    @seen = () if $tok->content eq 'if';

    my $cond = $tok->snext_sibling;
    next unless $cond && $cond->isa('PPI::Structure::Condition');

    my @ctoks = $cond->tokens;
    my ($var, $op, $num);

    # Filter out whitespace
    my @meaningful = grep { ! $_->isa('PPI::Token::Whitespace') } @ctoks;

    for my $j (0 .. $#meaningful - 2) {
        my ($t1, $t2, $t3) = @meaningful[$j, $j+1, $j+2];
        if ( ($t1->isa('PPI::Token::Symbol') || $t1->isa('PPI::Token::Word'))
            && $t2->isa('PPI::Token::Operator')
            && $t3->isa('PPI::Token::Number') )
        {
            $var = $t1->content;
            $op  = $t2->content;
            $num = $t3->content + 0;
            last;
        }
    }

    # Compose condition text (full)
    my $cond_text = join '', map { $_->content } @ctoks;

    # Evaluate always true/false if possible
    my $val = eval_condition($cond_text);
    if (defined $val) {
        if ($val) {
            print "Always true condition on line ", $tok->line_number, ": $cond_text\n";
        } else {
            print "Always false condition on line ", $tok->line_number, ": $cond_text\n";
        }
    }

    next unless defined $var && defined $op && defined $num;
    next unless $op eq '>';

    # Check redundancy with prior conditions
    for my $prev (@seen) {
        if ($prev->{var} eq $var && $prev->{op} eq '>' && $prev->{num} >= $num) {
            print "Redundant test on line ", $tok->line_number,
                  ": $var > $num is implied by earlier $var > $prev->{num} (line $prev->{line})\n";
            last;
        }
    }

    push @seen, { var => $var, op => $op, num => $num, line => $tok->line_number };
}
