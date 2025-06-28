#!/usr/bin/env perl
use strict;
use warnings;
use PPI;

my $filename = shift or die "Usage: $0 file.pl\n";
my $doc = PPI::Document->new($filename) or die "Failed to parse $filename\n";

my @tokens = $doc->tokens;
my @seen;

for (my $i = 0; $i < @tokens; $i++) {
	my $tok = $tokens[$i];

	next unless $tok->isa('PPI::Token::Word');
	next unless $tok->content eq 'if' || $tok->content eq 'elsif';

	# Reset when a new 'if' is found (start of chain)
	@seen = () if $tok->content eq 'if';

	my $cond = $tok->snext_sibling;
	next unless $cond && $cond->isa('PPI::Structure::Condition');

	my @ctoks = $cond->tokens;

	my ($var, $op, $num);

	# Extract tokens ignoring whitespace
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
			# print "Found condition: $var $op $num\n";
			last;
		}
	}
	
	next unless defined $var && defined $op && defined $num;
	next unless $op eq '>';

	for my $prev (@seen) {
		if ($prev->{var} eq $var && $prev->{op} eq '>' && $prev->{num} >= $num) {
			print "Redundant test on line ", $tok->line_number, ": $var > $num is implied by earlier $var > $prev->{num} (line $prev->{line})\n";
			last;
		}
	}

	push @seen, {
		var  => $var,
		op   => $op,
		num  => $num,
		line => $tok->line_number,
	};
}
