#!/usr/bin/env perl
use strict;
use warnings;
use PPI;
use JSON::PP;

# Parse options
my $sarif = 0;
if (@ARGV && $ARGV[0] eq '--sarif') {
    $sarif = 1;
    shift @ARGV;
}

die "Usage: $0 [--sarif] file1.pl [file2.pl ...]\n" unless @ARGV;

my @results;  # To hold all results for SARIF

for my $filename (@ARGV) {
    my $doc = PPI::Document->new($filename)
      or warn "Could not parse $filename: $!\n" and next;

    my @tokens = $doc->tokens;
    my @seen;

    for (my $i = 0; $i < @tokens; $i++) {
        my $tok = $tokens[$i];
        next unless $tok->isa('PPI::Token::Word');
        next unless $tok->content eq 'if' || $tok->content eq 'elsif';

        @seen = () if $tok->content eq 'if';

        my $cond = $tok->snext_sibling;
        next unless $cond && $cond->isa('PPI::Structure::Condition');

        my @ctoks = $cond->tokens;
        my ($var, $op, $num);

        # Ignore whitespace for pattern matching
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

        next unless defined $var && defined $op && defined $num;
        next unless $op eq '>';

        for my $prev (@seen) {
            if ($prev->{var} eq $var && $prev->{op} eq '>' && $prev->{num} >= $num) {
                my $msg = "Redundant test on line ".$tok->line_number.
                          ": $var > $num is implied by earlier $var > $prev->{num} (line $prev->{line})";

                if ($sarif) {
                    push @results, {
                        file       => $filename,
                        message    => $msg,
                        line       => $tok->line_number,
                        start_line => $tok->line_number,
                    };
                } else {
                    print "$filename: $msg\n";
                }
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
}

if ($sarif) {
    my $sarif_obj = {
        version => "2.1.0",
        '$schema' => "https://schemastore.azurewebsites.net/schemas/json/sarif-2.1.0.json",
        runs    => [ {
            tool => {
                driver => {
                    name => "PerlRedundantTestScanner",
                    informationUri => "https://example.com",
                    rules => [
                        {
                            id => "RedundantTest",
                            shortDescription => { text => "Redundant conditional test" },
                            helpUri => "https://example.com/docs/redundant-tests",
                            properties => {
                                category => "Logic",
                                severity => "warning",
                            },
                        }
                    ],
                }
            },
            results => [
                map {
                    {
                        ruleId => "RedundantTest",
                        level  => "warning",
                        message => { text => $_->{message} },
                        locations => [
                            {
                                physicalLocation => {
                                    artifactLocation => { uri => $_->{file} },
                                    region => { startLine => $_->{start_line} },
                                }
                            }
                        ],
                    }
                } @results
            ],
        } ]
    };

    print JSON::PP->new->utf8->pretty->encode($sarif_obj);
}
