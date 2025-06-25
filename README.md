# Perl Redundancy Scanner

A PPI-based tool to detect redundant boolean tests and nested-block conditions  
(supports `if`/`elsif`/`unless`/`while`/`until`, `&&`/`||`, DateTime coercion).

## Install

    git clone git@github.com:you/perl-redundancy-scanner.git
    cd perl-redundancy-scanner
    cpanm PPI Scalar::Util DateTime

Or bundle into a distro with `dzil`.

## Usage

Make sure `bin/scan-redundancy` is executable:

    ./bin/scan-redundancy lib/*.pm script.pl

## CI

We run a quick smoke test on each push via GitHub Actions (see `.github/workflows/perl.yml`).

## Extending

- Add more `coerce_*` rules in `implies()`
- Plug in SARIF JSON output
- Write more tests in `t/`
