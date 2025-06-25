# Perl Redundancy Scanner

A PPI-based tool to detect redundant boolean tests and nested-block conditions  
(supports `if`/`elsif`/`unless`/`while`/`until`, `&&`/`||`, DateTime coercion).

## Install

    git clone git@github.com:nigelhorne/perl-redundancy-scanner.git
    cd perl-redundancy-scanner
    cpanm PPI Scalar::Util DateTime

## Usage

Make sure `bin/scan-redundancy` is executable:

    ./bin/scan-redundancy lib/*.pm script.pl

## Invocation in Your Workflow

Add this to your Actions workflow

```yaml
---
name: Redundancy CI

on: [push,pull_request]

jobs:
  redundancy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Run Perl redundancy scan
        uses: nigelhorne/perl-redundancy-scanner/.github/actions/scan@v1
        with:
          files: 'lib/**/*.pm script/**/*.pl'
  
```
