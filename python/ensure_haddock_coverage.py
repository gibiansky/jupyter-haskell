#!/usr/bin/env python
"""
Tiny utility script to check that coverage statistics output by stack haddock
are all 100%.
"""
import sys
import re

def main():
    """Entry point to ensure-haddock-coverage.py."""
    # Verify that the number of arguments is correct.
    if len(sys.argv) != 2:
        print("Usage: ./ensure-haddock-coverage.py $HADDOCK_OUT",
              file=sys.stderr)
        sys.exit(1)

    # Read contents of input file.
    filename = sys.argv[1]
    with open(filename, "r") as handle:
        contents = handle.read()

    # Find all coverage statistics.
    stats = []
    for line in contents.split("\n"):
        pat = " ([0-9]*)% \\([ 0-9]* / [ 0-9]*\\) in '([a-zA-Z.0-9]*)'"
        match = re.search(pat, line)
        if match is not None:
            stats.append((int(match.group(1)), match.group(2)))

    insufficient_coverage = False
    for coverage, name in stats:
        if coverage != 100:
            print("Insufficient Haddock Coverage on {}: {}"
                  .format(name, coverage))
            insufficient_coverage = True

    if len(stats) < 8:
        print(("Expecting at least 8 Haddock-covered modules.\n"
               "Possibly Haddock output nothing, or number of modules "
               "has decreased.\nIf number of modules has decreased, edit "
               "ensure_haddock_coverage.py to be up to date."),
              file=sys.stderr)
        insufficient_coverage = True

    if insufficient_coverage:
        sys.exit(1)


if __name__ == '__main__':
    main()
