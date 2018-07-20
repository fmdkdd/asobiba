#!/bin/bash

# Exit on first failure
set -e

# Make sure the compiler is up to date
cargo build

# Build all the tests/*.cobra files to build/* binaries
echo == Building tests...
make

# Check the output of each binary against the expected output (the first line of
# the adder file)
ret=0
echo == Running tests...
for file in `find build/ -executable -type f | sort`; do
    bin=`basename "$file"`
    expect=`head -n1 "tests/$bin.cobra" | cut -d: -f2`
    actual=`./$file`
    if [[ "$expect" == "$actual" ]]; then
        echo pass: "$bin"
    else
        echo FAIL: "$bin": expected $expect, got $actual
        ret=1
    fi
done

exit $ret
