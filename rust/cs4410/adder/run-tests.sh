#!/bin/sh

# Exit on first failure
set -e

# Make sure the compiler is up to date
cargo build

# Build all the tests/*.adder files to build/* binaries
echo == Building tests...
make

# Check the output of each binary against the expected output (the first line of
# the adder file)
ret=0
echo == Running tests...
for file in `find build/ -executable -type f`; do
    bin=`basename "$file"`
    expect=`head -n1 "tests/$bin.adder" | cut -d: -f2`
    actual=`./$file`
    if [ $expect != $actual ]; then
        echo FAIL: "$bin": expected $expect, got $actual
        ret=1
    else
        echo pass: "$bin"
    fi
done

exit $ret
