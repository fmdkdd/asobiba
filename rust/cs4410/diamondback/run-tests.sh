#!/bin/bash

COMP=target/debug/diamondback
SUFFIX=.diamondback

# Return value
ret=0

# Make sure the compiler is up to date
cargo build || exit 2

# Run static tests first
echo == Running static tests...

for file in `find tests/static/ -name "*${SUFFIX}" | sort`; do
    base=`basename -s $SUFFIX "$file"`
    expect=`head -n1 $file | cut -d: -f2-`
    actual=`$COMP < $file 2>&1 | rg :error: | cut -d: -f2-`
    if [[ "$expect" == "$actual" ]]; then
        echo pass: "$base"
    else
        echo FAIL: "$base": expected \"$expect\", got \"$actual\"
        ret=1
    fi
done

# Exit early if any static test failed
if [[ $ret != 0 ]]; then
    exit $ret
fi
ret=0

# Build all the tests/*.diamondback files to build/* binaries
echo == Building dynamic tests...
make || exit 3

# Check the output of each binary against the expected output (the first line of
# the adder file)
ret=0
echo == Running dynamic tests...
for file in `find build/ -executable -type f | sort`; do
    bin=`basename "$file"`
    expect=`head -n1 "tests/$bin$SUFFIX" | cut -d: -f2-`
    actual=`./$file 2>&1`
    if [[ "$expect" == "$actual" ]]; then
        echo pass: "$bin"
    else
        echo FAIL: "$bin": expected \"$expect\", got \"$actual\"
        ret=1
    fi
done

exit $ret
