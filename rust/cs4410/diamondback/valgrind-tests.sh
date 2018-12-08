#!/bin/bash

# Exit on first failure
set -e

# Make sure the compiler is up to date
cargo build

# Build all the tests/*.diamondback files to build/* binaries
echo == Building tests...
make

# Check the output of each binary against the expected output (the first line of
# the adder file)
set +e

echo == Running valgrind on tests...
for file in `find build/ -executable -type f | sort`; do
    bin=`basename "$file"`
    echo Running: $bin...
    valgrind --quiet ./$file
    echo
done
