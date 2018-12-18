#!/bin/bash

COMP=target/debug/diamondback
SUFFIX=.diamondback

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m'

# Make sure the compiler is up to date
cargo build || exit 2

# Build all the tests/*.diamondback files to build/* binaries
echo -e "${YELLOW}== Building dynamic tests...${NC}"
make -j4 || exit 3

# Return value
ret=0

# Run static tests first
echo -e "${YELLOW}== Running static tests...${NC}"

for file in `find tests/static/ -name "*${SUFFIX}" | sort`; do
    base=`basename -s $SUFFIX "$file"`
    expect=`head -n1 $file | cut -d: -f2-`
    actual=`$COMP < $file 2>&1 | rg :error: | cut -d: -f2-`
    if [[ "$expect" == "$actual" ]]; then
        echo -e "${GREEN}pass${NC}:" "$base"
    else
        echo -e "${RED}FAIL${NC}:" "$base": expected \"$expect\", got \"$actual\"
        $COMP < $file
        ret=1
    fi
done

# Check the output of each binary against the expected output (the first line)
echo -e "${YELLOW}== Running dynamic tests...${NC}"
for file in `find build/ -executable -type f | sort`; do
    bin=`basename "$file"`
    expect=`head -n1 "tests/$bin$SUFFIX" | cut -d: -f2-`
    actual=`./$file 2>&1`
    if [[ "$expect" == "$actual" ]]; then
        echo -e "${GREEN}pass${NC}:" "$bin"
    else
        echo -e "${RED}FAIL${NC}:" "$bin": expected \"$expect\", got \"$actual\"
        ./$file
        echo
        ret=1
    fi
done

exit $ret
