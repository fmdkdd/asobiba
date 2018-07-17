# Build all the COBRA files under tests/, whenever the test files change, or
# when the COBRA compiler is rebuilt.

COMPILER := target/debug/cobra
TESTS := $(wildcard tests/*.cobra)
ASM := $(TESTS:tests/%.cobra=build/%.s)
BINS := $(TESTS:tests/%.cobra=build/%)

all: build $(BINS)

build:
	mkdir -p build

build/%: main.c build/%.o
	gcc -g -m32 -o $@ $^

build/%.o: build/%.s
	nasm -f elf32 -w-orphan-labels -o $@ $^

.PRECIOUS: build/%.s
build/%.s: tests/%.cobra $(COMPILER)
	$(COMPILER) < $< > $@

.PHONY: clean
clean:
	rm -f $(BINS) $(ASM)
