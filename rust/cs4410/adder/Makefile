# Build all the ADDER files under tests/, whenever the test files change, or
# when the ADDER compiler is rebuilt.

COMPILER := target/debug/adder
TESTS := $(wildcard tests/*.adder)
ASM := $(TESTS:tests/%.adder=build/%.s)
BINS := $(TESTS:tests/%.adder=build/%)

all: build $(BINS)

build:
	mkdir -p build

build/%: main.c build/%.o
	gcc -g -m32 -o $@ $^

build/%.o: build/%.s
	nasm -f elf32 -w-orphan-labels -o $@ $^

.PRECIOUS: build/%.s
build/%.s: tests/%.adder $(COMPILER)
	@tail -n+2 $< > $@.in
	$(COMPILER) < $@.in > $@
	@rm $@.in

.PHONY: clean
clean:
	rm -f $(BINS) $(ASM)
