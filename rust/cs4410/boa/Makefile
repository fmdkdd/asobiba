# Build all the BOA files under tests/, whenever the test files change, or
# when the BOA compiler is rebuilt.

COMPILER := target/debug/boa
TESTS := $(wildcard tests/*.boa)
ASM := $(TESTS:tests/%.boa=build/%.s)
BINS := $(TESTS:tests/%.boa=build/%)

all: build $(BINS)

build:
	mkdir -p build

build/%: main.c build/%.o
	gcc -g -m32 -o $@ $^

build/%.o: build/%.s
	nasm -f elf32 -w-orphan-labels -o $@ $^

.PRECIOUS: build/%.s
build/%.s: tests/%.boa $(COMPILER)
	@tail -n+2 $< > $@.in
	$(COMPILER) < $@.in > $@
	@rm $@.in

.PHONY: clean
clean:
	rm -f $(BINS) $(ASM)
