# Build all the DIAMONDBACK files under tests/, whenever the test files change,
# or when the DIAMONDBACK compiler is rebuilt.

COMPILER := target/debug/diamondback
TESTS := $(wildcard tests/*.diamondback)
ASM := $(TESTS:tests/%.diamondback=build/%.s)
BINS := $(TESTS:tests/%.diamondback=build/%)

all: build $(BINS)

build:
	mkdir -p build

build/%: main.c build/%.o
	gcc -g -m32 -o $@ $^

build/%.o: build/%.s
	nasm -f elf32 -w-orphan-labels -o $@ $^

.PRECIOUS: build/%.s
build/%.s: tests/%.diamondback $(COMPILER)
	$(COMPILER) < $< > $@

.PHONY: clean
clean:
	rm -f $(BINS) $(ASM)
