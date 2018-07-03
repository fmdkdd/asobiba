all: build build/main

build:
	mkdir -p build

build/main: main.c build/prog.o
	gcc -g -m32 -o $@ $^

build/prog.o: build/prog.s
	nasm -f elf32 -w-orphan-labels -o $@ $^

build/prog.s:
	cargo run > $@

.PHONY: clean
clean:
	rm build/main build/prog.o build/prog.s
