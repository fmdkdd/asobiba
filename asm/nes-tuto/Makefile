all: main.nes

main.nes: main.o
	ld65 -o $@ -C main.cfg $<

%.o: %.s
	ca65 $< -g -o $@
