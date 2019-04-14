FLAGS := -Wall -Wextra -pedantic -std=c17 -O3
INCLUDES := -D_REENTRANT -I/usr/include/SDL2
LIBS := -pthread -lSDL2

# make CPUDIAG=1 triggers this
ifeq ($(CPUDIAG), 1)
FLAGS += -DCPUDIAG
endif

spinv: main.c cpu.h compiler_flags
	$(CC) $< -o $@ $(FLAGS) $(INCLUDES) $(LIBS)

# Remember last flags used
.PHONY: force
compiler_flags: force
	@echo '$(FLAGS)' | cmp -s - $@ || echo '$(FLAGS)' > $@
