FLAGS=

# make CPUDIAG=1 triggers this
ifeq ($(CPUDIAG), 1)
FLAGS += -DCPUDIAG
endif

spinv: main.c compiler_flags
	$(CC) $< -o $@ -Wall -Wextra -pedantic -std=c17 $(FLAGS)

# Remember last flags used
.PHONY: force
compiler_flags: force
	@echo '$(FLAGS)' | cmp -s - $@ || echo '$(FLAGS)' > $@
