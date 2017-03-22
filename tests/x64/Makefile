all: $(patsubst %c,%exe, $(wildcard *.c))

%.exe: %.s
	gcc -nostdlib $< -o $@

%.s: %.c
	gcc -S -c $< -o $@

.PRECIOUS: %.s

clean:
	rm *.s *.exe
