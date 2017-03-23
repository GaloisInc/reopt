all: $(patsubst %c,%exe, $(wildcard *.c))

%.exe: %.s
	gcc -fno-stack-protector -nostdlib $< -o $@

%.s: %.c
	gcc -fno-stack-protector -S -c $< -o $@

.PRECIOUS: %.s

clean:
	rm *.s *.exe
