all: $(patsubst %c,%exe, $(wildcard *.c))

# We have a special rule for the tail call test case because we need
# -O2 to convince gcc to emit a tail call, but we'd rather not have
# that much aggressive optimization for the other tests because it
# usually makes them trivial.
test-tail-call.exe: test-tail-call.c
	gcc -fno-stack-protector -O2 -fno-inline -S -c $< -o test-tail-call.s
	gcc -fno-stack-protector -O2 -fno-inline -nostdlib $< -o $@

%.exe: %.s
	gcc -fno-stack-protector -nostdlib $< -o $@

%.s: %.c
	gcc -fno-stack-protector -foptimize-sibling-calls -S -c $< -o $@

.PRECIOUS: %.s

clean:
	rm *.s *.exe
