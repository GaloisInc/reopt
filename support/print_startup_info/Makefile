clang?=clang

print_startup_info : print_startup_info.c write.s start.s
	$(clang) -g -nostdlib -o $@ $^

clean :
	rm -f print_startup_info

.PHONY : clean
