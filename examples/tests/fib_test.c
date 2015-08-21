#include <stdio.h>
#include <stdlib.h>

int main (int argc, char ** argv) {
	int count = strtol(argv[1], 0, 0);
	
	printf("%d\n", count);

	int x = 1, y = 0;
	for (int i = 0; i < count; i++) {
		int tmp = x;
		x = x+y;
		y = tmp;
	}
	printf ("%d\n", x);
}
