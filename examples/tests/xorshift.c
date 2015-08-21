#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

int main (int argc, char ** argv) {
	unsigned int x = 0x12345678, y = 0x23456789, z = 0x34567890, w = 0x4567890A;

	int count = strtol(argv[1], 0, 0);

	for (int i = 0; i < count; i++) {
		unsigned int t = x ^ (x<<11);
		x = y;
		y = z;
		z = w;
		w = w ^ (w >> 19) ^ t ^ (t >> 8);
	}

	printf("%d\n", w);
	return 0;
}
