// Compare original 'memcmp' to reopted 'memcmp'.
#include <stdint.h>
#include <stdio.h>
#include <string.h>

// uint64_t _F4004ec(uint64_t b1, uint64_t b2, uint64_t b3);
int F4004ec(const char *b1, const char *b2, uint64_t sz);

int main(void)
{
    printf("All output pairs should be equal:\n");

    char b1[] = "Hello World";
    char b2[] = "hello world";

    int r1 = F4004ec(b1, b2, strlen(b1));
    int s1 = memcmp(b1, b2, strlen(b1));

    printf("%d %d\n", r1, s1);
    
    int r2 = F4004ec(b1, b1, strlen(b1));
    int s2 = memcmp(b1, b1, strlen(b1));

    printf("%d %d\n", r2, s2);

    return 0;
}
