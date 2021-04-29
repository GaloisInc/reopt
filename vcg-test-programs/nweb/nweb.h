
typedef unsigned long size_t;
typedef int pid_t;
typedef long off_t;
typedef unsigned uint32_t;
typedef unsigned uint16_t;

long read(int fd, void *buf, size_t count);
long write(int fd, void* buf, size_t count);
int strcmp(const char *s1, const char *s2);
int strncmp(const char *s1, const char *s2, size_t n);
pid_t getpid();
int chdir(const char *path);
char *strcpy(char *dest, const char *src);
size_t strlen(const char *s);
int close(int fd);
pid_t fork();
off_t lseek(int fd, off_t offset, int whence);

//typedef void (*sighandler_t)(int);
typedef void* sighandler_t;
sighandler_t signal(int signum, sighandler_t handler);

unsigned sleep(unsigned seconds);
int setpgrp();
int socket(int domain, int type, int protocol);
int atoi(const char *nptr);
uint32_t htonl(uint32_t hostlong);
uint16_t htons(uint16_t hostshort);


typedef size_t socklen_t;
int bind(int sockfd, const void *addr, socklen_t addrlen);
int listen(int sockfd, int backlog);
int accept(int sockfd, void *addr, socklen_t *addrlen);