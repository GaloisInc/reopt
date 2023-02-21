// We don't support structs yet, so these typedefs fake them for now

typedef void* fd_set; // yikes

typedef void* nfds_t; // yikes

typedef void* pcre; // yikes
typedef void* pcre_extra; // yikes

typedef void* regex_t; // yikes
typedef void* regmatch_t; // yikes

typedef void* sigjmp_buf; // yikes
typedef void* struct_dirent; // yikes
typedef void* struct_dirent64; // yikes
typedef void* struct_epoll_event;
typedef void* struct_in_addr; // yikes
typedef void* struct_msghdr; // yikes
typedef void* struct_pollfd; // yikes
typedef void* struct_rlimit; // yikes
typedef void* struct_sockaddr; // yikes
typedef void* struct_stat64; // yikes
typedef void* struct_termios; // yikes
typedef void* struct_timeval; // yikes
typedef void* struct_timezone; // yikes
typedef void* struct_utimbuf; // yikes
typedef void* struct_ypall_callback; // yikes

typedef void* sockaddr; // yikes

typedef void* va_list; // yikes

typedef unsigned gid_t;
typedef unsigned uid_t;
typedef unsigned uint_t;
typedef unsigned uint16_t;
typedef unsigned uint32_t;

typedef void* DIR; // yikes
typedef void* FILE; // yikes

typedef uint_t mode_t;
typedef long off_t;
typedef long long off64_t; // ???
typedef int pid_t;
typedef void* sighandler_t;
typedef unsigned long size_t;
typedef size_t socklen_t;
typedef size_t ssize_t;
typedef int time_t;

// typedef struct utimbuf {
//   time_t actime;
//   time_t modtime;
// } utimbuf;

/*
TODO:
mkstemp64
tcgetattr
closedir
readdir64
*/

int __xstat64(int ver, const char * path, struct_stat64 * stat_buf);
int __lxstat64(int ver, const char * path, struct_stat64 * stat_buf);
int __fxstat64(int ver, int fildes, struct_stat64 * stat_buf);

int accept(int sockfd, void *addr, socklen_t *addrlen);
int atoi(const char *nptr);

int bind(int sockfd, const void *addr, socklen_t addrlen);

int chdir(const char *path);
int chmod(const char *path, mode_t mode);
int chown(const char *pathname, uid_t owner, gid_t group);
int close(int fd);
int closedir(DIR *dirp);
int connect(int sockfd, const sockaddr *addr, socklen_t addrlen);

char *db_version(int *major, int *minor, int *patch);

int epoll_create(int size);
int epoll_ctl(int epfd, int op, int fd, struct_epoll_event *event);
int epoll_wait(int epfd, struct_epoll_event *events, int maxevents, int timeout);

int fflush(FILE *stream);
int fchdir(int fd);
int fchmod(int fd, mode_t mode);
int fchown(int fd, uid_t owner, gid_t group);
int fclose(FILE *stream);
FILE *fdopen(int fd, const char *mode);
int ferror(FILE *stream);
int feof(FILE *stream);
int fgetc(FILE *stream);
char* fgets(char *str, int count, FILE *stream);
int flock(int fd, int operation);
FILE *fopen(const char *filename, const char *mode);
FILE *fopen64(const char *filename, const char *mode);
pid_t fork();
int fputc(int c, FILE *stream);
int fputs(const char *s, FILE *stream);
size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
void free(void *ptr);
int fseek(FILE *fp, long offset, int whence);
int fseeko(FILE *fp, off_t offset, int whence);
int fseeko64(FILE *fp, off64_t offset, int whence);
int fsync(int fd);
long ftell(FILE *stream);
off_t ftello(FILE *stream);
off64_t ftello64(FILE *stream);
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);

int getc(FILE *stream);
int getchar();
char *getcwd(char *buf, size_t size);
uid_t geteuid();
gid_t getegid();
gid_t getgid();
int gethostname(char *name, size_t len);
ssize_t getline(char **restrict lineptr, size_t *restrict n, FILE *restrict stream);
int getpeername(int sockfd, struct_sockaddr *restrict addr, socklen_t *restrict addrlen);
pid_t getpid();
int getrlimit(int resource, struct_rlimit *rlim);
char *gets(char *s);
int getsockname(int sockfd, struct_sockaddr *restrict addr, socklen_t *restrict addrlen);
int getsockopt(int sockfd, int level, int optname, void *optval, socklen_t *optlen);
int gettimeofday(struct_timeval *restrict tv, struct_timezone *restrict tz);
uid_t getuid();

uint32_t htonl(uint32_t hostlong);
uint16_t htons(uint16_t hostshort);

uint32_t inet_addr(const char *cp);
char *inet_ntoa(struct_in_addr in);
int inet_pton(int af, const char *restrict src, void *restrict dst);
int isatty(int fd);

int kill(pid_t pid, int sig);

int link(const char *oldpath, const char *newpath);
int listen(int sockfd, int backlog);
off_t lseek(int fd, off_t offset, int whence);

void* malloc(size_t numbytes);
void* memchr(const void *str, int c, size_t n);
void* memcpy(void *dest, const void *src, size_t n);
void* memmove(void *dest, const void *src, size_t n);
void* memset(void *s, int c, size_t n);
int mkdir(const char *path, mode_t mode);
int mkstemp64(char * template);

uint32_t ntohl(uint32_t netlong);
uint16_t ntohs(uint16_t netshort);

int open64(const char *pathname, int oflag); // actually vararg
DIR *opendir(const char *dirname);

int pclose(FILE *stream);
pcre *pcre_compile(const char *pattern, int options, const char **errptr, int *erroffset, const unsigned char *tableptr);
int pcre_exec(const pcre *code, const pcre_extra *extra, const char *subject, int length, int startoffset, int options, int *ovector, int ovecsize);
int pcre_get_substring(const char *subject, int *ovector, int stringcount, int stringnumber, const char **stringptr);
pcre_extra *pcre_study(const pcre *code, int options, const char **errptr);
int poll(struct_pollfd *fds, nfds_t nfds, int timeout);
FILE *popen(const char *command, const char *mode);
int putc(int char, FILE *stream);

int rand();
long read(int fd, void *buf, size_t count);
struct_dirent *readdir(DIR *dirp);
struct_dirent64 *readdir64(DIR *dirp);
ssize_t readlink(const char *path, char *buf, size_t bufsiz);
void *realloc(void *ptr, size_t size);
int regcomp(regex_t *restrict preg, const char *restrict pattern, int cflags);
size_t regerror(int errcode, const regex_t *preg, char *errbuf, size_t errbuf_size);
int regexec(const regex_t *preg, const char *string, size_t nmatch, regmatch_t *pmatch, int eflags);
void regfree(regex_t *preg);
int rename(const char *old, const char *new);
int rmdir(const char *path);

int select(int nfds, fd_set *readfds, fd_set *writefds, fd_set *exceptfds, struct_timeval *timeout);
ssize_t send(int sockfd, const void *buf, size_t len, int flags);
ssize_t sendto(int sockfd, const void *buf, size_t len, int flags, const struct_sockaddr *dest_addr, socklen_t addrlen);
ssize_t sendmsg(int sockfd, const struct_msghdr *msg, int flags);
int setsockopt(int sockfd, int level, int optname, const void *optval, socklen_t optlen);
int setenv(const char *name, const char *value, int overwrite);
int setgid(gid_t gid);
int setpgrp();
int setrlimit(int resource, const struct_rlimit *rlim);
int settimeofday(const struct_timeval *tv, const struct_timezone *tz);
int setuid(uid_t uid);
int setvbuf(FILE *stream, char *buffer, int mode, size_t size);
void siglongjmp(sigjmp_buf env, int val);
sighandler_t signal(int signum, sighandler_t handler);
unsigned sleep(unsigned seconds);
int socket(int domain, int type, int protocol);
void srand(unsigned int seed);
int strcasecmp(const char *s1, const char *s2);
char *strdup(const char *s);
int strncasecmp(const char *s1, const char *s2, size_t n);
char *strcat(char *destination, const char *source);
char *strchr(const char *s, int c);
int strcmp(const char *s1, const char *s2);
char *strcpy(char *dest, const char *src);
size_t strcspn(const char *str1, const char* str2);
size_t strlen(const char *s);
char *strncat(char *dest, const char *src, size_t n);
int strncmp(const char *s1, const char *s2, size_t n);
char *strncpy(char *destination, const char *source, size_t num);
char *strrchr(const char *s, int c);
size_t strspn(const char *str1, const char *str2);
char *strstr(const char *haystack, const char *needle);
char* strtok(char *str, const char *delimiters );
int system(const char *command);

int tcgetattr(int fd, struct_termios *termios_p);
time_t time(time_t *);
int truncate(const char *path, off_t length);
void tzset();

mode_t umask(mode_t mask);
int ungetc(int c, FILE *stream);
int unlink(const char *path);
int unsetenv(const char *name);
int utime(const char *filename, const struct_utimbuf *times);

void vsnsprintf(char *buff, const char *str, va_list args);

long write(int fd, void *buf, size_t count);

int yp_all(char *indomain, char *inmap, struct_ypall_callback *incallback);
int yp_first(char *indomain, char *inmap, char **outkey, int *outkeylen, char **outval, int *outvallen);
int yp_get_default_domain(char **outdomain);
int yp_master(char *indomain, char *inmap, char **outname);
int yp_match(char *indomain, char *inmap, char *inkey, int inkeylen, char **outval, int *outvallen);
int yp_next(char *indomain, char *inmap, char *inkey, int inkeylen, char **outkey, int *outkeylen, char **outval, int *outvallen);
int yp_order(char *indomain, char *inmap, unsigned long *outorder);
char *yperr_string(int incode);
int ypprot_err(int *domain);
