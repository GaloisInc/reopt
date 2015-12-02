# 1 "syscalls.master"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 157 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "syscalls.master" 2
 $FreeBSD: release/10.0.0/sys/kern/syscalls.master 255708 2013-09-19 18:53:42Z jhb $
; from: @(#)syscalls.master 8.2 (Berkeley) 1/13/94
;
; System call name/number master file.
; Processed to created init_sysent.c, syscalls.c and syscall.h.

; Columns: number audit type name alt{name,tag,rtyp}/comments
; number system call number, must be in order
; audit the audit event associated with the system call
; A value of AUE_NULL means no auditing, but it also means that
; there is no audit event for the call at this time. For the
; case where the event exists, but we don't want auditing, the
; event should be #defined to AUE_NULL in audit_kevents.h.
; type one of STD, OBSOL, UNIMPL, COMPAT, COMPAT4, COMPAT6,
; COMPAT7, NODEF, NOARGS, NOPROTO, NOSTD
; The COMPAT* options may be combined with one or more NO*
; options separated by '|' with no spaces (e.g. COMPAT|NOARGS)
; name psuedo-prototype of syscall routine
; If one of the following alts is different, then all appear:
; altname name of system call if different
; alttag name of args struct tag if different from [o]`name'"_args"
; altrtyp return type if not int (bogus - syscalls always return int)
; for UNIMPL/OBSOL, name continues with comments

; types:
; STD always included
; COMPAT included on COMPAT #ifdef
; COMPAT4 included on COMPAT4 #ifdef (FreeBSD 4 compat)
; COMPAT6 included on COMPAT6 #ifdef (FreeBSD 6 compat)
; COMPAT7 included on COMPAT7 #ifdef (FreeBSD 7 compat)
; OBSOL obsolete, not included in system, only specifies name
; UNIMPL not implemented, placeholder only
; NOSTD implemented but as a lkm that can be statically
; compiled in; sysent entry will be filled with lkmressys
; so the SYSCALL_MODULE macro works
; NOARGS same as STD except do not create structure in sys/sysproto.h
; NODEF same as STD except only have the entry in the syscall table
; added. Meaning - do not create structure or function
; prototype in sys/sysproto.h
; NOPROTO same as STD except do not create structure or
; function prototype in sys/sysproto.h. Does add a
; definition to syscall.h besides adding a sysent.
; NOTSTATIC syscall is loadable
;
; Please copy any additions and changes to the following compatability tables:
; sys/compat/freebsd32/syscalls.master

; #ifdef's, etc. may be included, and are copied to the output files.


# 1 "/usr/include/sys/param.h" 1 3 4
# 41 "/usr/include/sys/param.h" 3 4
# 1 "/usr/include/sys/_null.h" 1 3 4
# 42 "/usr/include/sys/param.h" 2 3 4
# 88 "/usr/include/sys/param.h" 3 4
# 1 "/usr/include/sys/types.h" 1 3 4
# 41 "/usr/include/sys/types.h" 3 4
# 1 "/usr/include/sys/cdefs.h" 1 3 4
# 42 "/usr/include/sys/types.h" 2 3 4


# 1 "/usr/include/machine/endian.h" 1 3 4





# 1 "/usr/include/x86/endian.h" 1 3 4
# 37 "/usr/include/x86/endian.h" 3 4
# 1 "/usr/include/sys/_types.h" 1 3 4
# 33 "/usr/include/sys/_types.h" 3 4
# 1 "/usr/include/machine/_types.h" 1 3 4





# 1 "/usr/include/x86/_types.h" 1 3 4
# 51 "/usr/include/x86/_types.h" 3 4
typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef short __int16_t;
typedef unsigned short __uint16_t;
typedef int __int32_t;
typedef unsigned int __uint32_t;

typedef long __int64_t;
typedef unsigned long __uint64_t;
# 77 "/usr/include/x86/_types.h" 3 4
typedef __int32_t __clock_t;
typedef __int64_t __critical_t;
typedef double __double_t;
typedef float __float_t;
typedef __int64_t __intfptr_t;
typedef __int64_t __intptr_t;
# 91 "/usr/include/x86/_types.h" 3 4
typedef __int64_t __intmax_t;
typedef __int32_t __int_fast8_t;
typedef __int32_t __int_fast16_t;
typedef __int32_t __int_fast32_t;
typedef __int64_t __int_fast64_t;
typedef __int8_t __int_least8_t;
typedef __int16_t __int_least16_t;
typedef __int32_t __int_least32_t;
typedef __int64_t __int_least64_t;

typedef __int64_t __ptrdiff_t;
typedef __int64_t __register_t;
typedef __int64_t __segsz_t;
typedef __uint64_t __size_t;
typedef __int64_t __ssize_t;
typedef __int64_t __time_t;
typedef __uint64_t __uintfptr_t;
typedef __uint64_t __uintptr_t;
# 119 "/usr/include/x86/_types.h" 3 4
typedef __uint64_t __uintmax_t;
typedef __uint32_t __uint_fast8_t;
typedef __uint32_t __uint_fast16_t;
typedef __uint32_t __uint_fast32_t;
typedef __uint64_t __uint_fast64_t;
typedef __uint8_t __uint_least8_t;
typedef __uint16_t __uint_least16_t;
typedef __uint32_t __uint_least32_t;
typedef __uint64_t __uint_least64_t;

typedef __uint64_t __u_register_t;
typedef __uint64_t __vm_offset_t;
typedef __uint64_t __vm_paddr_t;
typedef __uint64_t __vm_size_t;
# 143 "/usr/include/x86/_types.h" 3 4
typedef __int64_t __vm_ooffset_t;
typedef __uint64_t __vm_pindex_t;
typedef int __wchar_t;
# 154 "/usr/include/x86/_types.h" 3 4
typedef __builtin_va_list __va_list;






typedef __va_list __gnuc_va_list;
# 7 "/usr/include/machine/_types.h" 2 3 4
# 34 "/usr/include/sys/_types.h" 2 3 4




typedef __uint32_t __blksize_t;
typedef __int64_t __blkcnt_t;
typedef __int32_t __clockid_t;
typedef __uint32_t __fflags_t;
typedef __uint64_t __fsblkcnt_t;
typedef __uint64_t __fsfilcnt_t;
typedef __uint32_t __gid_t;
typedef __int64_t __id_t;
typedef __uint32_t __ino_t;
typedef long __key_t;
typedef __int32_t __lwpid_t;
typedef __uint16_t __mode_t;
typedef int __accmode_t;
typedef int __nl_item;
typedef __uint16_t __nlink_t;
typedef __int64_t __off_t;
typedef __int32_t __pid_t;
typedef __int64_t __rlim_t;


typedef __uint8_t __sa_family_t;
typedef __uint32_t __socklen_t;
typedef long __suseconds_t;
typedef struct __timer *__timer_t;
typedef struct __mq *__mqd_t;
typedef __uint32_t __uid_t;
typedef unsigned int __useconds_t;
typedef int __cpuwhich_t;
typedef int __cpulevel_t;
typedef int __cpusetid_t;
# 87 "/usr/include/sys/_types.h" 3 4
typedef int __ct_rune_t;
typedef __ct_rune_t __rune_t;
typedef __ct_rune_t __wint_t;



typedef __uint_least16_t __char16_t;
typedef __uint_least32_t __char32_t;







typedef __uint32_t __dev_t;

typedef __uint32_t __fixpt_t;





typedef union {
 char __mbstate8[128];
 __int64_t _mbstateL;
} __mbstate_t;
# 38 "/usr/include/x86/endian.h" 2 3 4
# 91 "/usr/include/x86/endian.h" 3 4
static __inline __uint16_t
__bswap16_var(__uint16_t _x)
{

 return ((__uint16_t)((_x) << 8 | (_x) >> 8));
}

static __inline __uint32_t
__bswap32_var(__uint32_t _x)
{


 __asm("bswap %0" : "+r" (_x));
 return (_x);



}

static __inline __uint64_t
__bswap64_var(__uint64_t _x)
{


 __asm("bswap %0" : "+r" (_x));
 return (_x);







}
# 7 "/usr/include/machine/endian.h" 2 3 4
# 45 "/usr/include/sys/types.h" 2 3 4


# 1 "/usr/include/sys/_pthreadtypes.h" 1 3 4
# 44 "/usr/include/sys/_pthreadtypes.h" 3 4
struct pthread;
struct pthread_attr;
struct pthread_cond;
struct pthread_cond_attr;
struct pthread_mutex;
struct pthread_mutex_attr;
struct pthread_once;
struct pthread_rwlock;
struct pthread_rwlockattr;
struct pthread_barrier;
struct pthread_barrier_attr;
struct pthread_spinlock;
# 65 "/usr/include/sys/_pthreadtypes.h" 3 4
typedef struct pthread *pthread_t;


typedef struct pthread_attr *pthread_attr_t;
typedef struct pthread_mutex *pthread_mutex_t;
typedef struct pthread_mutex_attr *pthread_mutexattr_t;
typedef struct pthread_cond *pthread_cond_t;
typedef struct pthread_cond_attr *pthread_condattr_t;
typedef int pthread_key_t;
typedef struct pthread_once pthread_once_t;
typedef struct pthread_rwlock *pthread_rwlock_t;
typedef struct pthread_rwlockattr *pthread_rwlockattr_t;
typedef struct pthread_barrier *pthread_barrier_t;
typedef struct pthread_barrierattr *pthread_barrierattr_t;
typedef struct pthread_spinlock *pthread_spinlock_t;







typedef void *pthread_addr_t;
typedef void *(*pthread_startroutine_t)(void *);




struct pthread_once {
 int state;
 pthread_mutex_t mutex;
};
# 48 "/usr/include/sys/types.h" 2 3 4


typedef unsigned char u_char;
typedef unsigned short u_short;
typedef unsigned int u_int;
typedef unsigned long u_long;

typedef unsigned short ushort;
typedef unsigned int uint;







# 1 "/usr/include/sys/_stdint.h" 1 3 4
# 34 "/usr/include/sys/_stdint.h" 3 4
typedef __int8_t int8_t;




typedef __int16_t int16_t;




typedef __int32_t int32_t;




typedef __int64_t int64_t;




typedef __uint8_t uint8_t;




typedef __uint16_t uint16_t;




typedef __uint32_t uint32_t;




typedef __uint64_t uint64_t;




typedef __intptr_t intptr_t;



typedef __uintptr_t uintptr_t;
# 64 "/usr/include/sys/types.h" 2 3 4

typedef __uint8_t u_int8_t;
typedef __uint16_t u_int16_t;
typedef __uint32_t u_int32_t;
typedef __uint64_t u_int64_t;

typedef __uint64_t u_quad_t;
typedef __int64_t quad_t;
typedef quad_t * qaddr_t;

typedef char * caddr_t;
typedef const char * c_caddr_t;


typedef __blksize_t blksize_t;



typedef __cpuwhich_t cpuwhich_t;
typedef __cpulevel_t cpulevel_t;
typedef __cpusetid_t cpusetid_t;


typedef __blkcnt_t blkcnt_t;




typedef __clock_t clock_t;




typedef __clockid_t clockid_t;



typedef __critical_t critical_t;
typedef __int64_t daddr_t;


typedef __dev_t dev_t;




typedef __fflags_t fflags_t;



typedef __fixpt_t fixpt_t;


typedef __fsblkcnt_t fsblkcnt_t;
typedef __fsfilcnt_t fsfilcnt_t;




typedef __gid_t gid_t;




typedef __uint32_t in_addr_t;




typedef __uint16_t in_port_t;




typedef __id_t id_t;




typedef __ino_t ino_t;




typedef __key_t key_t;




typedef __lwpid_t lwpid_t;




typedef __mode_t mode_t;




typedef __accmode_t accmode_t;




typedef __nlink_t nlink_t;




typedef __off_t off_t;




typedef __pid_t pid_t;



typedef __register_t register_t;


typedef __rlim_t rlim_t;



typedef __int64_t sbintime_t;

typedef __segsz_t segsz_t;


typedef __size_t size_t;




typedef __ssize_t ssize_t;




typedef __suseconds_t suseconds_t;




typedef __time_t time_t;




typedef __timer_t timer_t;




typedef __mqd_t mqd_t;



typedef __u_register_t u_register_t;


typedef __uid_t uid_t;




typedef __useconds_t useconds_t;





struct cap_rights;

typedef struct cap_rights cap_rights_t;


typedef __vm_offset_t vm_offset_t;
typedef __vm_ooffset_t vm_ooffset_t;
typedef __vm_paddr_t vm_paddr_t;
typedef __vm_pindex_t vm_pindex_t;
typedef __vm_size_t vm_size_t;
# 290 "/usr/include/sys/types.h" 3 4
# 1 "/usr/include/sys/select.h" 1 3 4
# 38 "/usr/include/sys/select.h" 3 4
# 1 "/usr/include/sys/_sigset.h" 1 3 4
# 51 "/usr/include/sys/_sigset.h" 3 4
typedef struct __sigset {
 __uint32_t __bits[4];
} __sigset_t;
# 39 "/usr/include/sys/select.h" 2 3 4
# 1 "/usr/include/sys/_timeval.h" 1 3 4
# 47 "/usr/include/sys/_timeval.h" 3 4
struct timeval {
 time_t tv_sec;
 suseconds_t tv_usec;
};
# 40 "/usr/include/sys/select.h" 2 3 4
# 1 "/usr/include/sys/timespec.h" 1 3 4
# 38 "/usr/include/sys/timespec.h" 3 4
# 1 "/usr/include/sys/_timespec.h" 1 3 4
# 44 "/usr/include/sys/_timespec.h" 3 4
struct timespec {
 time_t tv_sec;
 long tv_nsec;
};
# 39 "/usr/include/sys/timespec.h" 2 3 4
# 58 "/usr/include/sys/timespec.h" 3 4
struct itimerspec {
 struct timespec it_interval;
 struct timespec it_value;
};
# 41 "/usr/include/sys/select.h" 2 3 4

typedef unsigned long __fd_mask;

typedef __fd_mask fd_mask;




typedef __sigset_t sigset_t;
# 71 "/usr/include/sys/select.h" 3 4
typedef struct fd_set {
 __fd_mask __fds_bits[(((1024U) + (((sizeof(__fd_mask) * 8)) - 1)) / ((sizeof(__fd_mask) * 8)))];
} fd_set;
# 98 "/usr/include/sys/select.h" 3 4
int pselect(int, fd_set *restrict, fd_set *restrict, fd_set *restrict,
 const struct timespec *restrict, const sigset_t *restrict);



int select(int, fd_set *, fd_set *, fd_set *, struct timeval *);
# 291 "/usr/include/sys/types.h" 2 3 4
# 310 "/usr/include/sys/types.h" 3 4
int ftruncate(int, off_t);



off_t lseek(int, off_t, int);



void * mmap(void *, size_t, int, int, int, off_t);



int truncate(const char *, off_t);
# 89 "/usr/include/sys/param.h" 2 3 4








# 1 "/usr/include/sys/syslimits.h" 1 3 4
# 98 "/usr/include/sys/param.h" 2 3 4
# 129 "/usr/include/sys/param.h" 3 4
# 1 "/usr/include/sys/signal.h" 1 3 4
# 45 "/usr/include/sys/signal.h" 3 4
# 1 "/usr/include/machine/_limits.h" 1 3 4





# 1 "/usr/include/x86/_limits.h" 1 3 4
# 7 "/usr/include/machine/_limits.h" 2 3 4
# 46 "/usr/include/sys/signal.h" 2 3 4
# 1 "/usr/include/machine/signal.h" 1 3 4





# 1 "/usr/include/x86/signal.h" 1 3 4
# 45 "/usr/include/x86/signal.h" 3 4
# 1 "/usr/include/machine/trap.h" 1 3 4





# 1 "/usr/include/x86/trap.h" 1 3 4
# 7 "/usr/include/machine/trap.h" 2 3 4
# 46 "/usr/include/x86/signal.h" 2 3 4
# 103 "/usr/include/x86/signal.h" 3 4
typedef long sig_atomic_t;
# 116 "/usr/include/x86/signal.h" 3 4
struct sigcontext {
 struct __sigset sc_mask;
 long sc_onstack;
 long sc_rdi;
 long sc_rsi;
 long sc_rdx;
 long sc_rcx;
 long sc_r8;
 long sc_r9;
 long sc_rax;
 long sc_rbx;
 long sc_rbp;
 long sc_r10;
 long sc_r11;
 long sc_r12;
 long sc_r13;
 long sc_r14;
 long sc_r15;
 int sc_trapno;
 short sc_fs;
 short sc_gs;
 long sc_addr;
 int sc_flags;
 short sc_es;
 short sc_ds;
 long sc_err;
 long sc_rip;
 long sc_cs;
 long sc_rflags;
 long sc_rsp;
 long sc_ss;
 long sc_len;




 long sc_fpformat;
 long sc_ownedfp;
 long sc_fpstate[64] __attribute__((__aligned__(16)));

 long sc_fsbase;
 long sc_gsbase;

 long sc_xfpustate;
 long sc_xfpustate_len;

 long sc_spare[4];
};
# 7 "/usr/include/machine/signal.h" 2 3 4
# 47 "/usr/include/sys/signal.h" 2 3 4
# 142 "/usr/include/sys/signal.h" 3 4
typedef void __sighandler_t(int);
# 152 "/usr/include/sys/signal.h" 3 4
union sigval {

 int sival_int;
 void *sival_ptr;

 int sigval_int;
 void *sigval_ptr;
};



struct sigevent {
 int sigev_notify;
 int sigev_signo;
 union sigval sigev_value;
 union {
  __lwpid_t _threadid;
  struct {
   void (*_function)(union sigval);
   void *_attribute;
  } _sigev_thread;
  unsigned short _kevent_flags;
  long __spare__[8];
 } _sigev_un;
};
# 196 "/usr/include/sys/signal.h" 3 4
typedef struct __siginfo {
 int si_signo;
 int si_errno;






 int si_code;
 __pid_t si_pid;
 __uid_t si_uid;
 int si_status;
 void *si_addr;
 union sigval si_value;
 union {
  struct {
   int _trapno;
  } _fault;
  struct {
   int _timerid;
   int _overrun;
  } _timer;
  struct {
   int _mqd;
  } _mesgq;
  struct {
   long _band;
  } _poll;
  struct {
   long __spare1__;
   int __spare2__[7];
  } __spare__;
 } _reason;
} siginfo_t;
# 295 "/usr/include/sys/signal.h" 3 4
struct __siginfo;




struct sigaction {
 union {
  void (*__sa_handler)(int);
  void (*__sa_sigaction)(int, struct __siginfo *, void *);
 } __sigaction_u;
 int sa_flags;
 sigset_t sa_mask;
};
# 352 "/usr/include/sys/signal.h" 3 4
typedef __sighandler_t *sig_t;
typedef void __siginfohandler_t(int, struct __siginfo *, void *);







typedef struct sigaltstack {



 char *ss_sp;
 __size_t ss_size;
 int ss_flags;
} stack_t;
# 381 "/usr/include/sys/signal.h" 3 4
struct sigvec {
 __sighandler_t *sv_handler;
 int sv_mask;
 int sv_flags;
};
# 408 "/usr/include/sys/signal.h" 3 4
struct sigstack {

 char *ss_sp;
 int ss_onstack;
};
# 441 "/usr/include/sys/signal.h" 3 4
__sighandler_t *signal(int, __sighandler_t *);
# 130 "/usr/include/sys/param.h" 2 3 4



# 1 "/usr/include/machine/param.h" 1 3 4
# 46 "/usr/include/machine/param.h" 3 4
# 1 "/usr/include/machine/_align.h" 1 3 4





# 1 "/usr/include/x86/_align.h" 1 3 4
# 7 "/usr/include/machine/_align.h" 2 3 4
# 47 "/usr/include/machine/param.h" 2 3 4
# 134 "/usr/include/sys/param.h" 2 3 4

# 1 "/usr/include/sys/limits.h" 1 3 4
# 36 "/usr/include/sys/limits.h" 3 4
# 1 "/usr/include/machine/_limits.h" 1 3 4
# 37 "/usr/include/sys/limits.h" 2 3 4
# 136 "/usr/include/sys/param.h" 2 3 4
# 51 "syscalls.master" 2
# 1 "/usr/include/sys/sysent.h" 1 3 4
# 35 "/usr/include/sys/sysent.h" 3 4
# 1 "/usr/include/bsm/audit.h" 1 3 4
# 171 "/usr/include/bsm/audit.h" 3 4
typedef uid_t au_id_t;
typedef pid_t au_asid_t;
typedef u_int16_t au_event_t;
typedef u_int16_t au_emod_t;
typedef u_int32_t au_class_t;
typedef u_int64_t au_asflgs_t __attribute__ ((aligned (8)));

struct au_tid {
 dev_t port;
 u_int32_t machine;
};
typedef struct au_tid au_tid_t;

struct au_tid_addr {
 dev_t at_port;
 u_int32_t at_type;
 u_int32_t at_addr[4];
};
typedef struct au_tid_addr au_tid_addr_t;

struct au_mask {
 unsigned int am_success;
 unsigned int am_failure;
};
typedef struct au_mask au_mask_t;

struct auditinfo {
 au_id_t ai_auid;
 au_mask_t ai_mask;
 au_tid_t ai_termid;
 au_asid_t ai_asid;
};
typedef struct auditinfo auditinfo_t;

struct auditinfo_addr {
 au_id_t ai_auid;
 au_mask_t ai_mask;
 au_tid_addr_t ai_termid;
 au_asid_t ai_asid;
 au_asflgs_t ai_flags;
};
typedef struct auditinfo_addr auditinfo_addr_t;

struct auditpinfo {
 pid_t ap_pid;
 au_id_t ap_auid;
 au_mask_t ap_mask;
 au_tid_t ap_termid;
 au_asid_t ap_asid;
};
typedef struct auditpinfo auditpinfo_t;

struct auditpinfo_addr {
 pid_t ap_pid;
 au_id_t ap_auid;
 au_mask_t ap_mask;
 au_tid_addr_t ap_termid;
 au_asid_t ap_asid;
 au_asflgs_t ap_flags;
};
typedef struct auditpinfo_addr auditpinfo_addr_t;

struct au_session {
 auditinfo_addr_t *as_aia_p;
 au_mask_t as_mask;
};
typedef struct au_session au_session_t;




typedef struct au_token token_t;
# 252 "/usr/include/bsm/audit.h" 3 4
struct au_qctrl {
 int aq_hiwater;


 int aq_lowater;


 int aq_bufsz;
 int aq_delay;
 int aq_minfree;
};
typedef struct au_qctrl au_qctrl_t;




struct audit_stat {
 unsigned int as_version;
 unsigned int as_numevent;
 int as_generated;
 int as_nonattrib;
 int as_kernel;
 int as_audit;
 int as_auditctl;
 int as_enqueue;
 int as_written;
 int as_wblocked;
 int as_rblocked;
 int as_dropped;
 int as_totalsize;
 unsigned int as_memused;
};
typedef struct audit_stat au_stat_t;




struct audit_fstat {
 u_int64_t af_filesz;
 u_int64_t af_currsz;
};
typedef struct audit_fstat au_fstat_t;




struct au_evclass_map {
 au_event_t ec_number;
 au_class_t ec_class;
};
typedef struct au_evclass_map au_evclass_map_t;





int audit(const void *, int);
int auditon(int, void *, int);
int auditctl(const char *);
int getauid(au_id_t *);
int setauid(const au_id_t *);
int getaudit(struct auditinfo *);
int setaudit(const struct auditinfo *);
int getaudit_addr(struct auditinfo_addr *, int);
int setaudit_addr(const struct auditinfo_addr *, int);
# 36 "/usr/include/sys/sysent.h" 2 3 4

struct rlimit;
struct sysent;
struct thread;
struct ksiginfo;

typedef int sy_call_t(struct thread *, void *);


typedef void (*systrace_probe_func_t)(u_int32_t, int, struct sysent *, void *,
    int);





typedef void (*systrace_args_func_t)(int, void *, u_int64_t *, int *);

extern systrace_probe_func_t systrace_probe_func;

struct sysent {
 int sy_narg;
 sy_call_t *sy_call;
 au_event_t sy_auevent;
 systrace_args_func_t sy_systrace_args_func;

 u_int32_t sy_entry;
 u_int32_t sy_return;
 u_int32_t sy_flags;
 u_int32_t sy_thrcnt;
};
# 79 "/usr/include/sys/sysent.h" 3 4
struct image_params;
struct __sigset;
struct syscall_args;
struct trapframe;
struct vnode;

struct sysentvec {
 int sv_size;
 struct sysent *sv_table;
 u_int sv_mask;
 int sv_sigsize;
 int *sv_sigtbl;
 int sv_errsize;
 int *sv_errtbl;
 int (*sv_transtrap)(int, int);

 int (*sv_fixup)(register_t **, struct image_params *);

 void (*sv_sendsig)(void (*)(int), struct ksiginfo *, struct __sigset *);

 char *sv_sigcode;
 int *sv_szsigcode;
 void (*sv_prepsyscall)(struct trapframe *, int *, u_int *,
       caddr_t *);
 char *sv_name;
 int (*sv_coredump)(struct thread *, struct vnode *, off_t, int);

 int (*sv_imgact_try)(struct image_params *);
 int sv_minsigstksz;
 int sv_pagesize;
 vm_offset_t sv_minuser;
 vm_offset_t sv_maxuser;
 vm_offset_t sv_usrstack;
 vm_offset_t sv_psstrings;
 int sv_stackprot;
 register_t *(*sv_copyout_strings)(struct image_params *);
 void (*sv_setregs)(struct thread *, struct image_params *,
       u_long);
 void (*sv_fixlimit)(struct rlimit *, int);
 u_long *sv_maxssiz;
 u_int sv_flags;
 void (*sv_set_syscall_retval)(struct thread *, int);
 int (*sv_fetch_syscall_args)(struct thread *, struct
       syscall_args *);
 const char **sv_syscallnames;
 vm_offset_t sv_shared_page_base;
 vm_offset_t sv_shared_page_len;
 vm_offset_t sv_sigcode_base;
 vm_offset_t sv_timekeep_base;
 int sv_timekeep_off;
 int sv_timekeep_curr;
 uint32_t sv_timekeep_gen;
 void *sv_shared_page_obj;
 void (*sv_schedtail)(struct thread *);
};
# 52 "syscalls.master" 2
# 1 "/usr/include/sys/sysproto.h" 1 3 4
# 13 "/usr/include/sys/sysproto.h" 3 4
# 1 "/usr/include/sys/acl.h" 1 3 4
# 40 "/usr/include/sys/acl.h" 3 4
# 1 "/usr/include/sys/queue.h" 1 3 4
# 41 "/usr/include/sys/acl.h" 2 3 4
# 1 "/usr/include/vm/uma.h" 1 3 4
# 40 "/usr/include/vm/uma.h" 3 4
# 1 "/usr/include/sys/malloc.h" 1 3 4
# 40 "/usr/include/sys/malloc.h" 3 4
# 1 "/usr/include/sys/_lock.h" 1 3 4
# 34 "/usr/include/sys/_lock.h" 3 4
struct lock_object {
 const char *lo_name;
 u_int lo_flags;
 u_int lo_data;
 struct witness *lo_witness;
};
# 41 "/usr/include/sys/malloc.h" 2 3 4
# 1 "/usr/include/sys/_mutex.h" 1 3 4
# 45 "/usr/include/sys/_mutex.h" 3 4
struct mtx {
 struct lock_object lock_object;
 volatile uintptr_t mtx_lock;
};
# 59 "/usr/include/sys/_mutex.h" 3 4
struct mtx_padalign {
 struct lock_object lock_object;
 volatile uintptr_t mtx_lock;
} __attribute__((__aligned__((1 << 7))));
# 42 "/usr/include/sys/malloc.h" 2 3 4
# 75 "/usr/include/sys/malloc.h" 3 4
struct malloc_type_stats {
 uint64_t mts_memalloced;
 uint64_t mts_memfreed;
 uint64_t mts_numallocs;
 uint64_t mts_numfrees;
 uint64_t mts_size;
 uint64_t _mts_reserved1;
 uint64_t _mts_reserved2;
 uint64_t _mts_reserved3;
};
# 93 "/usr/include/sys/malloc.h" 3 4
struct malloc_type_internal {
 uint32_t mti_probes[2];

 u_char mti_zone;
 struct malloc_type_stats mti_stats[1];
};






struct malloc_type {
 struct malloc_type *ks_next;
 u_long ks_magic;
 const char *ks_shortdesc;
 void *ks_handle;
};
# 120 "/usr/include/sys/malloc.h" 3 4
struct malloc_type_stream_header {
 uint32_t mtsh_version;
 uint32_t mtsh_maxcpus;
 uint32_t mtsh_count;
 uint32_t _mtsh_pad;
};


struct malloc_type_header {
 char mth_name[32];
};
# 41 "/usr/include/vm/uma.h" 2 3 4






struct uma_zone;

typedef struct uma_zone * uma_zone_t;

void zone_drain(uma_zone_t);
# 70 "/usr/include/vm/uma.h" 3 4
typedef int (*uma_ctor)(void *mem, int size, void *arg, int flags);
# 88 "/usr/include/vm/uma.h" 3 4
typedef void (*uma_dtor)(void *mem, int size, void *arg);
# 107 "/usr/include/vm/uma.h" 3 4
typedef int (*uma_init)(void *mem, int size, int flags);
# 124 "/usr/include/vm/uma.h" 3 4
typedef void (*uma_fini)(void *mem, int size);




typedef int (*uma_import)(void *arg, void **store, int count, int flags);




typedef void (*uma_release)(void *arg, void **store, int count);
# 178 "/usr/include/vm/uma.h" 3 4
uma_zone_t uma_zcreate(const char *name, size_t size, uma_ctor ctor,
      uma_dtor dtor, uma_init uminit, uma_fini fini,
      int align, uint32_t flags);
# 214 "/usr/include/vm/uma.h" 3 4
uma_zone_t uma_zsecond_create(char *name, uma_ctor ctor, uma_dtor dtor,
      uma_init zinit, uma_fini zfini, uma_zone_t master);
# 226 "/usr/include/vm/uma.h" 3 4
int uma_zsecond_add(uma_zone_t zone, uma_zone_t master);
# 237 "/usr/include/vm/uma.h" 3 4
uma_zone_t uma_zcache_create(char *name, int size, uma_ctor ctor, uma_dtor dtor,
      uma_init zinit, uma_fini zfini, uma_import zimport,
      uma_release zrelease, void *arg, int flags);
# 307 "/usr/include/vm/uma.h" 3 4
void uma_zdestroy(uma_zone_t zone);
# 323 "/usr/include/vm/uma.h" 3 4
void *uma_zalloc_arg(uma_zone_t zone, void *arg, int flags);







static __inline void *uma_zalloc(uma_zone_t zone, int flags);

static __inline void *
uma_zalloc(uma_zone_t zone, int flags)
{
 return uma_zalloc_arg(zone, ((void *)0), flags);
}
# 351 "/usr/include/vm/uma.h" 3 4
void uma_zfree_arg(uma_zone_t zone, void *item, void *arg);







static __inline void uma_zfree(uma_zone_t zone, void *item);

static __inline void
uma_zfree(uma_zone_t zone, void *item)
{
 uma_zfree_arg(zone, item, ((void *)0));
}
# 385 "/usr/include/vm/uma.h" 3 4
typedef void *(*uma_alloc)(uma_zone_t zone, int size, uint8_t *pflag, int wait);
# 398 "/usr/include/vm/uma.h" 3 4
typedef void (*uma_free)(void *item, int size, uint8_t pflag);
# 418 "/usr/include/vm/uma.h" 3 4
void uma_startup(void *bootmem, int boot_pages);
# 434 "/usr/include/vm/uma.h" 3 4
void uma_startup2(void);
# 447 "/usr/include/vm/uma.h" 3 4
void uma_reclaim(void);
# 459 "/usr/include/vm/uma.h" 3 4
void uma_set_align(int align);





void uma_zone_reserve(uma_zone_t zone, int nitems);
# 484 "/usr/include/vm/uma.h" 3 4
int uma_zone_reserve_kva(uma_zone_t zone, int nitems);
# 496 "/usr/include/vm/uma.h" 3 4
int uma_zone_set_max(uma_zone_t zone, int nitems);
# 508 "/usr/include/vm/uma.h" 3 4
int uma_zone_get_max(uma_zone_t zone);
# 520 "/usr/include/vm/uma.h" 3 4
void uma_zone_set_warning(uma_zone_t zone, const char *warning);
# 531 "/usr/include/vm/uma.h" 3 4
int uma_zone_get_cur(uma_zone_t zone);
# 541 "/usr/include/vm/uma.h" 3 4
void uma_zone_set_init(uma_zone_t zone, uma_init uminit);
void uma_zone_set_fini(uma_zone_t zone, uma_fini fini);
# 552 "/usr/include/vm/uma.h" 3 4
void uma_zone_set_zinit(uma_zone_t zone, uma_init zinit);
void uma_zone_set_zfini(uma_zone_t zone, uma_fini zfini);
# 571 "/usr/include/vm/uma.h" 3 4
void uma_zone_set_allocf(uma_zone_t zone, uma_alloc allocf);
# 584 "/usr/include/vm/uma.h" 3 4
void uma_zone_set_freef(uma_zone_t zone, uma_free freef);
# 609 "/usr/include/vm/uma.h" 3 4
void uma_prealloc(uma_zone_t zone, int itemcnt);
# 624 "/usr/include/vm/uma.h" 3 4
uint32_t *uma_find_refcnt(uma_zone_t zone, void *item);
# 635 "/usr/include/vm/uma.h" 3 4
int uma_zone_exhausted(uma_zone_t zone);
int uma_zone_exhausted_nolock(uma_zone_t zone);







struct uma_stream_header {
 uint32_t ush_version;
 uint32_t ush_maxcpus;
 uint32_t ush_count;
 uint32_t _ush_pad;
};



struct uma_type_header {



 char uth_name[32];
 uint32_t uth_align;
 uint32_t uth_size;
 uint32_t uth_rsize;
 uint32_t uth_maxpages;
 uint32_t uth_limit;




 uint32_t uth_pages;
 uint32_t uth_keg_free;
 uint32_t uth_zone_free;
 uint32_t uth_bucketsize;
 uint32_t uth_zone_flags;
 uint64_t uth_allocs;
 uint64_t uth_frees;
 uint64_t uth_fails;
 uint64_t uth_sleeps;
 uint64_t _uth_reserved1[2];
};

struct uma_percpu_stat {
 uint64_t ups_allocs;
 uint64_t ups_frees;
 uint64_t ups_cache_free;
 uint64_t _ups_reserved[5];
};
# 42 "/usr/include/sys/acl.h" 2 3 4





typedef uint32_t acl_tag_t;
typedef uint32_t acl_perm_t;
typedef uint16_t acl_entry_type_t;
typedef uint16_t acl_flag_t;
typedef int acl_type_t;
typedef int *acl_permset_t;
typedef uint16_t *acl_flagset_t;
# 145 "/usr/include/sys/acl.h" 3 4
typedef void *acl_entry_t;
typedef void *acl_t;
# 358 "/usr/include/sys/acl.h" 3 4
int acl_add_flag_np(acl_flagset_t _flagset_d, acl_flag_t _flag);
int acl_add_perm(acl_permset_t _permset_d, acl_perm_t _perm);
int acl_calc_mask(acl_t *_acl_p);
int acl_clear_flags_np(acl_flagset_t _flagset_d);
int acl_clear_perms(acl_permset_t _permset_d);
int acl_copy_entry(acl_entry_t _dest_d, acl_entry_t _src_d);
ssize_t acl_copy_ext(void *_buf_p, acl_t _acl, ssize_t _size);
acl_t acl_copy_int(const void *_buf_p);
int acl_create_entry(acl_t *_acl_p, acl_entry_t *_entry_p);
int acl_create_entry_np(acl_t *_acl_p, acl_entry_t *_entry_p, int _index);
int acl_delete_entry(acl_t _acl, acl_entry_t _entry_d);
int acl_delete_entry_np(acl_t _acl, int _index);
int acl_delete_fd_np(int _filedes, acl_type_t _type);
int acl_delete_file_np(const char *_path_p, acl_type_t _type);
int acl_delete_link_np(const char *_path_p, acl_type_t _type);
int acl_delete_def_file(const char *_path_p);
int acl_delete_def_link_np(const char *_path_p);
int acl_delete_flag_np(acl_flagset_t _flagset_d, acl_flag_t _flag);
int acl_delete_perm(acl_permset_t _permset_d, acl_perm_t _perm);
acl_t acl_dup(acl_t _acl);
int acl_free(void *_obj_p);
acl_t acl_from_text(const char *_buf_p);
int acl_get_brand_np(acl_t _acl, int *_brand_p);
int acl_get_entry(acl_t _acl, int _entry_id, acl_entry_t *_entry_p);
acl_t acl_get_fd(int _fd);
acl_t acl_get_fd_np(int fd, acl_type_t _type);
acl_t acl_get_file(const char *_path_p, acl_type_t _type);
int acl_get_entry_type_np(acl_entry_t _entry_d, acl_entry_type_t *_entry_type_p);
acl_t acl_get_link_np(const char *_path_p, acl_type_t _type);
void *acl_get_qualifier(acl_entry_t _entry_d);
int acl_get_flag_np(acl_flagset_t _flagset_d, acl_flag_t _flag);
int acl_get_perm_np(acl_permset_t _permset_d, acl_perm_t _perm);
int acl_get_flagset_np(acl_entry_t _entry_d, acl_flagset_t *_flagset_p);
int acl_get_permset(acl_entry_t _entry_d, acl_permset_t *_permset_p);
int acl_get_tag_type(acl_entry_t _entry_d, acl_tag_t *_tag_type_p);
acl_t acl_init(int _count);
int acl_set_fd(int _fd, acl_t _acl);
int acl_set_fd_np(int _fd, acl_t _acl, acl_type_t _type);
int acl_set_file(const char *_path_p, acl_type_t _type, acl_t _acl);
int acl_set_entry_type_np(acl_entry_t _entry_d, acl_entry_type_t _entry_type);
int acl_set_link_np(const char *_path_p, acl_type_t _type, acl_t _acl);
int acl_set_flagset_np(acl_entry_t _entry_d, acl_flagset_t _flagset_d);
int acl_set_permset(acl_entry_t _entry_d, acl_permset_t _permset_d);
int acl_set_qualifier(acl_entry_t _entry_d, const void *_tag_qualifier_p);
int acl_set_tag_type(acl_entry_t _entry_d, acl_tag_t _tag_type);
ssize_t acl_size(acl_t _acl);
char *acl_to_text(acl_t _acl, ssize_t *_len_p);
char *acl_to_text_np(acl_t _acl, ssize_t *_len_p, int _flags);
int acl_valid(acl_t _acl);
int acl_valid_fd_np(int _fd, acl_type_t _type, acl_t _acl);
int acl_valid_file_np(const char *_path_p, acl_type_t _type, acl_t _acl);
int acl_valid_link_np(const char *_path_p, acl_type_t _type, acl_t _acl);
int acl_is_trivial_np(const acl_t _acl, int *_trivialp);
acl_t acl_strip_np(const acl_t _acl, int recalculate_mask);
# 14 "/usr/include/sys/sysproto.h" 2 3 4
# 1 "/usr/include/sys/cpuset.h" 1 3 4
# 35 "/usr/include/sys/cpuset.h" 3 4
# 1 "/usr/include/sys/_cpuset.h" 1 3 4
# 35 "/usr/include/sys/_cpuset.h" 3 4
# 1 "/usr/include/sys/_bitset.h" 1 3 4
# 36 "/usr/include/sys/_cpuset.h" 2 3 4
# 50 "/usr/include/sys/_cpuset.h" 3 4
struct _cpuset { long __bits[(((((128))+(((sizeof(long) * 8))-1))/((sizeof(long) * 8))))]; };;
typedef struct _cpuset cpuset_t;
# 36 "/usr/include/sys/cpuset.h" 2 3 4

# 1 "/usr/include/sys/bitset.h" 1 3 4
# 38 "/usr/include/sys/cpuset.h" 2 3 4
# 131 "/usr/include/sys/cpuset.h" 3 4
int cpuset(cpusetid_t *);
int cpuset_setid(cpuwhich_t, id_t, cpusetid_t);
int cpuset_getid(cpulevel_t, cpuwhich_t, id_t, cpusetid_t *);
int cpuset_getaffinity(cpulevel_t, cpuwhich_t, id_t, size_t, cpuset_t *);
int cpuset_setaffinity(cpulevel_t, cpuwhich_t, id_t, size_t, const cpuset_t *);
# 15 "/usr/include/sys/sysproto.h" 2 3 4
# 1 "/usr/include/sys/_ffcounter.h" 1 3 4
# 40 "/usr/include/sys/_ffcounter.h" 3 4
typedef uint64_t ffcounter;
# 16 "/usr/include/sys/sysproto.h" 2 3 4
# 1 "/usr/include/sys/_semaphore.h" 1 3 4
# 31 "/usr/include/sys/_semaphore.h" 3 4
typedef intptr_t semid_t;
struct timespec;







int ksem_close(semid_t id);
int ksem_post(semid_t id);
int ksem_wait(semid_t id);
int ksem_trywait(semid_t id);
int ksem_timedwait(semid_t id, const struct timespec *abstime);
int ksem_init(semid_t *idp, unsigned int value);
int ksem_open(semid_t *idp, const char *name, int oflag, mode_t mode,
    unsigned int value);
int ksem_unlink(const char *name);
int ksem_getvalue(semid_t id, int *val);
int ksem_destroy(semid_t id);
# 17 "/usr/include/sys/sysproto.h" 2 3 4
# 1 "/usr/include/sys/ucontext.h" 1 3 4
# 35 "/usr/include/sys/ucontext.h" 3 4
# 1 "/usr/include/machine/ucontext.h" 1 3 4





# 1 "/usr/include/x86/ucontext.h" 1 3 4
# 104 "/usr/include/x86/ucontext.h" 3 4
typedef struct __mcontext {






 __register_t mc_onstack;
 __register_t mc_rdi;
 __register_t mc_rsi;
 __register_t mc_rdx;
 __register_t mc_rcx;
 __register_t mc_r8;
 __register_t mc_r9;
 __register_t mc_rax;
 __register_t mc_rbx;
 __register_t mc_rbp;
 __register_t mc_r10;
 __register_t mc_r11;
 __register_t mc_r12;
 __register_t mc_r13;
 __register_t mc_r14;
 __register_t mc_r15;
 __uint32_t mc_trapno;
 __uint16_t mc_fs;
 __uint16_t mc_gs;
 __register_t mc_addr;
 __uint32_t mc_flags;
 __uint16_t mc_es;
 __uint16_t mc_ds;
 __register_t mc_err;
 __register_t mc_rip;
 __register_t mc_cs;
 __register_t mc_rflags;
 __register_t mc_rsp;
 __register_t mc_ss;

 long mc_len;



 long mc_fpformat;



 long mc_ownedfp;



 long mc_fpstate[64] __attribute__((__aligned__(16)));

 __register_t mc_fsbase;
 __register_t mc_gsbase;

 __register_t mc_xfpustate;
 __register_t mc_xfpustate_len;

 long mc_spare[4];
} mcontext_t;
# 7 "/usr/include/machine/ucontext.h" 2 3 4
# 36 "/usr/include/sys/ucontext.h" 2 3 4

typedef struct __ucontext {
# 46 "/usr/include/sys/ucontext.h" 3 4
 sigset_t uc_sigmask;
 mcontext_t uc_mcontext;

 struct __ucontext *uc_link;
 stack_t uc_stack;
 int uc_flags;

 int __spare__[4];
} ucontext_t;
# 74 "/usr/include/sys/ucontext.h" 3 4
int getcontext(ucontext_t *) __attribute__((__returns_twice__));
ucontext_t *getcontextx(void);
int setcontext(const ucontext_t *);
void makecontext(ucontext_t *, void (*)(void), int, ...);
int signalcontext(ucontext_t *, int, __sighandler_t *);
int swapcontext(ucontext_t *, const ucontext_t *);


int __getcontextx_size(void);
int __fillcontextx(char *ctx) __attribute__((__returns_twice__));
int __fillcontextx2(char *ctx);
# 18 "/usr/include/sys/sysproto.h" 2 3 4
# 1 "/usr/include/sys/wait.h" 1 3 4
# 92 "/usr/include/sys/wait.h" 3 4
typedef enum

 idtype

  {
# 107 "/usr/include/sys/wait.h" 3 4
 P_PID,
 P_PPID,
 P_PGID,
 P_SID,
 P_CID,
 P_UID,
 P_GID,
 P_ALL,
 P_LWPID,
 P_TASKID,
 P_PROJID,
 P_POOLID,
 P_JAILID,
 P_CTID,
 P_CPUID,
 P_PSETID
} idtype_t;
# 145 "/usr/include/sys/wait.h" 3 4
struct __siginfo;
pid_t wait(int *);
pid_t waitpid(pid_t, int *, int);

int waitid(idtype_t, id_t, struct __siginfo *, int);


struct rusage;
struct __wrusage;
pid_t wait3(int *, int, struct rusage *);
pid_t wait4(pid_t, int *, int, struct rusage *);
pid_t wait6(idtype_t, id_t, int *, int, struct __wrusage *,
     struct __siginfo *);
# 19 "/usr/include/sys/sysproto.h" 2 3 4

# 1 "/usr/include/bsm/audit_kevents.h" 1 3 4
# 21 "/usr/include/sys/sysproto.h" 2 3 4

struct proc;

struct thread;
# 37 "/usr/include/sys/sysproto.h" 3 4
struct nosys_args {
 register_t dummy;
};
struct sys_exit_args {
 char rval_l_[0]; int rval; char rval_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct fork_args {
 register_t dummy;
};
struct read_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char buf_l_[0]; void * buf; char buf_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char nbyte_l_[0]; size_t nbyte; char nbyte_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct write_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char buf_l_[0]; const void * buf; char buf_r_[(sizeof(register_t) <= sizeof(const void *) ? 0 : sizeof(register_t) - sizeof(const void *))];
 char nbyte_l_[0]; size_t nbyte; char nbyte_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct open_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char mode_l_[0]; int mode; char mode_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct close_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct wait4_args {
 char pid_l_[0]; int pid; char pid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char status_l_[0]; int * status; char status_r_[(sizeof(register_t) <= sizeof(int *) ? 0 : sizeof(register_t) - sizeof(int *))];
 char options_l_[0]; int options; char options_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char rusage_l_[0]; struct rusage * rusage; char rusage_r_[(sizeof(register_t) <= sizeof(struct rusage *) ? 0 : sizeof(register_t) - sizeof(struct rusage *))];
};
struct link_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char link_l_[0]; char * link; char link_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
};
struct unlink_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
};
struct chdir_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
};
struct fchdir_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct mknod_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char mode_l_[0]; int mode; char mode_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char dev_l_[0]; int dev; char dev_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct chmod_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char mode_l_[0]; int mode; char mode_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct chown_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char uid_l_[0]; int uid; char uid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char gid_l_[0]; int gid; char gid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct obreak_args {
 char nsize_l_[0]; char * nsize; char nsize_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
};
struct getpid_args {
 register_t dummy;
};
struct mount_args {
 char type_l_[0]; char * type; char type_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char data_l_[0]; caddr_t data; char data_r_[(sizeof(register_t) <= sizeof(caddr_t) ? 0 : sizeof(register_t) - sizeof(caddr_t))];
};
struct unmount_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct setuid_args {
 char uid_l_[0]; uid_t uid; char uid_r_[(sizeof(register_t) <= sizeof(uid_t) ? 0 : sizeof(register_t) - sizeof(uid_t))];
};
struct getuid_args {
 register_t dummy;
};
struct geteuid_args {
 register_t dummy;
};
struct ptrace_args {
 char req_l_[0]; int req; char req_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char pid_l_[0]; pid_t pid; char pid_r_[(sizeof(register_t) <= sizeof(pid_t) ? 0 : sizeof(register_t) - sizeof(pid_t))];
 char addr_l_[0]; caddr_t addr; char addr_r_[(sizeof(register_t) <= sizeof(caddr_t) ? 0 : sizeof(register_t) - sizeof(caddr_t))];
 char data_l_[0]; int data; char data_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct recvmsg_args {
 char s_l_[0]; int s; char s_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char msg_l_[0]; struct msghdr * msg; char msg_r_[(sizeof(register_t) <= sizeof(struct msghdr *) ? 0 : sizeof(register_t) - sizeof(struct msghdr *))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct sendmsg_args {
 char s_l_[0]; int s; char s_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char msg_l_[0]; struct msghdr * msg; char msg_r_[(sizeof(register_t) <= sizeof(struct msghdr *) ? 0 : sizeof(register_t) - sizeof(struct msghdr *))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct recvfrom_args {
 char s_l_[0]; int s; char s_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char buf_l_[0]; caddr_t buf; char buf_r_[(sizeof(register_t) <= sizeof(caddr_t) ? 0 : sizeof(register_t) - sizeof(caddr_t))];
 char len_l_[0]; size_t len; char len_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char from_l_[0]; struct sockaddr *restrict from; char from_r_[(sizeof(register_t) <= sizeof(struct sockaddr *restrict) ? 0 : sizeof(register_t) - sizeof(struct sockaddr *restrict))];
 char fromlenaddr_l_[0]; __socklen_t *restrict fromlenaddr; char fromlenaddr_r_[(sizeof(register_t) <= sizeof(__socklen_t *restrict) ? 0 : sizeof(register_t) - sizeof(__socklen_t *restrict))];
};
struct accept_args {
 char s_l_[0]; int s; char s_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char name_l_[0]; struct sockaddr *restrict name; char name_r_[(sizeof(register_t) <= sizeof(struct sockaddr *restrict) ? 0 : sizeof(register_t) - sizeof(struct sockaddr *restrict))];
 char anamelen_l_[0]; __socklen_t *restrict anamelen; char anamelen_r_[(sizeof(register_t) <= sizeof(__socklen_t *restrict) ? 0 : sizeof(register_t) - sizeof(__socklen_t *restrict))];
};
struct getpeername_args {
 char fdes_l_[0]; int fdes; char fdes_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char asa_l_[0]; struct sockaddr *restrict asa; char asa_r_[(sizeof(register_t) <= sizeof(struct sockaddr *restrict) ? 0 : sizeof(register_t) - sizeof(struct sockaddr *restrict))];
 char alen_l_[0]; __socklen_t *restrict alen; char alen_r_[(sizeof(register_t) <= sizeof(__socklen_t *restrict) ? 0 : sizeof(register_t) - sizeof(__socklen_t *restrict))];
};
struct getsockname_args {
 char fdes_l_[0]; int fdes; char fdes_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char asa_l_[0]; struct sockaddr *restrict asa; char asa_r_[(sizeof(register_t) <= sizeof(struct sockaddr *restrict) ? 0 : sizeof(register_t) - sizeof(struct sockaddr *restrict))];
 char alen_l_[0]; __socklen_t *restrict alen; char alen_r_[(sizeof(register_t) <= sizeof(__socklen_t *restrict) ? 0 : sizeof(register_t) - sizeof(__socklen_t *restrict))];
};
struct access_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char amode_l_[0]; int amode; char amode_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct chflags_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char flags_l_[0]; u_long flags; char flags_r_[(sizeof(register_t) <= sizeof(u_long) ? 0 : sizeof(register_t) - sizeof(u_long))];
};
struct fchflags_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char flags_l_[0]; u_long flags; char flags_r_[(sizeof(register_t) <= sizeof(u_long) ? 0 : sizeof(register_t) - sizeof(u_long))];
};
struct sync_args {
 register_t dummy;
};
struct kill_args {
 char pid_l_[0]; int pid; char pid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char signum_l_[0]; int signum; char signum_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct getppid_args {
 register_t dummy;
};
struct dup_args {
 char fd_l_[0]; u_int fd; char fd_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
};
struct pipe_args {
 register_t dummy;
};
struct getegid_args {
 register_t dummy;
};
struct profil_args {
 char samples_l_[0]; caddr_t samples; char samples_r_[(sizeof(register_t) <= sizeof(caddr_t) ? 0 : sizeof(register_t) - sizeof(caddr_t))];
 char size_l_[0]; size_t size; char size_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char offset_l_[0]; size_t offset; char offset_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char scale_l_[0]; u_int scale; char scale_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
};
struct ktrace_args {
 char fname_l_[0]; const char * fname; char fname_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char ops_l_[0]; int ops; char ops_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char facs_l_[0]; int facs; char facs_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char pid_l_[0]; int pid; char pid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct getgid_args {
 register_t dummy;
};
struct getlogin_args {
 char namebuf_l_[0]; char * namebuf; char namebuf_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char namelen_l_[0]; u_int namelen; char namelen_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
};
struct setlogin_args {
 char namebuf_l_[0]; char * namebuf; char namebuf_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
};
struct acct_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
};
struct osigpending_args {
 register_t dummy;
};
struct sigaltstack_args {
 char ss_l_[0]; stack_t * ss; char ss_r_[(sizeof(register_t) <= sizeof(stack_t *) ? 0 : sizeof(register_t) - sizeof(stack_t *))];
 char oss_l_[0]; stack_t * oss; char oss_r_[(sizeof(register_t) <= sizeof(stack_t *) ? 0 : sizeof(register_t) - sizeof(stack_t *))];
};
struct ioctl_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char com_l_[0]; u_long com; char com_r_[(sizeof(register_t) <= sizeof(u_long) ? 0 : sizeof(register_t) - sizeof(u_long))];
 char data_l_[0]; caddr_t data; char data_r_[(sizeof(register_t) <= sizeof(caddr_t) ? 0 : sizeof(register_t) - sizeof(caddr_t))];
};
struct reboot_args {
 char opt_l_[0]; int opt; char opt_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct revoke_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
};
struct symlink_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char link_l_[0]; char * link; char link_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
};
struct readlink_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char buf_l_[0]; char * buf; char buf_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char count_l_[0]; size_t count; char count_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct execve_args {
 char fname_l_[0]; char * fname; char fname_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char argv_l_[0]; char ** argv; char argv_r_[(sizeof(register_t) <= sizeof(char **) ? 0 : sizeof(register_t) - sizeof(char **))];
 char envv_l_[0]; char ** envv; char envv_r_[(sizeof(register_t) <= sizeof(char **) ? 0 : sizeof(register_t) - sizeof(char **))];
};
struct umask_args {
 char newmask_l_[0]; int newmask; char newmask_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct chroot_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
};
struct getpagesize_args {
 register_t dummy;
};
struct msync_args {
 char addr_l_[0]; void * addr; char addr_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char len_l_[0]; size_t len; char len_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct vfork_args {
 register_t dummy;
};
struct sbrk_args {
 char incr_l_[0]; int incr; char incr_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct sstk_args {
 char incr_l_[0]; int incr; char incr_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct ovadvise_args {
 char anom_l_[0]; int anom; char anom_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct munmap_args {
 char addr_l_[0]; void * addr; char addr_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char len_l_[0]; size_t len; char len_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct mprotect_args {
 char addr_l_[0]; const void * addr; char addr_r_[(sizeof(register_t) <= sizeof(const void *) ? 0 : sizeof(register_t) - sizeof(const void *))];
 char len_l_[0]; size_t len; char len_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char prot_l_[0]; int prot; char prot_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct madvise_args {
 char addr_l_[0]; void * addr; char addr_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char len_l_[0]; size_t len; char len_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char behav_l_[0]; int behav; char behav_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct mincore_args {
 char addr_l_[0]; const void * addr; char addr_r_[(sizeof(register_t) <= sizeof(const void *) ? 0 : sizeof(register_t) - sizeof(const void *))];
 char len_l_[0]; size_t len; char len_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char vec_l_[0]; char * vec; char vec_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
};
struct getgroups_args {
 char gidsetsize_l_[0]; u_int gidsetsize; char gidsetsize_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
 char gidset_l_[0]; gid_t * gidset; char gidset_r_[(sizeof(register_t) <= sizeof(gid_t *) ? 0 : sizeof(register_t) - sizeof(gid_t *))];
};
struct setgroups_args {
 char gidsetsize_l_[0]; u_int gidsetsize; char gidsetsize_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
 char gidset_l_[0]; gid_t * gidset; char gidset_r_[(sizeof(register_t) <= sizeof(gid_t *) ? 0 : sizeof(register_t) - sizeof(gid_t *))];
};
struct getpgrp_args {
 register_t dummy;
};
struct setpgid_args {
 char pid_l_[0]; int pid; char pid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char pgid_l_[0]; int pgid; char pgid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct setitimer_args {
 char which_l_[0]; u_int which; char which_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
 char itv_l_[0]; struct itimerval * itv; char itv_r_[(sizeof(register_t) <= sizeof(struct itimerval *) ? 0 : sizeof(register_t) - sizeof(struct itimerval *))];
 char oitv_l_[0]; struct itimerval * oitv; char oitv_r_[(sizeof(register_t) <= sizeof(struct itimerval *) ? 0 : sizeof(register_t) - sizeof(struct itimerval *))];
};
struct owait_args {
 register_t dummy;
};
struct swapon_args {
 char name_l_[0]; char * name; char name_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
};
struct getitimer_args {
 char which_l_[0]; u_int which; char which_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
 char itv_l_[0]; struct itimerval * itv; char itv_r_[(sizeof(register_t) <= sizeof(struct itimerval *) ? 0 : sizeof(register_t) - sizeof(struct itimerval *))];
};
struct getdtablesize_args {
 register_t dummy;
};
struct dup2_args {
 char from_l_[0]; u_int from; char from_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
 char to_l_[0]; u_int to; char to_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
};
struct fcntl_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char cmd_l_[0]; int cmd; char cmd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char arg_l_[0]; long arg; char arg_r_[(sizeof(register_t) <= sizeof(long) ? 0 : sizeof(register_t) - sizeof(long))];
};
struct select_args {
 char nd_l_[0]; int nd; char nd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char in_l_[0]; fd_set * in; char in_r_[(sizeof(register_t) <= sizeof(fd_set *) ? 0 : sizeof(register_t) - sizeof(fd_set *))];
 char ou_l_[0]; fd_set * ou; char ou_r_[(sizeof(register_t) <= sizeof(fd_set *) ? 0 : sizeof(register_t) - sizeof(fd_set *))];
 char ex_l_[0]; fd_set * ex; char ex_r_[(sizeof(register_t) <= sizeof(fd_set *) ? 0 : sizeof(register_t) - sizeof(fd_set *))];
 char tv_l_[0]; struct timeval * tv; char tv_r_[(sizeof(register_t) <= sizeof(struct timeval *) ? 0 : sizeof(register_t) - sizeof(struct timeval *))];
};
struct fsync_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct setpriority_args {
 char which_l_[0]; int which; char which_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char who_l_[0]; int who; char who_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char prio_l_[0]; int prio; char prio_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct socket_args {
 char domain_l_[0]; int domain; char domain_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char type_l_[0]; int type; char type_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char protocol_l_[0]; int protocol; char protocol_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct connect_args {
 char s_l_[0]; int s; char s_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char name_l_[0]; caddr_t name; char name_r_[(sizeof(register_t) <= sizeof(caddr_t) ? 0 : sizeof(register_t) - sizeof(caddr_t))];
 char namelen_l_[0]; int namelen; char namelen_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct getpriority_args {
 char which_l_[0]; int which; char which_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char who_l_[0]; int who; char who_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct bind_args {
 char s_l_[0]; int s; char s_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char name_l_[0]; caddr_t name; char name_r_[(sizeof(register_t) <= sizeof(caddr_t) ? 0 : sizeof(register_t) - sizeof(caddr_t))];
 char namelen_l_[0]; int namelen; char namelen_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct setsockopt_args {
 char s_l_[0]; int s; char s_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char level_l_[0]; int level; char level_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char name_l_[0]; int name; char name_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char val_l_[0]; caddr_t val; char val_r_[(sizeof(register_t) <= sizeof(caddr_t) ? 0 : sizeof(register_t) - sizeof(caddr_t))];
 char valsize_l_[0]; int valsize; char valsize_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct listen_args {
 char s_l_[0]; int s; char s_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char backlog_l_[0]; int backlog; char backlog_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct gettimeofday_args {
 char tp_l_[0]; struct timeval * tp; char tp_r_[(sizeof(register_t) <= sizeof(struct timeval *) ? 0 : sizeof(register_t) - sizeof(struct timeval *))];
 char tzp_l_[0]; struct timezone * tzp; char tzp_r_[(sizeof(register_t) <= sizeof(struct timezone *) ? 0 : sizeof(register_t) - sizeof(struct timezone *))];
};
struct getrusage_args {
 char who_l_[0]; int who; char who_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char rusage_l_[0]; struct rusage * rusage; char rusage_r_[(sizeof(register_t) <= sizeof(struct rusage *) ? 0 : sizeof(register_t) - sizeof(struct rusage *))];
};
struct getsockopt_args {
 char s_l_[0]; int s; char s_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char level_l_[0]; int level; char level_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char name_l_[0]; int name; char name_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char val_l_[0]; caddr_t val; char val_r_[(sizeof(register_t) <= sizeof(caddr_t) ? 0 : sizeof(register_t) - sizeof(caddr_t))];
 char avalsize_l_[0]; int * avalsize; char avalsize_r_[(sizeof(register_t) <= sizeof(int *) ? 0 : sizeof(register_t) - sizeof(int *))];
};
struct readv_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char iovp_l_[0]; struct iovec * iovp; char iovp_r_[(sizeof(register_t) <= sizeof(struct iovec *) ? 0 : sizeof(register_t) - sizeof(struct iovec *))];
 char iovcnt_l_[0]; u_int iovcnt; char iovcnt_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
};
struct writev_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char iovp_l_[0]; struct iovec * iovp; char iovp_r_[(sizeof(register_t) <= sizeof(struct iovec *) ? 0 : sizeof(register_t) - sizeof(struct iovec *))];
 char iovcnt_l_[0]; u_int iovcnt; char iovcnt_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
};
struct settimeofday_args {
 char tv_l_[0]; struct timeval * tv; char tv_r_[(sizeof(register_t) <= sizeof(struct timeval *) ? 0 : sizeof(register_t) - sizeof(struct timeval *))];
 char tzp_l_[0]; struct timezone * tzp; char tzp_r_[(sizeof(register_t) <= sizeof(struct timezone *) ? 0 : sizeof(register_t) - sizeof(struct timezone *))];
};
struct fchown_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char uid_l_[0]; int uid; char uid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char gid_l_[0]; int gid; char gid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct fchmod_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char mode_l_[0]; int mode; char mode_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct setreuid_args {
 char ruid_l_[0]; int ruid; char ruid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char euid_l_[0]; int euid; char euid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct setregid_args {
 char rgid_l_[0]; int rgid; char rgid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char egid_l_[0]; int egid; char egid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct rename_args {
 char from_l_[0]; char * from; char from_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char to_l_[0]; char * to; char to_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
};
struct flock_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char how_l_[0]; int how; char how_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct mkfifo_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char mode_l_[0]; int mode; char mode_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct sendto_args {
 char s_l_[0]; int s; char s_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char buf_l_[0]; caddr_t buf; char buf_r_[(sizeof(register_t) <= sizeof(caddr_t) ? 0 : sizeof(register_t) - sizeof(caddr_t))];
 char len_l_[0]; size_t len; char len_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char to_l_[0]; caddr_t to; char to_r_[(sizeof(register_t) <= sizeof(caddr_t) ? 0 : sizeof(register_t) - sizeof(caddr_t))];
 char tolen_l_[0]; int tolen; char tolen_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct shutdown_args {
 char s_l_[0]; int s; char s_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char how_l_[0]; int how; char how_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct socketpair_args {
 char domain_l_[0]; int domain; char domain_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char type_l_[0]; int type; char type_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char protocol_l_[0]; int protocol; char protocol_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char rsv_l_[0]; int * rsv; char rsv_r_[(sizeof(register_t) <= sizeof(int *) ? 0 : sizeof(register_t) - sizeof(int *))];
};
struct mkdir_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char mode_l_[0]; int mode; char mode_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct rmdir_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
};
struct utimes_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char tptr_l_[0]; struct timeval * tptr; char tptr_r_[(sizeof(register_t) <= sizeof(struct timeval *) ? 0 : sizeof(register_t) - sizeof(struct timeval *))];
};
struct adjtime_args {
 char delta_l_[0]; struct timeval * delta; char delta_r_[(sizeof(register_t) <= sizeof(struct timeval *) ? 0 : sizeof(register_t) - sizeof(struct timeval *))];
 char olddelta_l_[0]; struct timeval * olddelta; char olddelta_r_[(sizeof(register_t) <= sizeof(struct timeval *) ? 0 : sizeof(register_t) - sizeof(struct timeval *))];
};
struct ogethostid_args {
 register_t dummy;
};
struct setsid_args {
 register_t dummy;
};
struct quotactl_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char cmd_l_[0]; int cmd; char cmd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char uid_l_[0]; int uid; char uid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char arg_l_[0]; caddr_t arg; char arg_r_[(sizeof(register_t) <= sizeof(caddr_t) ? 0 : sizeof(register_t) - sizeof(caddr_t))];
};
struct oquota_args {
 register_t dummy;
};
struct nlm_syscall_args {
 char debug_level_l_[0]; int debug_level; char debug_level_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char grace_period_l_[0]; int grace_period; char grace_period_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char addr_count_l_[0]; int addr_count; char addr_count_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char addrs_l_[0]; char ** addrs; char addrs_r_[(sizeof(register_t) <= sizeof(char **) ? 0 : sizeof(register_t) - sizeof(char **))];
};
struct nfssvc_args {
 char flag_l_[0]; int flag; char flag_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char argp_l_[0]; caddr_t argp; char argp_r_[(sizeof(register_t) <= sizeof(caddr_t) ? 0 : sizeof(register_t) - sizeof(caddr_t))];
};
struct lgetfh_args {
 char fname_l_[0]; char * fname; char fname_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char fhp_l_[0]; struct fhandle * fhp; char fhp_r_[(sizeof(register_t) <= sizeof(struct fhandle *) ? 0 : sizeof(register_t) - sizeof(struct fhandle *))];
};
struct getfh_args {
 char fname_l_[0]; char * fname; char fname_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char fhp_l_[0]; struct fhandle * fhp; char fhp_r_[(sizeof(register_t) <= sizeof(struct fhandle *) ? 0 : sizeof(register_t) - sizeof(struct fhandle *))];
};
struct sysarch_args {
 char op_l_[0]; int op; char op_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char parms_l_[0]; char * parms; char parms_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
};
struct rtprio_args {
 char function_l_[0]; int function; char function_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char pid_l_[0]; pid_t pid; char pid_r_[(sizeof(register_t) <= sizeof(pid_t) ? 0 : sizeof(register_t) - sizeof(pid_t))];
 char rtp_l_[0]; struct rtprio * rtp; char rtp_r_[(sizeof(register_t) <= sizeof(struct rtprio *) ? 0 : sizeof(register_t) - sizeof(struct rtprio *))];
};
struct semsys_args {
 char which_l_[0]; int which; char which_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char a2_l_[0]; int a2; char a2_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char a3_l_[0]; int a3; char a3_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char a4_l_[0]; int a4; char a4_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char a5_l_[0]; int a5; char a5_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct msgsys_args {
 char which_l_[0]; int which; char which_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char a2_l_[0]; int a2; char a2_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char a3_l_[0]; int a3; char a3_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char a4_l_[0]; int a4; char a4_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char a5_l_[0]; int a5; char a5_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char a6_l_[0]; int a6; char a6_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct shmsys_args {
 char which_l_[0]; int which; char which_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char a2_l_[0]; int a2; char a2_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char a3_l_[0]; int a3; char a3_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char a4_l_[0]; int a4; char a4_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct freebsd6_pread_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char buf_l_[0]; void * buf; char buf_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char nbyte_l_[0]; size_t nbyte; char nbyte_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char pad_l_[0]; int pad; char pad_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char offset_l_[0]; off_t offset; char offset_r_[(sizeof(register_t) <= sizeof(off_t) ? 0 : sizeof(register_t) - sizeof(off_t))];
};
struct freebsd6_pwrite_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char buf_l_[0]; const void * buf; char buf_r_[(sizeof(register_t) <= sizeof(const void *) ? 0 : sizeof(register_t) - sizeof(const void *))];
 char nbyte_l_[0]; size_t nbyte; char nbyte_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char pad_l_[0]; int pad; char pad_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char offset_l_[0]; off_t offset; char offset_r_[(sizeof(register_t) <= sizeof(off_t) ? 0 : sizeof(register_t) - sizeof(off_t))];
};
struct setfib_args {
 char fibnum_l_[0]; int fibnum; char fibnum_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct ntp_adjtime_args {
 char tp_l_[0]; struct timex * tp; char tp_r_[(sizeof(register_t) <= sizeof(struct timex *) ? 0 : sizeof(register_t) - sizeof(struct timex *))];
};
struct setgid_args {
 char gid_l_[0]; gid_t gid; char gid_r_[(sizeof(register_t) <= sizeof(gid_t) ? 0 : sizeof(register_t) - sizeof(gid_t))];
};
struct setegid_args {
 char egid_l_[0]; gid_t egid; char egid_r_[(sizeof(register_t) <= sizeof(gid_t) ? 0 : sizeof(register_t) - sizeof(gid_t))];
};
struct seteuid_args {
 char euid_l_[0]; uid_t euid; char euid_r_[(sizeof(register_t) <= sizeof(uid_t) ? 0 : sizeof(register_t) - sizeof(uid_t))];
};
struct stat_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char ub_l_[0]; struct stat * ub; char ub_r_[(sizeof(register_t) <= sizeof(struct stat *) ? 0 : sizeof(register_t) - sizeof(struct stat *))];
};
struct fstat_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char sb_l_[0]; struct stat * sb; char sb_r_[(sizeof(register_t) <= sizeof(struct stat *) ? 0 : sizeof(register_t) - sizeof(struct stat *))];
};
struct lstat_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char ub_l_[0]; struct stat * ub; char ub_r_[(sizeof(register_t) <= sizeof(struct stat *) ? 0 : sizeof(register_t) - sizeof(struct stat *))];
};
struct pathconf_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char name_l_[0]; int name; char name_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct fpathconf_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char name_l_[0]; int name; char name_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct __getrlimit_args {
 char which_l_[0]; u_int which; char which_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
 char rlp_l_[0]; struct rlimit * rlp; char rlp_r_[(sizeof(register_t) <= sizeof(struct rlimit *) ? 0 : sizeof(register_t) - sizeof(struct rlimit *))];
};
struct __setrlimit_args {
 char which_l_[0]; u_int which; char which_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
 char rlp_l_[0]; struct rlimit * rlp; char rlp_r_[(sizeof(register_t) <= sizeof(struct rlimit *) ? 0 : sizeof(register_t) - sizeof(struct rlimit *))];
};
struct getdirentries_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char buf_l_[0]; char * buf; char buf_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char count_l_[0]; u_int count; char count_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
 char basep_l_[0]; long * basep; char basep_r_[(sizeof(register_t) <= sizeof(long *) ? 0 : sizeof(register_t) - sizeof(long *))];
};
struct freebsd6_mmap_args {
 char addr_l_[0]; caddr_t addr; char addr_r_[(sizeof(register_t) <= sizeof(caddr_t) ? 0 : sizeof(register_t) - sizeof(caddr_t))];
 char len_l_[0]; size_t len; char len_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char prot_l_[0]; int prot; char prot_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char pad_l_[0]; int pad; char pad_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char pos_l_[0]; off_t pos; char pos_r_[(sizeof(register_t) <= sizeof(off_t) ? 0 : sizeof(register_t) - sizeof(off_t))];
};
struct freebsd6_lseek_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char pad_l_[0]; int pad; char pad_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char offset_l_[0]; off_t offset; char offset_r_[(sizeof(register_t) <= sizeof(off_t) ? 0 : sizeof(register_t) - sizeof(off_t))];
 char whence_l_[0]; int whence; char whence_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct freebsd6_truncate_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char pad_l_[0]; int pad; char pad_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char length_l_[0]; off_t length; char length_r_[(sizeof(register_t) <= sizeof(off_t) ? 0 : sizeof(register_t) - sizeof(off_t))];
};
struct freebsd6_ftruncate_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char pad_l_[0]; int pad; char pad_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char length_l_[0]; off_t length; char length_r_[(sizeof(register_t) <= sizeof(off_t) ? 0 : sizeof(register_t) - sizeof(off_t))];
};
struct sysctl_args {
 char name_l_[0]; int * name; char name_r_[(sizeof(register_t) <= sizeof(int *) ? 0 : sizeof(register_t) - sizeof(int *))];
 char namelen_l_[0]; u_int namelen; char namelen_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
 char old_l_[0]; void * old; char old_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char oldlenp_l_[0]; size_t * oldlenp; char oldlenp_r_[(sizeof(register_t) <= sizeof(size_t *) ? 0 : sizeof(register_t) - sizeof(size_t *))];
 char new_l_[0]; void * new; char new_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char newlen_l_[0]; size_t newlen; char newlen_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct mlock_args {
 char addr_l_[0]; const void * addr; char addr_r_[(sizeof(register_t) <= sizeof(const void *) ? 0 : sizeof(register_t) - sizeof(const void *))];
 char len_l_[0]; size_t len; char len_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct munlock_args {
 char addr_l_[0]; const void * addr; char addr_r_[(sizeof(register_t) <= sizeof(const void *) ? 0 : sizeof(register_t) - sizeof(const void *))];
 char len_l_[0]; size_t len; char len_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct undelete_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
};
struct futimes_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char tptr_l_[0]; struct timeval * tptr; char tptr_r_[(sizeof(register_t) <= sizeof(struct timeval *) ? 0 : sizeof(register_t) - sizeof(struct timeval *))];
};
struct getpgid_args {
 char pid_l_[0]; pid_t pid; char pid_r_[(sizeof(register_t) <= sizeof(pid_t) ? 0 : sizeof(register_t) - sizeof(pid_t))];
};
struct poll_args {
 char fds_l_[0]; struct pollfd * fds; char fds_r_[(sizeof(register_t) <= sizeof(struct pollfd *) ? 0 : sizeof(register_t) - sizeof(struct pollfd *))];
 char nfds_l_[0]; u_int nfds; char nfds_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
 char timeout_l_[0]; int timeout; char timeout_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct semget_args {
 char key_l_[0]; key_t key; char key_r_[(sizeof(register_t) <= sizeof(key_t) ? 0 : sizeof(register_t) - sizeof(key_t))];
 char nsems_l_[0]; int nsems; char nsems_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char semflg_l_[0]; int semflg; char semflg_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct semop_args {
 char semid_l_[0]; int semid; char semid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char sops_l_[0]; struct sembuf * sops; char sops_r_[(sizeof(register_t) <= sizeof(struct sembuf *) ? 0 : sizeof(register_t) - sizeof(struct sembuf *))];
 char nsops_l_[0]; size_t nsops; char nsops_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct msgget_args {
 char key_l_[0]; key_t key; char key_r_[(sizeof(register_t) <= sizeof(key_t) ? 0 : sizeof(register_t) - sizeof(key_t))];
 char msgflg_l_[0]; int msgflg; char msgflg_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct msgsnd_args {
 char msqid_l_[0]; int msqid; char msqid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char msgp_l_[0]; const void * msgp; char msgp_r_[(sizeof(register_t) <= sizeof(const void *) ? 0 : sizeof(register_t) - sizeof(const void *))];
 char msgsz_l_[0]; size_t msgsz; char msgsz_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char msgflg_l_[0]; int msgflg; char msgflg_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct msgrcv_args {
 char msqid_l_[0]; int msqid; char msqid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char msgp_l_[0]; void * msgp; char msgp_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char msgsz_l_[0]; size_t msgsz; char msgsz_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char msgtyp_l_[0]; long msgtyp; char msgtyp_r_[(sizeof(register_t) <= sizeof(long) ? 0 : sizeof(register_t) - sizeof(long))];
 char msgflg_l_[0]; int msgflg; char msgflg_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct shmat_args {
 char shmid_l_[0]; int shmid; char shmid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char shmaddr_l_[0]; const void * shmaddr; char shmaddr_r_[(sizeof(register_t) <= sizeof(const void *) ? 0 : sizeof(register_t) - sizeof(const void *))];
 char shmflg_l_[0]; int shmflg; char shmflg_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct shmdt_args {
 char shmaddr_l_[0]; const void * shmaddr; char shmaddr_r_[(sizeof(register_t) <= sizeof(const void *) ? 0 : sizeof(register_t) - sizeof(const void *))];
};
struct shmget_args {
 char key_l_[0]; key_t key; char key_r_[(sizeof(register_t) <= sizeof(key_t) ? 0 : sizeof(register_t) - sizeof(key_t))];
 char size_l_[0]; size_t size; char size_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char shmflg_l_[0]; int shmflg; char shmflg_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct clock_gettime_args {
 char clock_id_l_[0]; clockid_t clock_id; char clock_id_r_[(sizeof(register_t) <= sizeof(clockid_t) ? 0 : sizeof(register_t) - sizeof(clockid_t))];
 char tp_l_[0]; struct timespec * tp; char tp_r_[(sizeof(register_t) <= sizeof(struct timespec *) ? 0 : sizeof(register_t) - sizeof(struct timespec *))];
};
struct clock_settime_args {
 char clock_id_l_[0]; clockid_t clock_id; char clock_id_r_[(sizeof(register_t) <= sizeof(clockid_t) ? 0 : sizeof(register_t) - sizeof(clockid_t))];
 char tp_l_[0]; const struct timespec * tp; char tp_r_[(sizeof(register_t) <= sizeof(const struct timespec *) ? 0 : sizeof(register_t) - sizeof(const struct timespec *))];
};
struct clock_getres_args {
 char clock_id_l_[0]; clockid_t clock_id; char clock_id_r_[(sizeof(register_t) <= sizeof(clockid_t) ? 0 : sizeof(register_t) - sizeof(clockid_t))];
 char tp_l_[0]; struct timespec * tp; char tp_r_[(sizeof(register_t) <= sizeof(struct timespec *) ? 0 : sizeof(register_t) - sizeof(struct timespec *))];
};
struct ktimer_create_args {
 char clock_id_l_[0]; clockid_t clock_id; char clock_id_r_[(sizeof(register_t) <= sizeof(clockid_t) ? 0 : sizeof(register_t) - sizeof(clockid_t))];
 char evp_l_[0]; struct sigevent * evp; char evp_r_[(sizeof(register_t) <= sizeof(struct sigevent *) ? 0 : sizeof(register_t) - sizeof(struct sigevent *))];
 char timerid_l_[0]; int * timerid; char timerid_r_[(sizeof(register_t) <= sizeof(int *) ? 0 : sizeof(register_t) - sizeof(int *))];
};
struct ktimer_delete_args {
 char timerid_l_[0]; int timerid; char timerid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct ktimer_settime_args {
 char timerid_l_[0]; int timerid; char timerid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char value_l_[0]; const struct itimerspec * value; char value_r_[(sizeof(register_t) <= sizeof(const struct itimerspec *) ? 0 : sizeof(register_t) - sizeof(const struct itimerspec *))];
 char ovalue_l_[0]; struct itimerspec * ovalue; char ovalue_r_[(sizeof(register_t) <= sizeof(struct itimerspec *) ? 0 : sizeof(register_t) - sizeof(struct itimerspec *))];
};
struct ktimer_gettime_args {
 char timerid_l_[0]; int timerid; char timerid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char value_l_[0]; struct itimerspec * value; char value_r_[(sizeof(register_t) <= sizeof(struct itimerspec *) ? 0 : sizeof(register_t) - sizeof(struct itimerspec *))];
};
struct ktimer_getoverrun_args {
 char timerid_l_[0]; int timerid; char timerid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct nanosleep_args {
 char rqtp_l_[0]; const struct timespec * rqtp; char rqtp_r_[(sizeof(register_t) <= sizeof(const struct timespec *) ? 0 : sizeof(register_t) - sizeof(const struct timespec *))];
 char rmtp_l_[0]; struct timespec * rmtp; char rmtp_r_[(sizeof(register_t) <= sizeof(struct timespec *) ? 0 : sizeof(register_t) - sizeof(struct timespec *))];
};
struct ffclock_getcounter_args {
 char ffcount_l_[0]; ffcounter * ffcount; char ffcount_r_[(sizeof(register_t) <= sizeof(ffcounter *) ? 0 : sizeof(register_t) - sizeof(ffcounter *))];
};
struct ffclock_setestimate_args {
 char cest_l_[0]; struct ffclock_estimate * cest; char cest_r_[(sizeof(register_t) <= sizeof(struct ffclock_estimate *) ? 0 : sizeof(register_t) - sizeof(struct ffclock_estimate *))];
};
struct ffclock_getestimate_args {
 char cest_l_[0]; struct ffclock_estimate * cest; char cest_r_[(sizeof(register_t) <= sizeof(struct ffclock_estimate *) ? 0 : sizeof(register_t) - sizeof(struct ffclock_estimate *))];
};
struct clock_getcpuclockid2_args {
 char id_l_[0]; id_t id; char id_r_[(sizeof(register_t) <= sizeof(id_t) ? 0 : sizeof(register_t) - sizeof(id_t))];
 char which_l_[0]; int which; char which_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char clock_id_l_[0]; clockid_t * clock_id; char clock_id_r_[(sizeof(register_t) <= sizeof(clockid_t *) ? 0 : sizeof(register_t) - sizeof(clockid_t *))];
};
struct ntp_gettime_args {
 char ntvp_l_[0]; struct ntptimeval * ntvp; char ntvp_r_[(sizeof(register_t) <= sizeof(struct ntptimeval *) ? 0 : sizeof(register_t) - sizeof(struct ntptimeval *))];
};
struct minherit_args {
 char addr_l_[0]; void * addr; char addr_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char len_l_[0]; size_t len; char len_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char inherit_l_[0]; int inherit; char inherit_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct rfork_args {
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct openbsd_poll_args {
 char fds_l_[0]; struct pollfd * fds; char fds_r_[(sizeof(register_t) <= sizeof(struct pollfd *) ? 0 : sizeof(register_t) - sizeof(struct pollfd *))];
 char nfds_l_[0]; u_int nfds; char nfds_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
 char timeout_l_[0]; int timeout; char timeout_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct issetugid_args {
 register_t dummy;
};
struct lchown_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char uid_l_[0]; int uid; char uid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char gid_l_[0]; int gid; char gid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct aio_read_args {
 char aiocbp_l_[0]; struct aiocb * aiocbp; char aiocbp_r_[(sizeof(register_t) <= sizeof(struct aiocb *) ? 0 : sizeof(register_t) - sizeof(struct aiocb *))];
};
struct aio_write_args {
 char aiocbp_l_[0]; struct aiocb * aiocbp; char aiocbp_r_[(sizeof(register_t) <= sizeof(struct aiocb *) ? 0 : sizeof(register_t) - sizeof(struct aiocb *))];
};
struct lio_listio_args {
 char mode_l_[0]; int mode; char mode_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char acb_list_l_[0]; struct aiocb *const * acb_list; char acb_list_r_[(sizeof(register_t) <= sizeof(struct aiocb *const *) ? 0 : sizeof(register_t) - sizeof(struct aiocb *const *))];
 char nent_l_[0]; int nent; char nent_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char sig_l_[0]; struct sigevent * sig; char sig_r_[(sizeof(register_t) <= sizeof(struct sigevent *) ? 0 : sizeof(register_t) - sizeof(struct sigevent *))];
};
struct getdents_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char buf_l_[0]; char * buf; char buf_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char count_l_[0]; size_t count; char count_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct lchmod_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char mode_l_[0]; mode_t mode; char mode_r_[(sizeof(register_t) <= sizeof(mode_t) ? 0 : sizeof(register_t) - sizeof(mode_t))];
};
struct lutimes_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char tptr_l_[0]; struct timeval * tptr; char tptr_r_[(sizeof(register_t) <= sizeof(struct timeval *) ? 0 : sizeof(register_t) - sizeof(struct timeval *))];
};
struct nstat_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char ub_l_[0]; struct nstat * ub; char ub_r_[(sizeof(register_t) <= sizeof(struct nstat *) ? 0 : sizeof(register_t) - sizeof(struct nstat *))];
};
struct nfstat_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char sb_l_[0]; struct nstat * sb; char sb_r_[(sizeof(register_t) <= sizeof(struct nstat *) ? 0 : sizeof(register_t) - sizeof(struct nstat *))];
};
struct nlstat_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char ub_l_[0]; struct nstat * ub; char ub_r_[(sizeof(register_t) <= sizeof(struct nstat *) ? 0 : sizeof(register_t) - sizeof(struct nstat *))];
};
struct preadv_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char iovp_l_[0]; struct iovec * iovp; char iovp_r_[(sizeof(register_t) <= sizeof(struct iovec *) ? 0 : sizeof(register_t) - sizeof(struct iovec *))];
 char iovcnt_l_[0]; u_int iovcnt; char iovcnt_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
 char offset_l_[0]; off_t offset; char offset_r_[(sizeof(register_t) <= sizeof(off_t) ? 0 : sizeof(register_t) - sizeof(off_t))];
};
struct pwritev_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char iovp_l_[0]; struct iovec * iovp; char iovp_r_[(sizeof(register_t) <= sizeof(struct iovec *) ? 0 : sizeof(register_t) - sizeof(struct iovec *))];
 char iovcnt_l_[0]; u_int iovcnt; char iovcnt_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
 char offset_l_[0]; off_t offset; char offset_r_[(sizeof(register_t) <= sizeof(off_t) ? 0 : sizeof(register_t) - sizeof(off_t))];
};
struct fhopen_args {
 char u_fhp_l_[0]; const struct fhandle * u_fhp; char u_fhp_r_[(sizeof(register_t) <= sizeof(const struct fhandle *) ? 0 : sizeof(register_t) - sizeof(const struct fhandle *))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct fhstat_args {
 char u_fhp_l_[0]; const struct fhandle * u_fhp; char u_fhp_r_[(sizeof(register_t) <= sizeof(const struct fhandle *) ? 0 : sizeof(register_t) - sizeof(const struct fhandle *))];
 char sb_l_[0]; struct stat * sb; char sb_r_[(sizeof(register_t) <= sizeof(struct stat *) ? 0 : sizeof(register_t) - sizeof(struct stat *))];
};
struct modnext_args {
 char modid_l_[0]; int modid; char modid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct modstat_args {
 char modid_l_[0]; int modid; char modid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char stat_l_[0]; struct module_stat * stat; char stat_r_[(sizeof(register_t) <= sizeof(struct module_stat *) ? 0 : sizeof(register_t) - sizeof(struct module_stat *))];
};
struct modfnext_args {
 char modid_l_[0]; int modid; char modid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct modfind_args {
 char name_l_[0]; const char * name; char name_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
};
struct kldload_args {
 char file_l_[0]; const char * file; char file_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
};
struct kldunload_args {
 char fileid_l_[0]; int fileid; char fileid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct kldfind_args {
 char file_l_[0]; const char * file; char file_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
};
struct kldnext_args {
 char fileid_l_[0]; int fileid; char fileid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct kldstat_args {
 char fileid_l_[0]; int fileid; char fileid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char stat_l_[0]; struct kld_file_stat * stat; char stat_r_[(sizeof(register_t) <= sizeof(struct kld_file_stat *) ? 0 : sizeof(register_t) - sizeof(struct kld_file_stat *))];
};
struct kldfirstmod_args {
 char fileid_l_[0]; int fileid; char fileid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct getsid_args {
 char pid_l_[0]; pid_t pid; char pid_r_[(sizeof(register_t) <= sizeof(pid_t) ? 0 : sizeof(register_t) - sizeof(pid_t))];
};
struct setresuid_args {
 char ruid_l_[0]; uid_t ruid; char ruid_r_[(sizeof(register_t) <= sizeof(uid_t) ? 0 : sizeof(register_t) - sizeof(uid_t))];
 char euid_l_[0]; uid_t euid; char euid_r_[(sizeof(register_t) <= sizeof(uid_t) ? 0 : sizeof(register_t) - sizeof(uid_t))];
 char suid_l_[0]; uid_t suid; char suid_r_[(sizeof(register_t) <= sizeof(uid_t) ? 0 : sizeof(register_t) - sizeof(uid_t))];
};
struct setresgid_args {
 char rgid_l_[0]; gid_t rgid; char rgid_r_[(sizeof(register_t) <= sizeof(gid_t) ? 0 : sizeof(register_t) - sizeof(gid_t))];
 char egid_l_[0]; gid_t egid; char egid_r_[(sizeof(register_t) <= sizeof(gid_t) ? 0 : sizeof(register_t) - sizeof(gid_t))];
 char sgid_l_[0]; gid_t sgid; char sgid_r_[(sizeof(register_t) <= sizeof(gid_t) ? 0 : sizeof(register_t) - sizeof(gid_t))];
};
struct aio_return_args {
 char aiocbp_l_[0]; struct aiocb * aiocbp; char aiocbp_r_[(sizeof(register_t) <= sizeof(struct aiocb *) ? 0 : sizeof(register_t) - sizeof(struct aiocb *))];
};
struct aio_suspend_args {
 char aiocbp_l_[0]; struct aiocb *const * aiocbp; char aiocbp_r_[(sizeof(register_t) <= sizeof(struct aiocb *const *) ? 0 : sizeof(register_t) - sizeof(struct aiocb *const *))];
 char nent_l_[0]; int nent; char nent_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char timeout_l_[0]; const struct timespec * timeout; char timeout_r_[(sizeof(register_t) <= sizeof(const struct timespec *) ? 0 : sizeof(register_t) - sizeof(const struct timespec *))];
};
struct aio_cancel_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char aiocbp_l_[0]; struct aiocb * aiocbp; char aiocbp_r_[(sizeof(register_t) <= sizeof(struct aiocb *) ? 0 : sizeof(register_t) - sizeof(struct aiocb *))];
};
struct aio_error_args {
 char aiocbp_l_[0]; struct aiocb * aiocbp; char aiocbp_r_[(sizeof(register_t) <= sizeof(struct aiocb *) ? 0 : sizeof(register_t) - sizeof(struct aiocb *))];
};
struct oaio_read_args {
 char aiocbp_l_[0]; struct oaiocb * aiocbp; char aiocbp_r_[(sizeof(register_t) <= sizeof(struct oaiocb *) ? 0 : sizeof(register_t) - sizeof(struct oaiocb *))];
};
struct oaio_write_args {
 char aiocbp_l_[0]; struct oaiocb * aiocbp; char aiocbp_r_[(sizeof(register_t) <= sizeof(struct oaiocb *) ? 0 : sizeof(register_t) - sizeof(struct oaiocb *))];
};
struct olio_listio_args {
 char mode_l_[0]; int mode; char mode_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char acb_list_l_[0]; struct oaiocb *const * acb_list; char acb_list_r_[(sizeof(register_t) <= sizeof(struct oaiocb *const *) ? 0 : sizeof(register_t) - sizeof(struct oaiocb *const *))];
 char nent_l_[0]; int nent; char nent_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char sig_l_[0]; struct osigevent * sig; char sig_r_[(sizeof(register_t) <= sizeof(struct osigevent *) ? 0 : sizeof(register_t) - sizeof(struct osigevent *))];
};
struct yield_args {
 register_t dummy;
};
struct mlockall_args {
 char how_l_[0]; int how; char how_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct munlockall_args {
 register_t dummy;
};
struct __getcwd_args {
 char buf_l_[0]; u_char * buf; char buf_r_[(sizeof(register_t) <= sizeof(u_char *) ? 0 : sizeof(register_t) - sizeof(u_char *))];
 char buflen_l_[0]; u_int buflen; char buflen_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
};
struct sched_setparam_args {
 char pid_l_[0]; pid_t pid; char pid_r_[(sizeof(register_t) <= sizeof(pid_t) ? 0 : sizeof(register_t) - sizeof(pid_t))];
 char param_l_[0]; const struct sched_param * param; char param_r_[(sizeof(register_t) <= sizeof(const struct sched_param *) ? 0 : sizeof(register_t) - sizeof(const struct sched_param *))];
};
struct sched_getparam_args {
 char pid_l_[0]; pid_t pid; char pid_r_[(sizeof(register_t) <= sizeof(pid_t) ? 0 : sizeof(register_t) - sizeof(pid_t))];
 char param_l_[0]; struct sched_param * param; char param_r_[(sizeof(register_t) <= sizeof(struct sched_param *) ? 0 : sizeof(register_t) - sizeof(struct sched_param *))];
};
struct sched_setscheduler_args {
 char pid_l_[0]; pid_t pid; char pid_r_[(sizeof(register_t) <= sizeof(pid_t) ? 0 : sizeof(register_t) - sizeof(pid_t))];
 char policy_l_[0]; int policy; char policy_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char param_l_[0]; const struct sched_param * param; char param_r_[(sizeof(register_t) <= sizeof(const struct sched_param *) ? 0 : sizeof(register_t) - sizeof(const struct sched_param *))];
};
struct sched_getscheduler_args {
 char pid_l_[0]; pid_t pid; char pid_r_[(sizeof(register_t) <= sizeof(pid_t) ? 0 : sizeof(register_t) - sizeof(pid_t))];
};
struct sched_yield_args {
 register_t dummy;
};
struct sched_get_priority_max_args {
 char policy_l_[0]; int policy; char policy_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct sched_get_priority_min_args {
 char policy_l_[0]; int policy; char policy_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct sched_rr_get_interval_args {
 char pid_l_[0]; pid_t pid; char pid_r_[(sizeof(register_t) <= sizeof(pid_t) ? 0 : sizeof(register_t) - sizeof(pid_t))];
 char interval_l_[0]; struct timespec * interval; char interval_r_[(sizeof(register_t) <= sizeof(struct timespec *) ? 0 : sizeof(register_t) - sizeof(struct timespec *))];
};
struct utrace_args {
 char addr_l_[0]; const void * addr; char addr_r_[(sizeof(register_t) <= sizeof(const void *) ? 0 : sizeof(register_t) - sizeof(const void *))];
 char len_l_[0]; size_t len; char len_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct kldsym_args {
 char fileid_l_[0]; int fileid; char fileid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char cmd_l_[0]; int cmd; char cmd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char data_l_[0]; void * data; char data_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
};
struct jail_args {
 char jail_l_[0]; struct jail * jail; char jail_r_[(sizeof(register_t) <= sizeof(struct jail *) ? 0 : sizeof(register_t) - sizeof(struct jail *))];
};
struct nnpfs_syscall_args {
 char operation_l_[0]; int operation; char operation_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char a_pathP_l_[0]; char * a_pathP; char a_pathP_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char a_opcode_l_[0]; int a_opcode; char a_opcode_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char a_paramsP_l_[0]; void * a_paramsP; char a_paramsP_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char a_followSymlinks_l_[0]; int a_followSymlinks; char a_followSymlinks_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct sigprocmask_args {
 char how_l_[0]; int how; char how_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char set_l_[0]; const sigset_t * set; char set_r_[(sizeof(register_t) <= sizeof(const sigset_t *) ? 0 : sizeof(register_t) - sizeof(const sigset_t *))];
 char oset_l_[0]; sigset_t * oset; char oset_r_[(sizeof(register_t) <= sizeof(sigset_t *) ? 0 : sizeof(register_t) - sizeof(sigset_t *))];
};
struct sigsuspend_args {
 char sigmask_l_[0]; const sigset_t * sigmask; char sigmask_r_[(sizeof(register_t) <= sizeof(const sigset_t *) ? 0 : sizeof(register_t) - sizeof(const sigset_t *))];
};
struct sigpending_args {
 char set_l_[0]; sigset_t * set; char set_r_[(sizeof(register_t) <= sizeof(sigset_t *) ? 0 : sizeof(register_t) - sizeof(sigset_t *))];
};
struct sigtimedwait_args {
 char set_l_[0]; const sigset_t * set; char set_r_[(sizeof(register_t) <= sizeof(const sigset_t *) ? 0 : sizeof(register_t) - sizeof(const sigset_t *))];
 char info_l_[0]; siginfo_t * info; char info_r_[(sizeof(register_t) <= sizeof(siginfo_t *) ? 0 : sizeof(register_t) - sizeof(siginfo_t *))];
 char timeout_l_[0]; const struct timespec * timeout; char timeout_r_[(sizeof(register_t) <= sizeof(const struct timespec *) ? 0 : sizeof(register_t) - sizeof(const struct timespec *))];
};
struct sigwaitinfo_args {
 char set_l_[0]; const sigset_t * set; char set_r_[(sizeof(register_t) <= sizeof(const sigset_t *) ? 0 : sizeof(register_t) - sizeof(const sigset_t *))];
 char info_l_[0]; siginfo_t * info; char info_r_[(sizeof(register_t) <= sizeof(siginfo_t *) ? 0 : sizeof(register_t) - sizeof(siginfo_t *))];
};
struct __acl_get_file_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char type_l_[0]; acl_type_t type; char type_r_[(sizeof(register_t) <= sizeof(acl_type_t) ? 0 : sizeof(register_t) - sizeof(acl_type_t))];
 char aclp_l_[0]; struct acl * aclp; char aclp_r_[(sizeof(register_t) <= sizeof(struct acl *) ? 0 : sizeof(register_t) - sizeof(struct acl *))];
};
struct __acl_set_file_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char type_l_[0]; acl_type_t type; char type_r_[(sizeof(register_t) <= sizeof(acl_type_t) ? 0 : sizeof(register_t) - sizeof(acl_type_t))];
 char aclp_l_[0]; struct acl * aclp; char aclp_r_[(sizeof(register_t) <= sizeof(struct acl *) ? 0 : sizeof(register_t) - sizeof(struct acl *))];
};
struct __acl_get_fd_args {
 char filedes_l_[0]; int filedes; char filedes_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char type_l_[0]; acl_type_t type; char type_r_[(sizeof(register_t) <= sizeof(acl_type_t) ? 0 : sizeof(register_t) - sizeof(acl_type_t))];
 char aclp_l_[0]; struct acl * aclp; char aclp_r_[(sizeof(register_t) <= sizeof(struct acl *) ? 0 : sizeof(register_t) - sizeof(struct acl *))];
};
struct __acl_set_fd_args {
 char filedes_l_[0]; int filedes; char filedes_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char type_l_[0]; acl_type_t type; char type_r_[(sizeof(register_t) <= sizeof(acl_type_t) ? 0 : sizeof(register_t) - sizeof(acl_type_t))];
 char aclp_l_[0]; struct acl * aclp; char aclp_r_[(sizeof(register_t) <= sizeof(struct acl *) ? 0 : sizeof(register_t) - sizeof(struct acl *))];
};
struct __acl_delete_file_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char type_l_[0]; acl_type_t type; char type_r_[(sizeof(register_t) <= sizeof(acl_type_t) ? 0 : sizeof(register_t) - sizeof(acl_type_t))];
};
struct __acl_delete_fd_args {
 char filedes_l_[0]; int filedes; char filedes_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char type_l_[0]; acl_type_t type; char type_r_[(sizeof(register_t) <= sizeof(acl_type_t) ? 0 : sizeof(register_t) - sizeof(acl_type_t))];
};
struct __acl_aclcheck_file_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char type_l_[0]; acl_type_t type; char type_r_[(sizeof(register_t) <= sizeof(acl_type_t) ? 0 : sizeof(register_t) - sizeof(acl_type_t))];
 char aclp_l_[0]; struct acl * aclp; char aclp_r_[(sizeof(register_t) <= sizeof(struct acl *) ? 0 : sizeof(register_t) - sizeof(struct acl *))];
};
struct __acl_aclcheck_fd_args {
 char filedes_l_[0]; int filedes; char filedes_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char type_l_[0]; acl_type_t type; char type_r_[(sizeof(register_t) <= sizeof(acl_type_t) ? 0 : sizeof(register_t) - sizeof(acl_type_t))];
 char aclp_l_[0]; struct acl * aclp; char aclp_r_[(sizeof(register_t) <= sizeof(struct acl *) ? 0 : sizeof(register_t) - sizeof(struct acl *))];
};
struct extattrctl_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char cmd_l_[0]; int cmd; char cmd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char filename_l_[0]; const char * filename; char filename_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char attrnamespace_l_[0]; int attrnamespace; char attrnamespace_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char attrname_l_[0]; const char * attrname; char attrname_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
};
struct extattr_set_file_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char attrnamespace_l_[0]; int attrnamespace; char attrnamespace_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char attrname_l_[0]; const char * attrname; char attrname_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char data_l_[0]; void * data; char data_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char nbytes_l_[0]; size_t nbytes; char nbytes_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct extattr_get_file_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char attrnamespace_l_[0]; int attrnamespace; char attrnamespace_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char attrname_l_[0]; const char * attrname; char attrname_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char data_l_[0]; void * data; char data_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char nbytes_l_[0]; size_t nbytes; char nbytes_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct extattr_delete_file_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char attrnamespace_l_[0]; int attrnamespace; char attrnamespace_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char attrname_l_[0]; const char * attrname; char attrname_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
};
struct aio_waitcomplete_args {
 char aiocbp_l_[0]; struct aiocb ** aiocbp; char aiocbp_r_[(sizeof(register_t) <= sizeof(struct aiocb **) ? 0 : sizeof(register_t) - sizeof(struct aiocb **))];
 char timeout_l_[0]; struct timespec * timeout; char timeout_r_[(sizeof(register_t) <= sizeof(struct timespec *) ? 0 : sizeof(register_t) - sizeof(struct timespec *))];
};
struct getresuid_args {
 char ruid_l_[0]; uid_t * ruid; char ruid_r_[(sizeof(register_t) <= sizeof(uid_t *) ? 0 : sizeof(register_t) - sizeof(uid_t *))];
 char euid_l_[0]; uid_t * euid; char euid_r_[(sizeof(register_t) <= sizeof(uid_t *) ? 0 : sizeof(register_t) - sizeof(uid_t *))];
 char suid_l_[0]; uid_t * suid; char suid_r_[(sizeof(register_t) <= sizeof(uid_t *) ? 0 : sizeof(register_t) - sizeof(uid_t *))];
};
struct getresgid_args {
 char rgid_l_[0]; gid_t * rgid; char rgid_r_[(sizeof(register_t) <= sizeof(gid_t *) ? 0 : sizeof(register_t) - sizeof(gid_t *))];
 char egid_l_[0]; gid_t * egid; char egid_r_[(sizeof(register_t) <= sizeof(gid_t *) ? 0 : sizeof(register_t) - sizeof(gid_t *))];
 char sgid_l_[0]; gid_t * sgid; char sgid_r_[(sizeof(register_t) <= sizeof(gid_t *) ? 0 : sizeof(register_t) - sizeof(gid_t *))];
};
struct kqueue_args {
 register_t dummy;
};
struct kevent_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char changelist_l_[0]; struct kevent * changelist; char changelist_r_[(sizeof(register_t) <= sizeof(struct kevent *) ? 0 : sizeof(register_t) - sizeof(struct kevent *))];
 char nchanges_l_[0]; int nchanges; char nchanges_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char eventlist_l_[0]; struct kevent * eventlist; char eventlist_r_[(sizeof(register_t) <= sizeof(struct kevent *) ? 0 : sizeof(register_t) - sizeof(struct kevent *))];
 char nevents_l_[0]; int nevents; char nevents_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char timeout_l_[0]; const struct timespec * timeout; char timeout_r_[(sizeof(register_t) <= sizeof(const struct timespec *) ? 0 : sizeof(register_t) - sizeof(const struct timespec *))];
};
struct extattr_set_fd_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char attrnamespace_l_[0]; int attrnamespace; char attrnamespace_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char attrname_l_[0]; const char * attrname; char attrname_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char data_l_[0]; void * data; char data_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char nbytes_l_[0]; size_t nbytes; char nbytes_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct extattr_get_fd_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char attrnamespace_l_[0]; int attrnamespace; char attrnamespace_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char attrname_l_[0]; const char * attrname; char attrname_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char data_l_[0]; void * data; char data_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char nbytes_l_[0]; size_t nbytes; char nbytes_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct extattr_delete_fd_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char attrnamespace_l_[0]; int attrnamespace; char attrnamespace_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char attrname_l_[0]; const char * attrname; char attrname_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
};
struct __setugid_args {
 char flag_l_[0]; int flag; char flag_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct eaccess_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char amode_l_[0]; int amode; char amode_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct afs3_syscall_args {
 char syscall_l_[0]; long syscall; char syscall_r_[(sizeof(register_t) <= sizeof(long) ? 0 : sizeof(register_t) - sizeof(long))];
 char parm1_l_[0]; long parm1; char parm1_r_[(sizeof(register_t) <= sizeof(long) ? 0 : sizeof(register_t) - sizeof(long))];
 char parm2_l_[0]; long parm2; char parm2_r_[(sizeof(register_t) <= sizeof(long) ? 0 : sizeof(register_t) - sizeof(long))];
 char parm3_l_[0]; long parm3; char parm3_r_[(sizeof(register_t) <= sizeof(long) ? 0 : sizeof(register_t) - sizeof(long))];
 char parm4_l_[0]; long parm4; char parm4_r_[(sizeof(register_t) <= sizeof(long) ? 0 : sizeof(register_t) - sizeof(long))];
 char parm5_l_[0]; long parm5; char parm5_r_[(sizeof(register_t) <= sizeof(long) ? 0 : sizeof(register_t) - sizeof(long))];
 char parm6_l_[0]; long parm6; char parm6_r_[(sizeof(register_t) <= sizeof(long) ? 0 : sizeof(register_t) - sizeof(long))];
};
struct nmount_args {
 char iovp_l_[0]; struct iovec * iovp; char iovp_r_[(sizeof(register_t) <= sizeof(struct iovec *) ? 0 : sizeof(register_t) - sizeof(struct iovec *))];
 char iovcnt_l_[0]; unsigned int iovcnt; char iovcnt_r_[(sizeof(register_t) <= sizeof(unsigned int) ? 0 : sizeof(register_t) - sizeof(unsigned int))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct __mac_get_proc_args {
 char mac_p_l_[0]; struct mac * mac_p; char mac_p_r_[(sizeof(register_t) <= sizeof(struct mac *) ? 0 : sizeof(register_t) - sizeof(struct mac *))];
};
struct __mac_set_proc_args {
 char mac_p_l_[0]; struct mac * mac_p; char mac_p_r_[(sizeof(register_t) <= sizeof(struct mac *) ? 0 : sizeof(register_t) - sizeof(struct mac *))];
};
struct __mac_get_fd_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char mac_p_l_[0]; struct mac * mac_p; char mac_p_r_[(sizeof(register_t) <= sizeof(struct mac *) ? 0 : sizeof(register_t) - sizeof(struct mac *))];
};
struct __mac_get_file_args {
 char path_p_l_[0]; const char * path_p; char path_p_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char mac_p_l_[0]; struct mac * mac_p; char mac_p_r_[(sizeof(register_t) <= sizeof(struct mac *) ? 0 : sizeof(register_t) - sizeof(struct mac *))];
};
struct __mac_set_fd_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char mac_p_l_[0]; struct mac * mac_p; char mac_p_r_[(sizeof(register_t) <= sizeof(struct mac *) ? 0 : sizeof(register_t) - sizeof(struct mac *))];
};
struct __mac_set_file_args {
 char path_p_l_[0]; const char * path_p; char path_p_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char mac_p_l_[0]; struct mac * mac_p; char mac_p_r_[(sizeof(register_t) <= sizeof(struct mac *) ? 0 : sizeof(register_t) - sizeof(struct mac *))];
};
struct kenv_args {
 char what_l_[0]; int what; char what_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char name_l_[0]; const char * name; char name_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char value_l_[0]; char * value; char value_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char len_l_[0]; int len; char len_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct lchflags_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char flags_l_[0]; u_long flags; char flags_r_[(sizeof(register_t) <= sizeof(u_long) ? 0 : sizeof(register_t) - sizeof(u_long))];
};
struct uuidgen_args {
 char store_l_[0]; struct uuid * store; char store_r_[(sizeof(register_t) <= sizeof(struct uuid *) ? 0 : sizeof(register_t) - sizeof(struct uuid *))];
 char count_l_[0]; int count; char count_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct sendfile_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char s_l_[0]; int s; char s_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char offset_l_[0]; off_t offset; char offset_r_[(sizeof(register_t) <= sizeof(off_t) ? 0 : sizeof(register_t) - sizeof(off_t))];
 char nbytes_l_[0]; size_t nbytes; char nbytes_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char hdtr_l_[0]; struct sf_hdtr * hdtr; char hdtr_r_[(sizeof(register_t) <= sizeof(struct sf_hdtr *) ? 0 : sizeof(register_t) - sizeof(struct sf_hdtr *))];
 char sbytes_l_[0]; off_t * sbytes; char sbytes_r_[(sizeof(register_t) <= sizeof(off_t *) ? 0 : sizeof(register_t) - sizeof(off_t *))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct mac_syscall_args {
 char policy_l_[0]; const char * policy; char policy_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char call_l_[0]; int call; char call_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char arg_l_[0]; void * arg; char arg_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
};
struct getfsstat_args {
 char buf_l_[0]; struct statfs * buf; char buf_r_[(sizeof(register_t) <= sizeof(struct statfs *) ? 0 : sizeof(register_t) - sizeof(struct statfs *))];
 char bufsize_l_[0]; long bufsize; char bufsize_r_[(sizeof(register_t) <= sizeof(long) ? 0 : sizeof(register_t) - sizeof(long))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct statfs_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char buf_l_[0]; struct statfs * buf; char buf_r_[(sizeof(register_t) <= sizeof(struct statfs *) ? 0 : sizeof(register_t) - sizeof(struct statfs *))];
};
struct fstatfs_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char buf_l_[0]; struct statfs * buf; char buf_r_[(sizeof(register_t) <= sizeof(struct statfs *) ? 0 : sizeof(register_t) - sizeof(struct statfs *))];
};
struct fhstatfs_args {
 char u_fhp_l_[0]; const struct fhandle * u_fhp; char u_fhp_r_[(sizeof(register_t) <= sizeof(const struct fhandle *) ? 0 : sizeof(register_t) - sizeof(const struct fhandle *))];
 char buf_l_[0]; struct statfs * buf; char buf_r_[(sizeof(register_t) <= sizeof(struct statfs *) ? 0 : sizeof(register_t) - sizeof(struct statfs *))];
};
struct ksem_close_args {
 char id_l_[0]; semid_t id; char id_r_[(sizeof(register_t) <= sizeof(semid_t) ? 0 : sizeof(register_t) - sizeof(semid_t))];
};
struct ksem_post_args {
 char id_l_[0]; semid_t id; char id_r_[(sizeof(register_t) <= sizeof(semid_t) ? 0 : sizeof(register_t) - sizeof(semid_t))];
};
struct ksem_wait_args {
 char id_l_[0]; semid_t id; char id_r_[(sizeof(register_t) <= sizeof(semid_t) ? 0 : sizeof(register_t) - sizeof(semid_t))];
};
struct ksem_trywait_args {
 char id_l_[0]; semid_t id; char id_r_[(sizeof(register_t) <= sizeof(semid_t) ? 0 : sizeof(register_t) - sizeof(semid_t))];
};
struct ksem_init_args {
 char idp_l_[0]; semid_t * idp; char idp_r_[(sizeof(register_t) <= sizeof(semid_t *) ? 0 : sizeof(register_t) - sizeof(semid_t *))];
 char value_l_[0]; unsigned int value; char value_r_[(sizeof(register_t) <= sizeof(unsigned int) ? 0 : sizeof(register_t) - sizeof(unsigned int))];
};
struct ksem_open_args {
 char idp_l_[0]; semid_t * idp; char idp_r_[(sizeof(register_t) <= sizeof(semid_t *) ? 0 : sizeof(register_t) - sizeof(semid_t *))];
 char name_l_[0]; const char * name; char name_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char oflag_l_[0]; int oflag; char oflag_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char mode_l_[0]; mode_t mode; char mode_r_[(sizeof(register_t) <= sizeof(mode_t) ? 0 : sizeof(register_t) - sizeof(mode_t))];
 char value_l_[0]; unsigned int value; char value_r_[(sizeof(register_t) <= sizeof(unsigned int) ? 0 : sizeof(register_t) - sizeof(unsigned int))];
};
struct ksem_unlink_args {
 char name_l_[0]; const char * name; char name_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
};
struct ksem_getvalue_args {
 char id_l_[0]; semid_t id; char id_r_[(sizeof(register_t) <= sizeof(semid_t) ? 0 : sizeof(register_t) - sizeof(semid_t))];
 char val_l_[0]; int * val; char val_r_[(sizeof(register_t) <= sizeof(int *) ? 0 : sizeof(register_t) - sizeof(int *))];
};
struct ksem_destroy_args {
 char id_l_[0]; semid_t id; char id_r_[(sizeof(register_t) <= sizeof(semid_t) ? 0 : sizeof(register_t) - sizeof(semid_t))];
};
struct __mac_get_pid_args {
 char pid_l_[0]; pid_t pid; char pid_r_[(sizeof(register_t) <= sizeof(pid_t) ? 0 : sizeof(register_t) - sizeof(pid_t))];
 char mac_p_l_[0]; struct mac * mac_p; char mac_p_r_[(sizeof(register_t) <= sizeof(struct mac *) ? 0 : sizeof(register_t) - sizeof(struct mac *))];
};
struct __mac_get_link_args {
 char path_p_l_[0]; const char * path_p; char path_p_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char mac_p_l_[0]; struct mac * mac_p; char mac_p_r_[(sizeof(register_t) <= sizeof(struct mac *) ? 0 : sizeof(register_t) - sizeof(struct mac *))];
};
struct __mac_set_link_args {
 char path_p_l_[0]; const char * path_p; char path_p_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char mac_p_l_[0]; struct mac * mac_p; char mac_p_r_[(sizeof(register_t) <= sizeof(struct mac *) ? 0 : sizeof(register_t) - sizeof(struct mac *))];
};
struct extattr_set_link_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char attrnamespace_l_[0]; int attrnamespace; char attrnamespace_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char attrname_l_[0]; const char * attrname; char attrname_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char data_l_[0]; void * data; char data_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char nbytes_l_[0]; size_t nbytes; char nbytes_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct extattr_get_link_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char attrnamespace_l_[0]; int attrnamespace; char attrnamespace_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char attrname_l_[0]; const char * attrname; char attrname_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char data_l_[0]; void * data; char data_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char nbytes_l_[0]; size_t nbytes; char nbytes_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct extattr_delete_link_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char attrnamespace_l_[0]; int attrnamespace; char attrnamespace_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char attrname_l_[0]; const char * attrname; char attrname_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
};
struct __mac_execve_args {
 char fname_l_[0]; char * fname; char fname_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char argv_l_[0]; char ** argv; char argv_r_[(sizeof(register_t) <= sizeof(char **) ? 0 : sizeof(register_t) - sizeof(char **))];
 char envv_l_[0]; char ** envv; char envv_r_[(sizeof(register_t) <= sizeof(char **) ? 0 : sizeof(register_t) - sizeof(char **))];
 char mac_p_l_[0]; struct mac * mac_p; char mac_p_r_[(sizeof(register_t) <= sizeof(struct mac *) ? 0 : sizeof(register_t) - sizeof(struct mac *))];
};
struct sigaction_args {
 char sig_l_[0]; int sig; char sig_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char act_l_[0]; const struct sigaction * act; char act_r_[(sizeof(register_t) <= sizeof(const struct sigaction *) ? 0 : sizeof(register_t) - sizeof(const struct sigaction *))];
 char oact_l_[0]; struct sigaction * oact; char oact_r_[(sizeof(register_t) <= sizeof(struct sigaction *) ? 0 : sizeof(register_t) - sizeof(struct sigaction *))];
};
struct sigreturn_args {
 char sigcntxp_l_[0]; const struct __ucontext * sigcntxp; char sigcntxp_r_[(sizeof(register_t) <= sizeof(const struct __ucontext *) ? 0 : sizeof(register_t) - sizeof(const struct __ucontext *))];
};
struct getcontext_args {
 char ucp_l_[0]; struct __ucontext * ucp; char ucp_r_[(sizeof(register_t) <= sizeof(struct __ucontext *) ? 0 : sizeof(register_t) - sizeof(struct __ucontext *))];
};
struct setcontext_args {
 char ucp_l_[0]; const struct __ucontext * ucp; char ucp_r_[(sizeof(register_t) <= sizeof(const struct __ucontext *) ? 0 : sizeof(register_t) - sizeof(const struct __ucontext *))];
};
struct swapcontext_args {
 char oucp_l_[0]; struct __ucontext * oucp; char oucp_r_[(sizeof(register_t) <= sizeof(struct __ucontext *) ? 0 : sizeof(register_t) - sizeof(struct __ucontext *))];
 char ucp_l_[0]; const struct __ucontext * ucp; char ucp_r_[(sizeof(register_t) <= sizeof(const struct __ucontext *) ? 0 : sizeof(register_t) - sizeof(const struct __ucontext *))];
};
struct swapoff_args {
 char name_l_[0]; const char * name; char name_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
};
struct __acl_get_link_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char type_l_[0]; acl_type_t type; char type_r_[(sizeof(register_t) <= sizeof(acl_type_t) ? 0 : sizeof(register_t) - sizeof(acl_type_t))];
 char aclp_l_[0]; struct acl * aclp; char aclp_r_[(sizeof(register_t) <= sizeof(struct acl *) ? 0 : sizeof(register_t) - sizeof(struct acl *))];
};
struct __acl_set_link_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char type_l_[0]; acl_type_t type; char type_r_[(sizeof(register_t) <= sizeof(acl_type_t) ? 0 : sizeof(register_t) - sizeof(acl_type_t))];
 char aclp_l_[0]; struct acl * aclp; char aclp_r_[(sizeof(register_t) <= sizeof(struct acl *) ? 0 : sizeof(register_t) - sizeof(struct acl *))];
};
struct __acl_delete_link_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char type_l_[0]; acl_type_t type; char type_r_[(sizeof(register_t) <= sizeof(acl_type_t) ? 0 : sizeof(register_t) - sizeof(acl_type_t))];
};
struct __acl_aclcheck_link_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char type_l_[0]; acl_type_t type; char type_r_[(sizeof(register_t) <= sizeof(acl_type_t) ? 0 : sizeof(register_t) - sizeof(acl_type_t))];
 char aclp_l_[0]; struct acl * aclp; char aclp_r_[(sizeof(register_t) <= sizeof(struct acl *) ? 0 : sizeof(register_t) - sizeof(struct acl *))];
};
struct sigwait_args {
 char set_l_[0]; const sigset_t * set; char set_r_[(sizeof(register_t) <= sizeof(const sigset_t *) ? 0 : sizeof(register_t) - sizeof(const sigset_t *))];
 char sig_l_[0]; int * sig; char sig_r_[(sizeof(register_t) <= sizeof(int *) ? 0 : sizeof(register_t) - sizeof(int *))];
};
struct thr_create_args {
 char ctx_l_[0]; ucontext_t * ctx; char ctx_r_[(sizeof(register_t) <= sizeof(ucontext_t *) ? 0 : sizeof(register_t) - sizeof(ucontext_t *))];
 char id_l_[0]; long * id; char id_r_[(sizeof(register_t) <= sizeof(long *) ? 0 : sizeof(register_t) - sizeof(long *))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct thr_exit_args {
 char state_l_[0]; long * state; char state_r_[(sizeof(register_t) <= sizeof(long *) ? 0 : sizeof(register_t) - sizeof(long *))];
};
struct thr_self_args {
 char id_l_[0]; long * id; char id_r_[(sizeof(register_t) <= sizeof(long *) ? 0 : sizeof(register_t) - sizeof(long *))];
};
struct thr_kill_args {
 char id_l_[0]; long id; char id_r_[(sizeof(register_t) <= sizeof(long) ? 0 : sizeof(register_t) - sizeof(long))];
 char sig_l_[0]; int sig; char sig_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct _umtx_lock_args {
 char umtx_l_[0]; struct umtx * umtx; char umtx_r_[(sizeof(register_t) <= sizeof(struct umtx *) ? 0 : sizeof(register_t) - sizeof(struct umtx *))];
};
struct _umtx_unlock_args {
 char umtx_l_[0]; struct umtx * umtx; char umtx_r_[(sizeof(register_t) <= sizeof(struct umtx *) ? 0 : sizeof(register_t) - sizeof(struct umtx *))];
};
struct jail_attach_args {
 char jid_l_[0]; int jid; char jid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct extattr_list_fd_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char attrnamespace_l_[0]; int attrnamespace; char attrnamespace_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char data_l_[0]; void * data; char data_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char nbytes_l_[0]; size_t nbytes; char nbytes_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct extattr_list_file_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char attrnamespace_l_[0]; int attrnamespace; char attrnamespace_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char data_l_[0]; void * data; char data_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char nbytes_l_[0]; size_t nbytes; char nbytes_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct extattr_list_link_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char attrnamespace_l_[0]; int attrnamespace; char attrnamespace_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char data_l_[0]; void * data; char data_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char nbytes_l_[0]; size_t nbytes; char nbytes_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct ksem_timedwait_args {
 char id_l_[0]; semid_t id; char id_r_[(sizeof(register_t) <= sizeof(semid_t) ? 0 : sizeof(register_t) - sizeof(semid_t))];
 char abstime_l_[0]; const struct timespec * abstime; char abstime_r_[(sizeof(register_t) <= sizeof(const struct timespec *) ? 0 : sizeof(register_t) - sizeof(const struct timespec *))];
};
struct thr_suspend_args {
 char timeout_l_[0]; const struct timespec * timeout; char timeout_r_[(sizeof(register_t) <= sizeof(const struct timespec *) ? 0 : sizeof(register_t) - sizeof(const struct timespec *))];
};
struct thr_wake_args {
 char id_l_[0]; long id; char id_r_[(sizeof(register_t) <= sizeof(long) ? 0 : sizeof(register_t) - sizeof(long))];
};
struct kldunloadf_args {
 char fileid_l_[0]; int fileid; char fileid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct audit_args {
 char record_l_[0]; const void * record; char record_r_[(sizeof(register_t) <= sizeof(const void *) ? 0 : sizeof(register_t) - sizeof(const void *))];
 char length_l_[0]; u_int length; char length_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
};
struct auditon_args {
 char cmd_l_[0]; int cmd; char cmd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char data_l_[0]; void * data; char data_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char length_l_[0]; u_int length; char length_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
};
struct getauid_args {
 char auid_l_[0]; uid_t * auid; char auid_r_[(sizeof(register_t) <= sizeof(uid_t *) ? 0 : sizeof(register_t) - sizeof(uid_t *))];
};
struct setauid_args {
 char auid_l_[0]; uid_t * auid; char auid_r_[(sizeof(register_t) <= sizeof(uid_t *) ? 0 : sizeof(register_t) - sizeof(uid_t *))];
};
struct getaudit_args {
 char auditinfo_l_[0]; struct auditinfo * auditinfo; char auditinfo_r_[(sizeof(register_t) <= sizeof(struct auditinfo *) ? 0 : sizeof(register_t) - sizeof(struct auditinfo *))];
};
struct setaudit_args {
 char auditinfo_l_[0]; struct auditinfo * auditinfo; char auditinfo_r_[(sizeof(register_t) <= sizeof(struct auditinfo *) ? 0 : sizeof(register_t) - sizeof(struct auditinfo *))];
};
struct getaudit_addr_args {
 char auditinfo_addr_l_[0]; struct auditinfo_addr * auditinfo_addr; char auditinfo_addr_r_[(sizeof(register_t) <= sizeof(struct auditinfo_addr *) ? 0 : sizeof(register_t) - sizeof(struct auditinfo_addr *))];
 char length_l_[0]; u_int length; char length_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
};
struct setaudit_addr_args {
 char auditinfo_addr_l_[0]; struct auditinfo_addr * auditinfo_addr; char auditinfo_addr_r_[(sizeof(register_t) <= sizeof(struct auditinfo_addr *) ? 0 : sizeof(register_t) - sizeof(struct auditinfo_addr *))];
 char length_l_[0]; u_int length; char length_r_[(sizeof(register_t) <= sizeof(u_int) ? 0 : sizeof(register_t) - sizeof(u_int))];
};
struct auditctl_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
};
struct _umtx_op_args {
 char obj_l_[0]; void * obj; char obj_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char op_l_[0]; int op; char op_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char val_l_[0]; u_long val; char val_r_[(sizeof(register_t) <= sizeof(u_long) ? 0 : sizeof(register_t) - sizeof(u_long))];
 char uaddr1_l_[0]; void * uaddr1; char uaddr1_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char uaddr2_l_[0]; void * uaddr2; char uaddr2_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
};
struct thr_new_args {
 char param_l_[0]; struct thr_param * param; char param_r_[(sizeof(register_t) <= sizeof(struct thr_param *) ? 0 : sizeof(register_t) - sizeof(struct thr_param *))];
 char param_size_l_[0]; int param_size; char param_size_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct sigqueue_args {
 char pid_l_[0]; pid_t pid; char pid_r_[(sizeof(register_t) <= sizeof(pid_t) ? 0 : sizeof(register_t) - sizeof(pid_t))];
 char signum_l_[0]; int signum; char signum_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char value_l_[0]; void * value; char value_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
};
struct kmq_open_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char mode_l_[0]; mode_t mode; char mode_r_[(sizeof(register_t) <= sizeof(mode_t) ? 0 : sizeof(register_t) - sizeof(mode_t))];
 char attr_l_[0]; const struct mq_attr * attr; char attr_r_[(sizeof(register_t) <= sizeof(const struct mq_attr *) ? 0 : sizeof(register_t) - sizeof(const struct mq_attr *))];
};
struct kmq_setattr_args {
 char mqd_l_[0]; int mqd; char mqd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char attr_l_[0]; const struct mq_attr * attr; char attr_r_[(sizeof(register_t) <= sizeof(const struct mq_attr *) ? 0 : sizeof(register_t) - sizeof(const struct mq_attr *))];
 char oattr_l_[0]; struct mq_attr * oattr; char oattr_r_[(sizeof(register_t) <= sizeof(struct mq_attr *) ? 0 : sizeof(register_t) - sizeof(struct mq_attr *))];
};
struct kmq_timedreceive_args {
 char mqd_l_[0]; int mqd; char mqd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char msg_ptr_l_[0]; char * msg_ptr; char msg_ptr_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char msg_len_l_[0]; size_t msg_len; char msg_len_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char msg_prio_l_[0]; unsigned * msg_prio; char msg_prio_r_[(sizeof(register_t) <= sizeof(unsigned *) ? 0 : sizeof(register_t) - sizeof(unsigned *))];
 char abs_timeout_l_[0]; const struct timespec * abs_timeout; char abs_timeout_r_[(sizeof(register_t) <= sizeof(const struct timespec *) ? 0 : sizeof(register_t) - sizeof(const struct timespec *))];
};
struct kmq_timedsend_args {
 char mqd_l_[0]; int mqd; char mqd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char msg_ptr_l_[0]; const char * msg_ptr; char msg_ptr_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char msg_len_l_[0]; size_t msg_len; char msg_len_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char msg_prio_l_[0]; unsigned msg_prio; char msg_prio_r_[(sizeof(register_t) <= sizeof(unsigned) ? 0 : sizeof(register_t) - sizeof(unsigned))];
 char abs_timeout_l_[0]; const struct timespec * abs_timeout; char abs_timeout_r_[(sizeof(register_t) <= sizeof(const struct timespec *) ? 0 : sizeof(register_t) - sizeof(const struct timespec *))];
};
struct kmq_notify_args {
 char mqd_l_[0]; int mqd; char mqd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char sigev_l_[0]; const struct sigevent * sigev; char sigev_r_[(sizeof(register_t) <= sizeof(const struct sigevent *) ? 0 : sizeof(register_t) - sizeof(const struct sigevent *))];
};
struct kmq_unlink_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
};
struct abort2_args {
 char why_l_[0]; const char * why; char why_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char nargs_l_[0]; int nargs; char nargs_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char args_l_[0]; void ** args; char args_r_[(sizeof(register_t) <= sizeof(void **) ? 0 : sizeof(register_t) - sizeof(void **))];
};
struct thr_set_name_args {
 char id_l_[0]; long id; char id_r_[(sizeof(register_t) <= sizeof(long) ? 0 : sizeof(register_t) - sizeof(long))];
 char name_l_[0]; const char * name; char name_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
};
struct aio_fsync_args {
 char op_l_[0]; int op; char op_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char aiocbp_l_[0]; struct aiocb * aiocbp; char aiocbp_r_[(sizeof(register_t) <= sizeof(struct aiocb *) ? 0 : sizeof(register_t) - sizeof(struct aiocb *))];
};
struct rtprio_thread_args {
 char function_l_[0]; int function; char function_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char lwpid_l_[0]; lwpid_t lwpid; char lwpid_r_[(sizeof(register_t) <= sizeof(lwpid_t) ? 0 : sizeof(register_t) - sizeof(lwpid_t))];
 char rtp_l_[0]; struct rtprio * rtp; char rtp_r_[(sizeof(register_t) <= sizeof(struct rtprio *) ? 0 : sizeof(register_t) - sizeof(struct rtprio *))];
};
struct sctp_peeloff_args {
 char sd_l_[0]; int sd; char sd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char name_l_[0]; uint32_t name; char name_r_[(sizeof(register_t) <= sizeof(uint32_t) ? 0 : sizeof(register_t) - sizeof(uint32_t))];
};
struct sctp_generic_sendmsg_args {
 char sd_l_[0]; int sd; char sd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char msg_l_[0]; caddr_t msg; char msg_r_[(sizeof(register_t) <= sizeof(caddr_t) ? 0 : sizeof(register_t) - sizeof(caddr_t))];
 char mlen_l_[0]; int mlen; char mlen_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char to_l_[0]; caddr_t to; char to_r_[(sizeof(register_t) <= sizeof(caddr_t) ? 0 : sizeof(register_t) - sizeof(caddr_t))];
 char tolen_l_[0]; __socklen_t tolen; char tolen_r_[(sizeof(register_t) <= sizeof(__socklen_t) ? 0 : sizeof(register_t) - sizeof(__socklen_t))];
 char sinfo_l_[0]; struct sctp_sndrcvinfo * sinfo; char sinfo_r_[(sizeof(register_t) <= sizeof(struct sctp_sndrcvinfo *) ? 0 : sizeof(register_t) - sizeof(struct sctp_sndrcvinfo *))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct sctp_generic_sendmsg_iov_args {
 char sd_l_[0]; int sd; char sd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char iov_l_[0]; struct iovec * iov; char iov_r_[(sizeof(register_t) <= sizeof(struct iovec *) ? 0 : sizeof(register_t) - sizeof(struct iovec *))];
 char iovlen_l_[0]; int iovlen; char iovlen_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char to_l_[0]; caddr_t to; char to_r_[(sizeof(register_t) <= sizeof(caddr_t) ? 0 : sizeof(register_t) - sizeof(caddr_t))];
 char tolen_l_[0]; __socklen_t tolen; char tolen_r_[(sizeof(register_t) <= sizeof(__socklen_t) ? 0 : sizeof(register_t) - sizeof(__socklen_t))];
 char sinfo_l_[0]; struct sctp_sndrcvinfo * sinfo; char sinfo_r_[(sizeof(register_t) <= sizeof(struct sctp_sndrcvinfo *) ? 0 : sizeof(register_t) - sizeof(struct sctp_sndrcvinfo *))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct sctp_generic_recvmsg_args {
 char sd_l_[0]; int sd; char sd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char iov_l_[0]; struct iovec * iov; char iov_r_[(sizeof(register_t) <= sizeof(struct iovec *) ? 0 : sizeof(register_t) - sizeof(struct iovec *))];
 char iovlen_l_[0]; int iovlen; char iovlen_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char from_l_[0]; struct sockaddr * from; char from_r_[(sizeof(register_t) <= sizeof(struct sockaddr *) ? 0 : sizeof(register_t) - sizeof(struct sockaddr *))];
 char fromlenaddr_l_[0]; __socklen_t * fromlenaddr; char fromlenaddr_r_[(sizeof(register_t) <= sizeof(__socklen_t *) ? 0 : sizeof(register_t) - sizeof(__socklen_t *))];
 char sinfo_l_[0]; struct sctp_sndrcvinfo * sinfo; char sinfo_r_[(sizeof(register_t) <= sizeof(struct sctp_sndrcvinfo *) ? 0 : sizeof(register_t) - sizeof(struct sctp_sndrcvinfo *))];
 char msg_flags_l_[0]; int * msg_flags; char msg_flags_r_[(sizeof(register_t) <= sizeof(int *) ? 0 : sizeof(register_t) - sizeof(int *))];
};
struct pread_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char buf_l_[0]; void * buf; char buf_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char nbyte_l_[0]; size_t nbyte; char nbyte_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char offset_l_[0]; off_t offset; char offset_r_[(sizeof(register_t) <= sizeof(off_t) ? 0 : sizeof(register_t) - sizeof(off_t))];
};
struct pwrite_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char buf_l_[0]; const void * buf; char buf_r_[(sizeof(register_t) <= sizeof(const void *) ? 0 : sizeof(register_t) - sizeof(const void *))];
 char nbyte_l_[0]; size_t nbyte; char nbyte_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char offset_l_[0]; off_t offset; char offset_r_[(sizeof(register_t) <= sizeof(off_t) ? 0 : sizeof(register_t) - sizeof(off_t))];
};
struct mmap_args {
 char addr_l_[0]; caddr_t addr; char addr_r_[(sizeof(register_t) <= sizeof(caddr_t) ? 0 : sizeof(register_t) - sizeof(caddr_t))];
 char len_l_[0]; size_t len; char len_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char prot_l_[0]; int prot; char prot_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char pos_l_[0]; off_t pos; char pos_r_[(sizeof(register_t) <= sizeof(off_t) ? 0 : sizeof(register_t) - sizeof(off_t))];
};
struct lseek_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char offset_l_[0]; off_t offset; char offset_r_[(sizeof(register_t) <= sizeof(off_t) ? 0 : sizeof(register_t) - sizeof(off_t))];
 char whence_l_[0]; int whence; char whence_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct truncate_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char length_l_[0]; off_t length; char length_r_[(sizeof(register_t) <= sizeof(off_t) ? 0 : sizeof(register_t) - sizeof(off_t))];
};
struct ftruncate_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char length_l_[0]; off_t length; char length_r_[(sizeof(register_t) <= sizeof(off_t) ? 0 : sizeof(register_t) - sizeof(off_t))];
};
struct thr_kill2_args {
 char pid_l_[0]; pid_t pid; char pid_r_[(sizeof(register_t) <= sizeof(pid_t) ? 0 : sizeof(register_t) - sizeof(pid_t))];
 char id_l_[0]; long id; char id_r_[(sizeof(register_t) <= sizeof(long) ? 0 : sizeof(register_t) - sizeof(long))];
 char sig_l_[0]; int sig; char sig_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct shm_open_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char mode_l_[0]; mode_t mode; char mode_r_[(sizeof(register_t) <= sizeof(mode_t) ? 0 : sizeof(register_t) - sizeof(mode_t))];
};
struct shm_unlink_args {
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
};
struct cpuset_args {
 char setid_l_[0]; cpusetid_t * setid; char setid_r_[(sizeof(register_t) <= sizeof(cpusetid_t *) ? 0 : sizeof(register_t) - sizeof(cpusetid_t *))];
};
struct cpuset_setid_args {
 char which_l_[0]; cpuwhich_t which; char which_r_[(sizeof(register_t) <= sizeof(cpuwhich_t) ? 0 : sizeof(register_t) - sizeof(cpuwhich_t))];
 char id_l_[0]; id_t id; char id_r_[(sizeof(register_t) <= sizeof(id_t) ? 0 : sizeof(register_t) - sizeof(id_t))];
 char setid_l_[0]; cpusetid_t setid; char setid_r_[(sizeof(register_t) <= sizeof(cpusetid_t) ? 0 : sizeof(register_t) - sizeof(cpusetid_t))];
};
struct cpuset_getid_args {
 char level_l_[0]; cpulevel_t level; char level_r_[(sizeof(register_t) <= sizeof(cpulevel_t) ? 0 : sizeof(register_t) - sizeof(cpulevel_t))];
 char which_l_[0]; cpuwhich_t which; char which_r_[(sizeof(register_t) <= sizeof(cpuwhich_t) ? 0 : sizeof(register_t) - sizeof(cpuwhich_t))];
 char id_l_[0]; id_t id; char id_r_[(sizeof(register_t) <= sizeof(id_t) ? 0 : sizeof(register_t) - sizeof(id_t))];
 char setid_l_[0]; cpusetid_t * setid; char setid_r_[(sizeof(register_t) <= sizeof(cpusetid_t *) ? 0 : sizeof(register_t) - sizeof(cpusetid_t *))];
};
struct cpuset_getaffinity_args {
 char level_l_[0]; cpulevel_t level; char level_r_[(sizeof(register_t) <= sizeof(cpulevel_t) ? 0 : sizeof(register_t) - sizeof(cpulevel_t))];
 char which_l_[0]; cpuwhich_t which; char which_r_[(sizeof(register_t) <= sizeof(cpuwhich_t) ? 0 : sizeof(register_t) - sizeof(cpuwhich_t))];
 char id_l_[0]; id_t id; char id_r_[(sizeof(register_t) <= sizeof(id_t) ? 0 : sizeof(register_t) - sizeof(id_t))];
 char cpusetsize_l_[0]; size_t cpusetsize; char cpusetsize_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char mask_l_[0]; cpuset_t * mask; char mask_r_[(sizeof(register_t) <= sizeof(cpuset_t *) ? 0 : sizeof(register_t) - sizeof(cpuset_t *))];
};
struct cpuset_setaffinity_args {
 char level_l_[0]; cpulevel_t level; char level_r_[(sizeof(register_t) <= sizeof(cpulevel_t) ? 0 : sizeof(register_t) - sizeof(cpulevel_t))];
 char which_l_[0]; cpuwhich_t which; char which_r_[(sizeof(register_t) <= sizeof(cpuwhich_t) ? 0 : sizeof(register_t) - sizeof(cpuwhich_t))];
 char id_l_[0]; id_t id; char id_r_[(sizeof(register_t) <= sizeof(id_t) ? 0 : sizeof(register_t) - sizeof(id_t))];
 char cpusetsize_l_[0]; size_t cpusetsize; char cpusetsize_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char mask_l_[0]; const cpuset_t * mask; char mask_r_[(sizeof(register_t) <= sizeof(const cpuset_t *) ? 0 : sizeof(register_t) - sizeof(const cpuset_t *))];
};
struct faccessat_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char amode_l_[0]; int amode; char amode_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char flag_l_[0]; int flag; char flag_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct fchmodat_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char mode_l_[0]; mode_t mode; char mode_r_[(sizeof(register_t) <= sizeof(mode_t) ? 0 : sizeof(register_t) - sizeof(mode_t))];
 char flag_l_[0]; int flag; char flag_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct fchownat_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char uid_l_[0]; uid_t uid; char uid_r_[(sizeof(register_t) <= sizeof(uid_t) ? 0 : sizeof(register_t) - sizeof(uid_t))];
 char gid_l_[0]; gid_t gid; char gid_r_[(sizeof(register_t) <= sizeof(gid_t) ? 0 : sizeof(register_t) - sizeof(gid_t))];
 char flag_l_[0]; int flag; char flag_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct fexecve_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char argv_l_[0]; char ** argv; char argv_r_[(sizeof(register_t) <= sizeof(char **) ? 0 : sizeof(register_t) - sizeof(char **))];
 char envv_l_[0]; char ** envv; char envv_r_[(sizeof(register_t) <= sizeof(char **) ? 0 : sizeof(register_t) - sizeof(char **))];
};
struct fstatat_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char buf_l_[0]; struct stat * buf; char buf_r_[(sizeof(register_t) <= sizeof(struct stat *) ? 0 : sizeof(register_t) - sizeof(struct stat *))];
 char flag_l_[0]; int flag; char flag_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct futimesat_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char times_l_[0]; struct timeval * times; char times_r_[(sizeof(register_t) <= sizeof(struct timeval *) ? 0 : sizeof(register_t) - sizeof(struct timeval *))];
};
struct linkat_args {
 char fd1_l_[0]; int fd1; char fd1_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char path1_l_[0]; char * path1; char path1_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char fd2_l_[0]; int fd2; char fd2_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char path2_l_[0]; char * path2; char path2_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char flag_l_[0]; int flag; char flag_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct mkdirat_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char mode_l_[0]; mode_t mode; char mode_r_[(sizeof(register_t) <= sizeof(mode_t) ? 0 : sizeof(register_t) - sizeof(mode_t))];
};
struct mkfifoat_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char mode_l_[0]; mode_t mode; char mode_r_[(sizeof(register_t) <= sizeof(mode_t) ? 0 : sizeof(register_t) - sizeof(mode_t))];
};
struct mknodat_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char mode_l_[0]; mode_t mode; char mode_r_[(sizeof(register_t) <= sizeof(mode_t) ? 0 : sizeof(register_t) - sizeof(mode_t))];
 char dev_l_[0]; dev_t dev; char dev_r_[(sizeof(register_t) <= sizeof(dev_t) ? 0 : sizeof(register_t) - sizeof(dev_t))];
};
struct openat_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char flag_l_[0]; int flag; char flag_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char mode_l_[0]; mode_t mode; char mode_r_[(sizeof(register_t) <= sizeof(mode_t) ? 0 : sizeof(register_t) - sizeof(mode_t))];
};
struct readlinkat_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char buf_l_[0]; char * buf; char buf_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char bufsize_l_[0]; size_t bufsize; char bufsize_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct renameat_args {
 char oldfd_l_[0]; int oldfd; char oldfd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char old_l_[0]; char * old; char old_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char newfd_l_[0]; int newfd; char newfd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char new_l_[0]; char * new; char new_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
};
struct symlinkat_args {
 char path1_l_[0]; char * path1; char path1_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char path2_l_[0]; char * path2; char path2_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
};
struct unlinkat_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char flag_l_[0]; int flag; char flag_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct posix_openpt_args {
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct gssd_syscall_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
};
struct jail_get_args {
 char iovp_l_[0]; struct iovec * iovp; char iovp_r_[(sizeof(register_t) <= sizeof(struct iovec *) ? 0 : sizeof(register_t) - sizeof(struct iovec *))];
 char iovcnt_l_[0]; unsigned int iovcnt; char iovcnt_r_[(sizeof(register_t) <= sizeof(unsigned int) ? 0 : sizeof(register_t) - sizeof(unsigned int))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct jail_set_args {
 char iovp_l_[0]; struct iovec * iovp; char iovp_r_[(sizeof(register_t) <= sizeof(struct iovec *) ? 0 : sizeof(register_t) - sizeof(struct iovec *))];
 char iovcnt_l_[0]; unsigned int iovcnt; char iovcnt_r_[(sizeof(register_t) <= sizeof(unsigned int) ? 0 : sizeof(register_t) - sizeof(unsigned int))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct jail_remove_args {
 char jid_l_[0]; int jid; char jid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct closefrom_args {
 char lowfd_l_[0]; int lowfd; char lowfd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct __semctl_args {
 char semid_l_[0]; int semid; char semid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char semnum_l_[0]; int semnum; char semnum_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char cmd_l_[0]; int cmd; char cmd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char arg_l_[0]; union semun * arg; char arg_r_[(sizeof(register_t) <= sizeof(union semun *) ? 0 : sizeof(register_t) - sizeof(union semun *))];
};
struct msgctl_args {
 char msqid_l_[0]; int msqid; char msqid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char cmd_l_[0]; int cmd; char cmd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char buf_l_[0]; struct msqid_ds * buf; char buf_r_[(sizeof(register_t) <= sizeof(struct msqid_ds *) ? 0 : sizeof(register_t) - sizeof(struct msqid_ds *))];
};
struct shmctl_args {
 char shmid_l_[0]; int shmid; char shmid_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char cmd_l_[0]; int cmd; char cmd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char buf_l_[0]; struct shmid_ds * buf; char buf_r_[(sizeof(register_t) <= sizeof(struct shmid_ds *) ? 0 : sizeof(register_t) - sizeof(struct shmid_ds *))];
};
struct lpathconf_args {
 char path_l_[0]; char * path; char path_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char name_l_[0]; int name; char name_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct __cap_rights_get_args {
 char version_l_[0]; int version; char version_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char rightsp_l_[0]; cap_rights_t * rightsp; char rightsp_r_[(sizeof(register_t) <= sizeof(cap_rights_t *) ? 0 : sizeof(register_t) - sizeof(cap_rights_t *))];
};
struct cap_enter_args {
 register_t dummy;
};
struct cap_getmode_args {
 char modep_l_[0]; u_int * modep; char modep_r_[(sizeof(register_t) <= sizeof(u_int *) ? 0 : sizeof(register_t) - sizeof(u_int *))];
};
struct pdfork_args {
 char fdp_l_[0]; int * fdp; char fdp_r_[(sizeof(register_t) <= sizeof(int *) ? 0 : sizeof(register_t) - sizeof(int *))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct pdkill_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char signum_l_[0]; int signum; char signum_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct pdgetpid_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char pidp_l_[0]; pid_t * pidp; char pidp_r_[(sizeof(register_t) <= sizeof(pid_t *) ? 0 : sizeof(register_t) - sizeof(pid_t *))];
};
struct pselect_args {
 char nd_l_[0]; int nd; char nd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char in_l_[0]; fd_set * in; char in_r_[(sizeof(register_t) <= sizeof(fd_set *) ? 0 : sizeof(register_t) - sizeof(fd_set *))];
 char ou_l_[0]; fd_set * ou; char ou_r_[(sizeof(register_t) <= sizeof(fd_set *) ? 0 : sizeof(register_t) - sizeof(fd_set *))];
 char ex_l_[0]; fd_set * ex; char ex_r_[(sizeof(register_t) <= sizeof(fd_set *) ? 0 : sizeof(register_t) - sizeof(fd_set *))];
 char ts_l_[0]; const struct timespec * ts; char ts_r_[(sizeof(register_t) <= sizeof(const struct timespec *) ? 0 : sizeof(register_t) - sizeof(const struct timespec *))];
 char sm_l_[0]; const sigset_t * sm; char sm_r_[(sizeof(register_t) <= sizeof(const sigset_t *) ? 0 : sizeof(register_t) - sizeof(const sigset_t *))];
};
struct getloginclass_args {
 char namebuf_l_[0]; char * namebuf; char namebuf_r_[(sizeof(register_t) <= sizeof(char *) ? 0 : sizeof(register_t) - sizeof(char *))];
 char namelen_l_[0]; size_t namelen; char namelen_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct setloginclass_args {
 char namebuf_l_[0]; const char * namebuf; char namebuf_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
};
struct rctl_get_racct_args {
 char inbufp_l_[0]; const void * inbufp; char inbufp_r_[(sizeof(register_t) <= sizeof(const void *) ? 0 : sizeof(register_t) - sizeof(const void *))];
 char inbuflen_l_[0]; size_t inbuflen; char inbuflen_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char outbufp_l_[0]; void * outbufp; char outbufp_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char outbuflen_l_[0]; size_t outbuflen; char outbuflen_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct rctl_get_rules_args {
 char inbufp_l_[0]; const void * inbufp; char inbufp_r_[(sizeof(register_t) <= sizeof(const void *) ? 0 : sizeof(register_t) - sizeof(const void *))];
 char inbuflen_l_[0]; size_t inbuflen; char inbuflen_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char outbufp_l_[0]; void * outbufp; char outbufp_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char outbuflen_l_[0]; size_t outbuflen; char outbuflen_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct rctl_get_limits_args {
 char inbufp_l_[0]; const void * inbufp; char inbufp_r_[(sizeof(register_t) <= sizeof(const void *) ? 0 : sizeof(register_t) - sizeof(const void *))];
 char inbuflen_l_[0]; size_t inbuflen; char inbuflen_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char outbufp_l_[0]; void * outbufp; char outbufp_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char outbuflen_l_[0]; size_t outbuflen; char outbuflen_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct rctl_add_rule_args {
 char inbufp_l_[0]; const void * inbufp; char inbufp_r_[(sizeof(register_t) <= sizeof(const void *) ? 0 : sizeof(register_t) - sizeof(const void *))];
 char inbuflen_l_[0]; size_t inbuflen; char inbuflen_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char outbufp_l_[0]; void * outbufp; char outbufp_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char outbuflen_l_[0]; size_t outbuflen; char outbuflen_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct rctl_remove_rule_args {
 char inbufp_l_[0]; const void * inbufp; char inbufp_r_[(sizeof(register_t) <= sizeof(const void *) ? 0 : sizeof(register_t) - sizeof(const void *))];
 char inbuflen_l_[0]; size_t inbuflen; char inbuflen_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
 char outbufp_l_[0]; void * outbufp; char outbufp_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
 char outbuflen_l_[0]; size_t outbuflen; char outbuflen_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct posix_fallocate_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char offset_l_[0]; off_t offset; char offset_r_[(sizeof(register_t) <= sizeof(off_t) ? 0 : sizeof(register_t) - sizeof(off_t))];
 char len_l_[0]; off_t len; char len_r_[(sizeof(register_t) <= sizeof(off_t) ? 0 : sizeof(register_t) - sizeof(off_t))];
};
struct posix_fadvise_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char offset_l_[0]; off_t offset; char offset_r_[(sizeof(register_t) <= sizeof(off_t) ? 0 : sizeof(register_t) - sizeof(off_t))];
 char len_l_[0]; off_t len; char len_r_[(sizeof(register_t) <= sizeof(off_t) ? 0 : sizeof(register_t) - sizeof(off_t))];
 char advice_l_[0]; int advice; char advice_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct wait6_args {
 char idtype_l_[0]; idtype_t idtype; char idtype_r_[(sizeof(register_t) <= sizeof(idtype_t) ? 0 : sizeof(register_t) - sizeof(idtype_t))];
 char id_l_[0]; id_t id; char id_r_[(sizeof(register_t) <= sizeof(id_t) ? 0 : sizeof(register_t) - sizeof(id_t))];
 char status_l_[0]; int * status; char status_r_[(sizeof(register_t) <= sizeof(int *) ? 0 : sizeof(register_t) - sizeof(int *))];
 char options_l_[0]; int options; char options_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char wrusage_l_[0]; struct __wrusage * wrusage; char wrusage_r_[(sizeof(register_t) <= sizeof(struct __wrusage *) ? 0 : sizeof(register_t) - sizeof(struct __wrusage *))];
 char info_l_[0]; siginfo_t * info; char info_r_[(sizeof(register_t) <= sizeof(siginfo_t *) ? 0 : sizeof(register_t) - sizeof(siginfo_t *))];
};
struct cap_rights_limit_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char rightsp_l_[0]; cap_rights_t * rightsp; char rightsp_r_[(sizeof(register_t) <= sizeof(cap_rights_t *) ? 0 : sizeof(register_t) - sizeof(cap_rights_t *))];
};
struct cap_ioctls_limit_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char cmds_l_[0]; const u_long * cmds; char cmds_r_[(sizeof(register_t) <= sizeof(const u_long *) ? 0 : sizeof(register_t) - sizeof(const u_long *))];
 char ncmds_l_[0]; size_t ncmds; char ncmds_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct cap_ioctls_get_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char cmds_l_[0]; u_long * cmds; char cmds_r_[(sizeof(register_t) <= sizeof(u_long *) ? 0 : sizeof(register_t) - sizeof(u_long *))];
 char maxcmds_l_[0]; size_t maxcmds; char maxcmds_r_[(sizeof(register_t) <= sizeof(size_t) ? 0 : sizeof(register_t) - sizeof(size_t))];
};
struct cap_fcntls_limit_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char fcntlrights_l_[0]; uint32_t fcntlrights; char fcntlrights_r_[(sizeof(register_t) <= sizeof(uint32_t) ? 0 : sizeof(register_t) - sizeof(uint32_t))];
};
struct cap_fcntls_get_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char fcntlrightsp_l_[0]; uint32_t * fcntlrightsp; char fcntlrightsp_r_[(sizeof(register_t) <= sizeof(uint32_t *) ? 0 : sizeof(register_t) - sizeof(uint32_t *))];
};
struct bindat_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char s_l_[0]; int s; char s_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char name_l_[0]; caddr_t name; char name_r_[(sizeof(register_t) <= sizeof(caddr_t) ? 0 : sizeof(register_t) - sizeof(caddr_t))];
 char namelen_l_[0]; int namelen; char namelen_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct connectat_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char s_l_[0]; int s; char s_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char name_l_[0]; caddr_t name; char name_r_[(sizeof(register_t) <= sizeof(caddr_t) ? 0 : sizeof(register_t) - sizeof(caddr_t))];
 char namelen_l_[0]; int namelen; char namelen_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct chflagsat_args {
 char fd_l_[0]; int fd; char fd_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char path_l_[0]; const char * path; char path_r_[(sizeof(register_t) <= sizeof(const char *) ? 0 : sizeof(register_t) - sizeof(const char *))];
 char flags_l_[0]; u_long flags; char flags_r_[(sizeof(register_t) <= sizeof(u_long) ? 0 : sizeof(register_t) - sizeof(u_long))];
 char atflag_l_[0]; int atflag; char atflag_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct accept4_args {
 char s_l_[0]; int s; char s_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char name_l_[0]; struct sockaddr *restrict name; char name_r_[(sizeof(register_t) <= sizeof(struct sockaddr *restrict) ? 0 : sizeof(register_t) - sizeof(struct sockaddr *restrict))];
 char anamelen_l_[0]; __socklen_t *restrict anamelen; char anamelen_r_[(sizeof(register_t) <= sizeof(__socklen_t *restrict) ? 0 : sizeof(register_t) - sizeof(__socklen_t *restrict))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct pipe2_args {
 char fildes_l_[0]; int * fildes; char fildes_r_[(sizeof(register_t) <= sizeof(int *) ? 0 : sizeof(register_t) - sizeof(int *))];
 char flags_l_[0]; int flags; char flags_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
};
struct aio_mlock_args {
 char aiocbp_l_[0]; struct aiocb * aiocbp; char aiocbp_r_[(sizeof(register_t) <= sizeof(struct aiocb *) ? 0 : sizeof(register_t) - sizeof(struct aiocb *))];
};
struct procctl_args {
 char idtype_l_[0]; idtype_t idtype; char idtype_r_[(sizeof(register_t) <= sizeof(idtype_t) ? 0 : sizeof(register_t) - sizeof(idtype_t))];
 char id_l_[0]; id_t id; char id_r_[(sizeof(register_t) <= sizeof(id_t) ? 0 : sizeof(register_t) - sizeof(id_t))];
 char com_l_[0]; int com; char com_r_[(sizeof(register_t) <= sizeof(int) ? 0 : sizeof(register_t) - sizeof(int))];
 char data_l_[0]; void * data; char data_r_[(sizeof(register_t) <= sizeof(void *) ? 0 : sizeof(register_t) - sizeof(void *))];
};
int nosys(struct thread *, struct nosys_args *);
void sys_sys_exit(struct thread *, struct sys_exit_args *);
int sys_fork(struct thread *, struct fork_args *);
int sys_read(struct thread *, struct read_args *);
int sys_write(struct thread *, struct write_args *);
int sys_open(struct thread *, struct open_args *);
int sys_close(struct thread *, struct close_args *);
int sys_wait4(struct thread *, struct wait4_args *);
int sys_link(struct thread *, struct link_args *);
int sys_unlink(struct thread *, struct unlink_args *);
int sys_chdir(struct thread *, struct chdir_args *);
int sys_fchdir(struct thread *, struct fchdir_args *);
int sys_mknod(struct thread *, struct mknod_args *);
int sys_chmod(struct thread *, struct chmod_args *);
int sys_chown(struct thread *, struct chown_args *);
int sys_obreak(struct thread *, struct obreak_args *);
int sys_getpid(struct thread *, struct getpid_args *);
int sys_mount(struct thread *, struct mount_args *);
int sys_unmount(struct thread *, struct unmount_args *);
int sys_setuid(struct thread *, struct setuid_args *);
int sys_getuid(struct thread *, struct getuid_args *);
int sys_geteuid(struct thread *, struct geteuid_args *);
int sys_ptrace(struct thread *, struct ptrace_args *);
int sys_recvmsg(struct thread *, struct recvmsg_args *);
int sys_sendmsg(struct thread *, struct sendmsg_args *);
int sys_recvfrom(struct thread *, struct recvfrom_args *);
int sys_accept(struct thread *, struct accept_args *);
int sys_getpeername(struct thread *, struct getpeername_args *);
int sys_getsockname(struct thread *, struct getsockname_args *);
int sys_access(struct thread *, struct access_args *);
int sys_chflags(struct thread *, struct chflags_args *);
int sys_fchflags(struct thread *, struct fchflags_args *);
int sys_sync(struct thread *, struct sync_args *);
int sys_kill(struct thread *, struct kill_args *);
int sys_getppid(struct thread *, struct getppid_args *);
int sys_dup(struct thread *, struct dup_args *);
int sys_pipe(struct thread *, struct pipe_args *);
int sys_getegid(struct thread *, struct getegid_args *);
int sys_profil(struct thread *, struct profil_args *);
int sys_ktrace(struct thread *, struct ktrace_args *);
int sys_getgid(struct thread *, struct getgid_args *);
int sys_getlogin(struct thread *, struct getlogin_args *);
int sys_setlogin(struct thread *, struct setlogin_args *);
int sys_acct(struct thread *, struct acct_args *);
int sys_sigaltstack(struct thread *, struct sigaltstack_args *);
int sys_ioctl(struct thread *, struct ioctl_args *);
int sys_reboot(struct thread *, struct reboot_args *);
int sys_revoke(struct thread *, struct revoke_args *);
int sys_symlink(struct thread *, struct symlink_args *);
int sys_readlink(struct thread *, struct readlink_args *);
int sys_execve(struct thread *, struct execve_args *);
int sys_umask(struct thread *, struct umask_args *);
int sys_chroot(struct thread *, struct chroot_args *);
int sys_msync(struct thread *, struct msync_args *);
int sys_vfork(struct thread *, struct vfork_args *);
int sys_sbrk(struct thread *, struct sbrk_args *);
int sys_sstk(struct thread *, struct sstk_args *);
int sys_ovadvise(struct thread *, struct ovadvise_args *);
int sys_munmap(struct thread *, struct munmap_args *);
int sys_mprotect(struct thread *, struct mprotect_args *);
int sys_madvise(struct thread *, struct madvise_args *);
int sys_mincore(struct thread *, struct mincore_args *);
int sys_getgroups(struct thread *, struct getgroups_args *);
int sys_setgroups(struct thread *, struct setgroups_args *);
int sys_getpgrp(struct thread *, struct getpgrp_args *);
int sys_setpgid(struct thread *, struct setpgid_args *);
int sys_setitimer(struct thread *, struct setitimer_args *);
int sys_swapon(struct thread *, struct swapon_args *);
int sys_getitimer(struct thread *, struct getitimer_args *);
int sys_getdtablesize(struct thread *, struct getdtablesize_args *);
int sys_dup2(struct thread *, struct dup2_args *);
int sys_fcntl(struct thread *, struct fcntl_args *);
int sys_select(struct thread *, struct select_args *);
int sys_fsync(struct thread *, struct fsync_args *);
int sys_setpriority(struct thread *, struct setpriority_args *);
int sys_socket(struct thread *, struct socket_args *);
int sys_connect(struct thread *, struct connect_args *);
int sys_getpriority(struct thread *, struct getpriority_args *);
int sys_bind(struct thread *, struct bind_args *);
int sys_setsockopt(struct thread *, struct setsockopt_args *);
int sys_listen(struct thread *, struct listen_args *);
int sys_gettimeofday(struct thread *, struct gettimeofday_args *);
int sys_getrusage(struct thread *, struct getrusage_args *);
int sys_getsockopt(struct thread *, struct getsockopt_args *);
int sys_readv(struct thread *, struct readv_args *);
int sys_writev(struct thread *, struct writev_args *);
int sys_settimeofday(struct thread *, struct settimeofday_args *);
int sys_fchown(struct thread *, struct fchown_args *);
int sys_fchmod(struct thread *, struct fchmod_args *);
int sys_setreuid(struct thread *, struct setreuid_args *);
int sys_setregid(struct thread *, struct setregid_args *);
int sys_rename(struct thread *, struct rename_args *);
int sys_flock(struct thread *, struct flock_args *);
int sys_mkfifo(struct thread *, struct mkfifo_args *);
int sys_sendto(struct thread *, struct sendto_args *);
int sys_shutdown(struct thread *, struct shutdown_args *);
int sys_socketpair(struct thread *, struct socketpair_args *);
int sys_mkdir(struct thread *, struct mkdir_args *);
int sys_rmdir(struct thread *, struct rmdir_args *);
int sys_utimes(struct thread *, struct utimes_args *);
int sys_adjtime(struct thread *, struct adjtime_args *);
int sys_setsid(struct thread *, struct setsid_args *);
int sys_quotactl(struct thread *, struct quotactl_args *);
int sys_nlm_syscall(struct thread *, struct nlm_syscall_args *);
int sys_nfssvc(struct thread *, struct nfssvc_args *);
int sys_lgetfh(struct thread *, struct lgetfh_args *);
int sys_getfh(struct thread *, struct getfh_args *);
int sysarch(struct thread *, struct sysarch_args *);
int sys_rtprio(struct thread *, struct rtprio_args *);
int sys_semsys(struct thread *, struct semsys_args *);
int sys_msgsys(struct thread *, struct msgsys_args *);
int sys_shmsys(struct thread *, struct shmsys_args *);
int freebsd6_pread(struct thread *, struct freebsd6_pread_args *);
int freebsd6_pwrite(struct thread *, struct freebsd6_pwrite_args *);
int sys_setfib(struct thread *, struct setfib_args *);
int sys_ntp_adjtime(struct thread *, struct ntp_adjtime_args *);
int sys_setgid(struct thread *, struct setgid_args *);
int sys_setegid(struct thread *, struct setegid_args *);
int sys_seteuid(struct thread *, struct seteuid_args *);
int sys_stat(struct thread *, struct stat_args *);
int sys_fstat(struct thread *, struct fstat_args *);
int sys_lstat(struct thread *, struct lstat_args *);
int sys_pathconf(struct thread *, struct pathconf_args *);
int sys_fpathconf(struct thread *, struct fpathconf_args *);
int sys_getrlimit(struct thread *, struct __getrlimit_args *);
int sys_setrlimit(struct thread *, struct __setrlimit_args *);
int sys_getdirentries(struct thread *, struct getdirentries_args *);
int freebsd6_mmap(struct thread *, struct freebsd6_mmap_args *);
int freebsd6_lseek(struct thread *, struct freebsd6_lseek_args *);
int freebsd6_truncate(struct thread *, struct freebsd6_truncate_args *);
int freebsd6_ftruncate(struct thread *, struct freebsd6_ftruncate_args *);
int sys___sysctl(struct thread *, struct sysctl_args *);
int sys_mlock(struct thread *, struct mlock_args *);
int sys_munlock(struct thread *, struct munlock_args *);
int sys_undelete(struct thread *, struct undelete_args *);
int sys_futimes(struct thread *, struct futimes_args *);
int sys_getpgid(struct thread *, struct getpgid_args *);
int sys_poll(struct thread *, struct poll_args *);
int sys_semget(struct thread *, struct semget_args *);
int sys_semop(struct thread *, struct semop_args *);
int sys_msgget(struct thread *, struct msgget_args *);
int sys_msgsnd(struct thread *, struct msgsnd_args *);
int sys_msgrcv(struct thread *, struct msgrcv_args *);
int sys_shmat(struct thread *, struct shmat_args *);
int sys_shmdt(struct thread *, struct shmdt_args *);
int sys_shmget(struct thread *, struct shmget_args *);
int sys_clock_gettime(struct thread *, struct clock_gettime_args *);
int sys_clock_settime(struct thread *, struct clock_settime_args *);
int sys_clock_getres(struct thread *, struct clock_getres_args *);
int sys_ktimer_create(struct thread *, struct ktimer_create_args *);
int sys_ktimer_delete(struct thread *, struct ktimer_delete_args *);
int sys_ktimer_settime(struct thread *, struct ktimer_settime_args *);
int sys_ktimer_gettime(struct thread *, struct ktimer_gettime_args *);
int sys_ktimer_getoverrun(struct thread *, struct ktimer_getoverrun_args *);
int sys_nanosleep(struct thread *, struct nanosleep_args *);
int sys_ffclock_getcounter(struct thread *, struct ffclock_getcounter_args *);
int sys_ffclock_setestimate(struct thread *, struct ffclock_setestimate_args *);
int sys_ffclock_getestimate(struct thread *, struct ffclock_getestimate_args *);
int sys_clock_getcpuclockid2(struct thread *, struct clock_getcpuclockid2_args *);
int sys_ntp_gettime(struct thread *, struct ntp_gettime_args *);
int sys_minherit(struct thread *, struct minherit_args *);
int sys_rfork(struct thread *, struct rfork_args *);
int sys_openbsd_poll(struct thread *, struct openbsd_poll_args *);
int sys_issetugid(struct thread *, struct issetugid_args *);
int sys_lchown(struct thread *, struct lchown_args *);
int sys_aio_read(struct thread *, struct aio_read_args *);
int sys_aio_write(struct thread *, struct aio_write_args *);
int sys_lio_listio(struct thread *, struct lio_listio_args *);
int sys_getdents(struct thread *, struct getdents_args *);
int sys_lchmod(struct thread *, struct lchmod_args *);
int sys_lutimes(struct thread *, struct lutimes_args *);
int sys_nstat(struct thread *, struct nstat_args *);
int sys_nfstat(struct thread *, struct nfstat_args *);
int sys_nlstat(struct thread *, struct nlstat_args *);
int sys_preadv(struct thread *, struct preadv_args *);
int sys_pwritev(struct thread *, struct pwritev_args *);
int sys_fhopen(struct thread *, struct fhopen_args *);
int sys_fhstat(struct thread *, struct fhstat_args *);
int sys_modnext(struct thread *, struct modnext_args *);
int sys_modstat(struct thread *, struct modstat_args *);
int sys_modfnext(struct thread *, struct modfnext_args *);
int sys_modfind(struct thread *, struct modfind_args *);
int sys_kldload(struct thread *, struct kldload_args *);
int sys_kldunload(struct thread *, struct kldunload_args *);
int sys_kldfind(struct thread *, struct kldfind_args *);
int sys_kldnext(struct thread *, struct kldnext_args *);
int sys_kldstat(struct thread *, struct kldstat_args *);
int sys_kldfirstmod(struct thread *, struct kldfirstmod_args *);
int sys_getsid(struct thread *, struct getsid_args *);
int sys_setresuid(struct thread *, struct setresuid_args *);
int sys_setresgid(struct thread *, struct setresgid_args *);
int sys_aio_return(struct thread *, struct aio_return_args *);
int sys_aio_suspend(struct thread *, struct aio_suspend_args *);
int sys_aio_cancel(struct thread *, struct aio_cancel_args *);
int sys_aio_error(struct thread *, struct aio_error_args *);
int sys_oaio_read(struct thread *, struct oaio_read_args *);
int sys_oaio_write(struct thread *, struct oaio_write_args *);
int sys_olio_listio(struct thread *, struct olio_listio_args *);
int sys_yield(struct thread *, struct yield_args *);
int sys_mlockall(struct thread *, struct mlockall_args *);
int sys_munlockall(struct thread *, struct munlockall_args *);
int sys___getcwd(struct thread *, struct __getcwd_args *);
int sys_sched_setparam(struct thread *, struct sched_setparam_args *);
int sys_sched_getparam(struct thread *, struct sched_getparam_args *);
int sys_sched_setscheduler(struct thread *, struct sched_setscheduler_args *);
int sys_sched_getscheduler(struct thread *, struct sched_getscheduler_args *);
int sys_sched_yield(struct thread *, struct sched_yield_args *);
int sys_sched_get_priority_max(struct thread *, struct sched_get_priority_max_args *);
int sys_sched_get_priority_min(struct thread *, struct sched_get_priority_min_args *);
int sys_sched_rr_get_interval(struct thread *, struct sched_rr_get_interval_args *);
int sys_utrace(struct thread *, struct utrace_args *);
int sys_kldsym(struct thread *, struct kldsym_args *);
int sys_jail(struct thread *, struct jail_args *);
int sys_nnpfs_syscall(struct thread *, struct nnpfs_syscall_args *);
int sys_sigprocmask(struct thread *, struct sigprocmask_args *);
int sys_sigsuspend(struct thread *, struct sigsuspend_args *);
int sys_sigpending(struct thread *, struct sigpending_args *);
int sys_sigtimedwait(struct thread *, struct sigtimedwait_args *);
int sys_sigwaitinfo(struct thread *, struct sigwaitinfo_args *);
int sys___acl_get_file(struct thread *, struct __acl_get_file_args *);
int sys___acl_set_file(struct thread *, struct __acl_set_file_args *);
int sys___acl_get_fd(struct thread *, struct __acl_get_fd_args *);
int sys___acl_set_fd(struct thread *, struct __acl_set_fd_args *);
int sys___acl_delete_file(struct thread *, struct __acl_delete_file_args *);
int sys___acl_delete_fd(struct thread *, struct __acl_delete_fd_args *);
int sys___acl_aclcheck_file(struct thread *, struct __acl_aclcheck_file_args *);
int sys___acl_aclcheck_fd(struct thread *, struct __acl_aclcheck_fd_args *);
int sys_extattrctl(struct thread *, struct extattrctl_args *);
int sys_extattr_set_file(struct thread *, struct extattr_set_file_args *);
int sys_extattr_get_file(struct thread *, struct extattr_get_file_args *);
int sys_extattr_delete_file(struct thread *, struct extattr_delete_file_args *);
int sys_aio_waitcomplete(struct thread *, struct aio_waitcomplete_args *);
int sys_getresuid(struct thread *, struct getresuid_args *);
int sys_getresgid(struct thread *, struct getresgid_args *);
int sys_kqueue(struct thread *, struct kqueue_args *);
int sys_kevent(struct thread *, struct kevent_args *);
int sys_extattr_set_fd(struct thread *, struct extattr_set_fd_args *);
int sys_extattr_get_fd(struct thread *, struct extattr_get_fd_args *);
int sys_extattr_delete_fd(struct thread *, struct extattr_delete_fd_args *);
int sys___setugid(struct thread *, struct __setugid_args *);
int sys_eaccess(struct thread *, struct eaccess_args *);
int sys_afs3_syscall(struct thread *, struct afs3_syscall_args *);
int sys_nmount(struct thread *, struct nmount_args *);
int sys___mac_get_proc(struct thread *, struct __mac_get_proc_args *);
int sys___mac_set_proc(struct thread *, struct __mac_set_proc_args *);
int sys___mac_get_fd(struct thread *, struct __mac_get_fd_args *);
int sys___mac_get_file(struct thread *, struct __mac_get_file_args *);
int sys___mac_set_fd(struct thread *, struct __mac_set_fd_args *);
int sys___mac_set_file(struct thread *, struct __mac_set_file_args *);
int sys_kenv(struct thread *, struct kenv_args *);
int sys_lchflags(struct thread *, struct lchflags_args *);
int sys_uuidgen(struct thread *, struct uuidgen_args *);
int sys_sendfile(struct thread *, struct sendfile_args *);
int sys_mac_syscall(struct thread *, struct mac_syscall_args *);
int sys_getfsstat(struct thread *, struct getfsstat_args *);
int sys_statfs(struct thread *, struct statfs_args *);
int sys_fstatfs(struct thread *, struct fstatfs_args *);
int sys_fhstatfs(struct thread *, struct fhstatfs_args *);
int sys_ksem_close(struct thread *, struct ksem_close_args *);
int sys_ksem_post(struct thread *, struct ksem_post_args *);
int sys_ksem_wait(struct thread *, struct ksem_wait_args *);
int sys_ksem_trywait(struct thread *, struct ksem_trywait_args *);
int sys_ksem_init(struct thread *, struct ksem_init_args *);
int sys_ksem_open(struct thread *, struct ksem_open_args *);
int sys_ksem_unlink(struct thread *, struct ksem_unlink_args *);
int sys_ksem_getvalue(struct thread *, struct ksem_getvalue_args *);
int sys_ksem_destroy(struct thread *, struct ksem_destroy_args *);
int sys___mac_get_pid(struct thread *, struct __mac_get_pid_args *);
int sys___mac_get_link(struct thread *, struct __mac_get_link_args *);
int sys___mac_set_link(struct thread *, struct __mac_set_link_args *);
int sys_extattr_set_link(struct thread *, struct extattr_set_link_args *);
int sys_extattr_get_link(struct thread *, struct extattr_get_link_args *);
int sys_extattr_delete_link(struct thread *, struct extattr_delete_link_args *);
int sys___mac_execve(struct thread *, struct __mac_execve_args *);
int sys_sigaction(struct thread *, struct sigaction_args *);
int sys_sigreturn(struct thread *, struct sigreturn_args *);
int sys_getcontext(struct thread *, struct getcontext_args *);
int sys_setcontext(struct thread *, struct setcontext_args *);
int sys_swapcontext(struct thread *, struct swapcontext_args *);
int sys_swapoff(struct thread *, struct swapoff_args *);
int sys___acl_get_link(struct thread *, struct __acl_get_link_args *);
int sys___acl_set_link(struct thread *, struct __acl_set_link_args *);
int sys___acl_delete_link(struct thread *, struct __acl_delete_link_args *);
int sys___acl_aclcheck_link(struct thread *, struct __acl_aclcheck_link_args *);
int sys_sigwait(struct thread *, struct sigwait_args *);
int sys_thr_create(struct thread *, struct thr_create_args *);
int sys_thr_exit(struct thread *, struct thr_exit_args *);
int sys_thr_self(struct thread *, struct thr_self_args *);
int sys_thr_kill(struct thread *, struct thr_kill_args *);
int sys__umtx_lock(struct thread *, struct _umtx_lock_args *);
int sys__umtx_unlock(struct thread *, struct _umtx_unlock_args *);
int sys_jail_attach(struct thread *, struct jail_attach_args *);
int sys_extattr_list_fd(struct thread *, struct extattr_list_fd_args *);
int sys_extattr_list_file(struct thread *, struct extattr_list_file_args *);
int sys_extattr_list_link(struct thread *, struct extattr_list_link_args *);
int sys_ksem_timedwait(struct thread *, struct ksem_timedwait_args *);
int sys_thr_suspend(struct thread *, struct thr_suspend_args *);
int sys_thr_wake(struct thread *, struct thr_wake_args *);
int sys_kldunloadf(struct thread *, struct kldunloadf_args *);
int sys_audit(struct thread *, struct audit_args *);
int sys_auditon(struct thread *, struct auditon_args *);
int sys_getauid(struct thread *, struct getauid_args *);
int sys_setauid(struct thread *, struct setauid_args *);
int sys_getaudit(struct thread *, struct getaudit_args *);
int sys_setaudit(struct thread *, struct setaudit_args *);
int sys_getaudit_addr(struct thread *, struct getaudit_addr_args *);
int sys_setaudit_addr(struct thread *, struct setaudit_addr_args *);
int sys_auditctl(struct thread *, struct auditctl_args *);
int sys__umtx_op(struct thread *, struct _umtx_op_args *);
int sys_thr_new(struct thread *, struct thr_new_args *);
int sys_sigqueue(struct thread *, struct sigqueue_args *);
int sys_kmq_open(struct thread *, struct kmq_open_args *);
int sys_kmq_setattr(struct thread *, struct kmq_setattr_args *);
int sys_kmq_timedreceive(struct thread *, struct kmq_timedreceive_args *);
int sys_kmq_timedsend(struct thread *, struct kmq_timedsend_args *);
int sys_kmq_notify(struct thread *, struct kmq_notify_args *);
int sys_kmq_unlink(struct thread *, struct kmq_unlink_args *);
int sys_abort2(struct thread *, struct abort2_args *);
int sys_thr_set_name(struct thread *, struct thr_set_name_args *);
int sys_aio_fsync(struct thread *, struct aio_fsync_args *);
int sys_rtprio_thread(struct thread *, struct rtprio_thread_args *);
int sys_sctp_peeloff(struct thread *, struct sctp_peeloff_args *);
int sys_sctp_generic_sendmsg(struct thread *, struct sctp_generic_sendmsg_args *);
int sys_sctp_generic_sendmsg_iov(struct thread *, struct sctp_generic_sendmsg_iov_args *);
int sys_sctp_generic_recvmsg(struct thread *, struct sctp_generic_recvmsg_args *);
int sys_pread(struct thread *, struct pread_args *);
int sys_pwrite(struct thread *, struct pwrite_args *);
int sys_mmap(struct thread *, struct mmap_args *);
int sys_lseek(struct thread *, struct lseek_args *);
int sys_truncate(struct thread *, struct truncate_args *);
int sys_ftruncate(struct thread *, struct ftruncate_args *);
int sys_thr_kill2(struct thread *, struct thr_kill2_args *);
int sys_shm_open(struct thread *, struct shm_open_args *);
int sys_shm_unlink(struct thread *, struct shm_unlink_args *);
int sys_cpuset(struct thread *, struct cpuset_args *);
int sys_cpuset_setid(struct thread *, struct cpuset_setid_args *);
int sys_cpuset_getid(struct thread *, struct cpuset_getid_args *);
int sys_cpuset_getaffinity(struct thread *, struct cpuset_getaffinity_args *);
int sys_cpuset_setaffinity(struct thread *, struct cpuset_setaffinity_args *);
int sys_faccessat(struct thread *, struct faccessat_args *);
int sys_fchmodat(struct thread *, struct fchmodat_args *);
int sys_fchownat(struct thread *, struct fchownat_args *);
int sys_fexecve(struct thread *, struct fexecve_args *);
int sys_fstatat(struct thread *, struct fstatat_args *);
int sys_futimesat(struct thread *, struct futimesat_args *);
int sys_linkat(struct thread *, struct linkat_args *);
int sys_mkdirat(struct thread *, struct mkdirat_args *);
int sys_mkfifoat(struct thread *, struct mkfifoat_args *);
int sys_mknodat(struct thread *, struct mknodat_args *);
int sys_openat(struct thread *, struct openat_args *);
int sys_readlinkat(struct thread *, struct readlinkat_args *);
int sys_renameat(struct thread *, struct renameat_args *);
int sys_symlinkat(struct thread *, struct symlinkat_args *);
int sys_unlinkat(struct thread *, struct unlinkat_args *);
int sys_posix_openpt(struct thread *, struct posix_openpt_args *);
int sys_gssd_syscall(struct thread *, struct gssd_syscall_args *);
int sys_jail_get(struct thread *, struct jail_get_args *);
int sys_jail_set(struct thread *, struct jail_set_args *);
int sys_jail_remove(struct thread *, struct jail_remove_args *);
int sys_closefrom(struct thread *, struct closefrom_args *);
int sys___semctl(struct thread *, struct __semctl_args *);
int sys_msgctl(struct thread *, struct msgctl_args *);
int sys_shmctl(struct thread *, struct shmctl_args *);
int sys_lpathconf(struct thread *, struct lpathconf_args *);
int sys___cap_rights_get(struct thread *, struct __cap_rights_get_args *);
int sys_cap_enter(struct thread *, struct cap_enter_args *);
int sys_cap_getmode(struct thread *, struct cap_getmode_args *);
int sys_pdfork(struct thread *, struct pdfork_args *);
int sys_pdkill(struct thread *, struct pdkill_args *);
int sys_pdgetpid(struct thread *, struct pdgetpid_args *);
int sys_pselect(struct thread *, struct pselect_args *);
int sys_getloginclass(struct thread *, struct getloginclass_args *);
int sys_setloginclass(struct thread *, struct setloginclass_args *);
int sys_rctl_get_racct(struct thread *, struct rctl_get_racct_args *);
int sys_rctl_get_rules(struct thread *, struct rctl_get_rules_args *);
int sys_rctl_get_limits(struct thread *, struct rctl_get_limits_args *);
int sys_rctl_add_rule(struct thread *, struct rctl_add_rule_args *);
int sys_rctl_remove_rule(struct thread *, struct rctl_remove_rule_args *);
int sys_posix_fallocate(struct thread *, struct posix_fallocate_args *);
int sys_posix_fadvise(struct thread *, struct posix_fadvise_args *);
int sys_wait6(struct thread *, struct wait6_args *);
int sys_cap_rights_limit(struct thread *, struct cap_rights_limit_args *);
int sys_cap_ioctls_limit(struct thread *, struct cap_ioctls_limit_args *);
int sys_cap_ioctls_get(struct thread *, struct cap_ioctls_get_args *);
int sys_cap_fcntls_limit(struct thread *, struct cap_fcntls_limit_args *);
int sys_cap_fcntls_get(struct thread *, struct cap_fcntls_get_args *);
int sys_bindat(struct thread *, struct bindat_args *);
int sys_connectat(struct thread *, struct connectat_args *);
int sys_chflagsat(struct thread *, struct chflagsat_args *);
int sys_accept4(struct thread *, struct accept4_args *);
int sys_pipe2(struct thread *, struct pipe2_args *);
int sys_aio_mlock(struct thread *, struct aio_mlock_args *);
int sys_procctl(struct thread *, struct procctl_args *);
# 53 "syscalls.master" 2

; Reserved/unimplemented system calls in the range 0-150 inclusive
; are reserved for use in future Berkeley releases.
; Additional system calls implemented in vendor and other
; redistributions should be placed in the reserved range at the end
; of the current calls.

0 0 STD { int nosys(void); } syscall nosys_args int
1 1 STD { void sys_exit(int rval); } exit sys_exit_args void

2 2 STD { int fork(void); }
3 0 STD { ssize_t read(int fd, void *buf, size_t nbyte); }

4 0 STD { ssize_t write(int fd, const void *buf, size_t nbyte); }

5 83 STD { int open(char *path, int flags, int mode); }
; XXX should be { int open(const char *path, int flags, ...); }
; but we're not ready for `const' or varargs.
; XXX man page says `mode_t mode'.
6 112 STD { int close(int fd); }
7 43060 STD { int wait4(int pid, int *status, int options, struct rusage *rusage); }

8 4 COMPAT { int creat(char *path, int mode); }
9 5 STD { int link(char *path, char *link); }
10 6 STD { int unlink(char *path); }
11 0 OBSOL execv
12 8 STD { int chdir(char *path); }
13 68 STD { int fchdir(int fd); }
14 9 STD { int mknod(char *path, int mode, int dev); }
15 10 STD { int chmod(char *path, int mode); }
16 11 STD { int chown(char *path, int uid, int gid); }
17 0 STD { int obreak(char *nsize); } break obreak_args int

18 43001 COMPAT4 { int getfsstat(struct ostatfs *buf, long bufsize, int flags); }

19 194 COMPAT { long lseek(int fd, long offset, int whence); }

20 0 STD { pid_t getpid(void); }
21 62 STD { int mount(char *type, char *path, int flags, caddr_t data); }

; XXX `path' should have type `const char *' but we're not ready for that.
22 12 STD { int unmount(char *path, int flags); }
23 200 STD { int setuid(uid_t uid); }
24 0 STD { uid_t getuid(void); }
25 0 STD { uid_t geteuid(void); }
26 43002 STD { int ptrace(int req, pid_t pid, caddr_t addr, int data); }

27 190 STD { int recvmsg(int s, struct msghdr *msg, int flags); }

28 188 STD { int sendmsg(int s, struct msghdr *msg, int flags); }

29 191 STD { int recvfrom(int s, caddr_t buf, size_t len, int flags, struct sockaddr * restrict from, __socklen_t * restrict fromlenaddr); }



30 33 STD { int accept(int s, struct sockaddr * restrict name, __socklen_t * restrict anamelen); }


31 0 STD { int getpeername(int fdes, struct sockaddr * restrict asa, __socklen_t * restrict alen); }


32 0 STD { int getsockname(int fdes, struct sockaddr * restrict asa, __socklen_t * restrict alen); }


33 14 STD { int access(char *path, int amode); }
34 43003 STD { int chflags(const char *path, u_long flags); }
35 43004 STD { int fchflags(int fd, u_long flags); }
36 0 STD { int sync(void); }
37 15 STD { int kill(int pid, int signum); }
38 16 COMPAT { int stat(char *path, struct ostat *ub); }
39 0 STD { pid_t getppid(void); }
40 17 COMPAT { int lstat(char *path, struct ostat *ub); }
41 0 STD { int dup(u_int fd); }
42 185 STD { int pipe(void); }
43 0 STD { gid_t getegid(void); }
44 43005 STD { int profil(caddr_t samples, size_t size, size_t offset, u_int scale); }

45 43006 STD { int ktrace(const char *fname, int ops, int facs, int pid); }

46 0 COMPAT { int sigaction(int signum, struct osigaction *nsa, struct osigaction *osa); }


47 0 STD { gid_t getgid(void); }
48 0 COMPAT { int sigprocmask(int how, osigset_t mask); }
; XXX note nonstandard (bogus) calling convention - the libc stub passes
; us the mask, not a pointer to it, and we return the old mask as the
; (int) return value.
49 0 STD { int getlogin(char *namebuf, u_int namelen); }

50 43007 STD { int setlogin(char *namebuf); }
51 18 STD { int acct(char *path); }
52 0 COMPAT { int sigpending(void); }
53 0 STD { int sigaltstack(stack_t *ss, stack_t *oss); }

54 158 STD { int ioctl(int fd, u_long com, caddr_t data); }

55 20 STD { int reboot(int opt); }
56 151 STD { int revoke(char *path); }
57 21 STD { int symlink(char *path, char *link); }
58 22 STD { ssize_t readlink(char *path, char *buf, size_t count); }

59 23 STD { int execve(char *fname, char **argv, char **envv); }

60 43009 STD { int umask(int newmask); } umask umask_args int

61 24 STD { int chroot(char *path); }
62 208 COMPAT { int fstat(int fd, struct ostat *sb); }
63 0 COMPAT { int getkerninfo(int op, char *where, size_t *size, int arg); } getkerninfo getkerninfo_args int


64 0 COMPAT { int getpagesize(void); } getpagesize getpagesize_args int

65 0 STD { int msync(void *addr, size_t len, int flags); }

66 25 STD { int vfork(void); }
67 0 OBSOL vread
68 0 OBSOL vwrite
69 0 STD { int sbrk(int incr); }
70 0 STD { int sstk(int incr); }
71 210 COMPAT { int mmap(void *addr, int len, int prot, int flags, int fd, long pos); }

72 0 STD { int ovadvise(int anom); } vadvise ovadvise_args int

73 213 STD { int munmap(void *addr, size_t len); }
74 43010 STD { int mprotect(const void *addr, size_t len, int prot); }

75 0 STD { int madvise(void *addr, size_t len, int behav); }

76 0 OBSOL vhangup
77 0 OBSOL vlimit
78 0 STD { int mincore(const void *addr, size_t len, char *vec); }

79 0 STD { int getgroups(u_int gidsetsize, gid_t *gidset); }

80 26 STD { int setgroups(u_int gidsetsize, gid_t *gidset); }

81 0 STD { int getpgrp(void); }
82 27 STD { int setpgid(int pid, int pgid); }
83 0 STD { int setitimer(u_int which, struct itimerval *itv, struct itimerval *oitv); }

84 43060 COMPAT { int wait(void); }
85 28 STD { int swapon(char *name); }
86 0 STD { int getitimer(u_int which, struct itimerval *itv); }

87 43021 COMPAT { int gethostname(char *hostname, u_int len); } gethostname gethostname_args int


88 43021 COMPAT { int sethostname(char *hostname, u_int len); } sethostname sethostname_args int


89 0 STD { int getdtablesize(void); }
90 209 STD { int dup2(u_int from, u_int to); }
91 0 UNIMPL getdopt
92 30 STD { int fcntl(int fd, int cmd, long arg); }
; XXX should be { int fcntl(int fd, int cmd, ...); }
; but we're not ready for varargs.
93 0 STD { int select(int nd, fd_set *in, fd_set *ou, fd_set *ex, struct timeval *tv); }

94 0 UNIMPL setdopt
95 43069 STD { int fsync(int fd); }
96 31 STD { int setpriority(int which, int who, int prio); }

97 183 STD { int socket(int domain, int type, int protocol); }

98 32 STD { int connect(int s, caddr_t name, int namelen); }

99 33 COMPAT|NOARGS { int accept(int s, caddr_t name, int *anamelen); } accept accept_args int

100 0 STD { int getpriority(int which, int who); }
101 187 COMPAT { int send(int s, caddr_t buf, int len, int flags); }

102 189 COMPAT { int recv(int s, caddr_t buf, int len, int flags); }

103 0 COMPAT { int sigreturn( struct osigcontext *sigcntxp); }

104 34 STD { int bind(int s, caddr_t name, int namelen); }

105 35 STD { int setsockopt(int s, int level, int name, caddr_t val, int valsize); }

106 43140 STD { int listen(int s, int backlog); }
107 0 OBSOL vtimes
108 0 COMPAT { int sigvec(int signum, struct sigvec *nsv, struct sigvec *osv); }

109 0 COMPAT { int sigblock(int mask); }
110 0 COMPAT { int sigsetmask(int mask); }
111 0 COMPAT { int sigsuspend(osigset_t mask); }
; XXX note nonstandard (bogus) calling convention - the libc stub passes
; us the mask, not a pointer to it.
112 0 COMPAT { int sigstack(struct sigstack *nss, struct sigstack *oss); }

113 190 COMPAT { int recvmsg(int s, struct omsghdr *msg, int flags); }

114 188 COMPAT { int sendmsg(int s, caddr_t msg, int flags); }

115 0 OBSOL vtrace
116 0 STD { int gettimeofday(struct timeval *tp, struct timezone *tzp); }

117 0 STD { int getrusage(int who, struct rusage *rusage); }

118 0 STD { int getsockopt(int s, int level, int name, caddr_t val, int *avalsize); }

119 0 UNIMPL resuba (199506/OS 2.x)
120 198 STD { int readv(int fd, struct iovec *iovp, u_int iovcnt); }

121 196 STD { int writev(int fd, struct iovec *iovp, u_int iovcnt); }

122 37 STD { int settimeofday(struct timeval *tv, struct timezone *tzp); }

123 38 STD { int fchown(int fd, int uid, int gid); }
124 39 STD { int fchmod(int fd, int mode); }
125 191 COMPAT|NOARGS { int recvfrom(int s, caddr_t buf, size_t len, int flags, caddr_t from, int *fromlenaddr); } recvfrom recvfrom_args int



126 40 STD { int setreuid(int ruid, int euid); }
127 41 STD { int setregid(int rgid, int egid); }
128 42 STD { int rename(char *from, char *to); }
129 43 COMPAT { int truncate(char *path, long length); }
130 44 COMPAT { int ftruncate(int fd, long length); }
131 45 STD { int flock(int fd, int how); }
132 43011 STD { int mkfifo(char *path, int mode); }
133 184 STD { int sendto(int s, caddr_t buf, size_t len, int flags, caddr_t to, int tolen); }

134 46 STD { int shutdown(int s, int how); }
135 186 STD { int socketpair(int domain, int type, int protocol, int *rsv); }

136 47 STD { int mkdir(char *path, int mode); }
137 48 STD { int rmdir(char *path); }
138 49 STD { int utimes(char *path, struct timeval *tptr); }

139 0 OBSOL 4.2 sigreturn
140 50 STD { int adjtime(struct timeval *delta, struct timeval *olddelta); }

141 0 COMPAT { int getpeername(int fdes, caddr_t asa, int *alen); }

142 43021 COMPAT { long gethostid(void); }
143 43021 COMPAT { int sethostid(long hostid); }
144 0 COMPAT { int getrlimit(u_int which, struct orlimit *rlp); }

145 51 COMPAT { int setrlimit(u_int which, struct orlimit *rlp); }

146 52 COMPAT { int killpg(int pgid, int signum); }
147 43014 STD { int setsid(void); }
148 60 STD { int quotactl(char *path, int cmd, int uid, caddr_t arg); }

149 0 COMPAT { int quota(void); }
150 0 COMPAT|NOARGS { int getsockname(int fdec, caddr_t asa, int *alen); } getsockname getsockname_args int



; Syscalls 151-180 inclusive are reserved for vendor-specific
; system calls. (This includes various calls added for compatibity
; with other Unix variants.)
; Some of these calls are now supported by 199506 ...
151 0 UNIMPL sem_lock (199506/OS 2.x)
152 0 UNIMPL sem_wakeup (199506/OS 2.x)
153 0 UNIMPL asyncdaemon (199506/OS 2.x)
; 154 is initialised by the NLM code, if present.
154 0 NOSTD { int nlm_syscall(int debug_level, int grace_period, int addr_count, char **addrs); }
; 155 is initialized by the NFS code, if present.
155 53 NOSTD { int nfssvc(int flag, caddr_t argp); }
156 43020 COMPAT { int getdirentries(int fd, char *buf, u_int count, long *basep); }

157 54 COMPAT4 { int statfs(char *path, struct ostatfs *buf); }

158 55 COMPAT4 { int fstatfs(int fd, struct ostatfs *buf); }

159 0 UNIMPL nosys
160 43061 STD { int lgetfh(char *fname, struct fhandle *fhp); }

161 58 STD { int getfh(char *fname, struct fhandle *fhp); }

162 43021 COMPAT4 { int getdomainname(char *domainname, int len); }

163 43021 COMPAT4 { int setdomainname(char *domainname, int len); }

164 0 COMPAT4 { int uname(struct utsname *name); }
165 43100 STD { int sysarch(int op, char *parms); }
166 43082 STD { int rtprio(int function, pid_t pid, struct rtprio *rtp); }

167 0 UNIMPL nosys
168 0 UNIMPL nosys
169 63 NOSTD { int semsys(int which, int a2, int a3, int a4, int a5); }

; XXX should be { int semsys(int which, ...); }
170 64 NOSTD { int msgsys(int which, int a2, int a3, int a4, int a5, int a6); }

; XXX should be { int msgsys(int which, ...); }
171 65 NOSTD { int shmsys(int which, int a2, int a3, int a4); }

; XXX should be { int shmsys(int which, ...); }
172 0 UNIMPL nosys
173 43192 STD { ssize_t freebsd6_pread(int fd, void *buf, size_t nbyte, int pad, off_t offset); }

174 43193 STD { ssize_t freebsd6_pwrite(int fd, const void *buf, size_t nbyte, int pad, off_t offset); }


175 0 STD { int setfib(int fibnum); }
176 288 STD { int ntp_adjtime(struct timex *tp); }
177 0 UNIMPL sfork (199506/OS 2.x)
178 0 UNIMPL getdescriptor (199506/OS 2.x)
179 0 UNIMPL setdescriptor (199506/OS 2.x)
180 0 UNIMPL nosys

; Syscalls 181-199 are used by/reserved for 199506
181 205 STD { int setgid(gid_t gid); }
182 214 STD { int setegid(gid_t egid); }
183 215 STD { int seteuid(uid_t euid); }
184 0 UNIMPL lfs_bmapv
185 0 UNIMPL lfs_markv
186 0 UNIMPL lfs_segclean
187 0 UNIMPL lfs_segwait
188 16 STD { int stat(char *path, struct stat *ub); }
189 208 STD { int fstat(int fd, struct stat *sb); }
190 17 STD { int lstat(char *path, struct stat *ub); }
191 71 STD { int pathconf(char *path, int name); }
192 43019 STD { int fpathconf(int fd, int name); }
193 0 UNIMPL nosys
194 0 STD { int getrlimit(u_int which, struct rlimit *rlp); } getrlimit __getrlimit_args int


195 51 STD { int setrlimit(u_int which, struct rlimit *rlp); } setrlimit __setrlimit_args int


196 43020 STD { int getdirentries(int fd, char *buf, u_int count, long *basep); }

197 210 STD { caddr_t freebsd6_mmap(caddr_t addr, size_t len, int prot, int flags, int fd, int pad, off_t pos); }


198 0 NOPROTO { int nosys(void); } __syscall __syscall_args int

199 194 STD { off_t freebsd6_lseek(int fd, int pad, off_t offset, int whence); }

200 43 STD { int freebsd6_truncate(char *path, int pad, off_t length); }

201 44 STD { int freebsd6_ftruncate(int fd, int pad, off_t length); }

202 43021 STD { int __sysctl(int *name, u_int namelen, void *old, size_t *oldlenp, void *new, size_t newlen); } __sysctl sysctl_args int


203 43022 STD { int mlock(const void *addr, size_t len); }
204 43023 STD { int munlock(const void *addr, size_t len); }
205 43024 STD { int undelete(char *path); }
206 43013 STD { int futimes(int fd, struct timeval *tptr); }
207 0 STD { int getpgid(pid_t pid); }
208 0 UNIMPL newreboot (NetBSD)
209 43012 STD { int poll(struct pollfd *fds, u_int nfds, int timeout); }


;
; The following are reserved for loadable syscalls
;
210 0 NODEF|NOTSTATIC lkmnosys lkmnosys nosys_args int
211 0 NODEF|NOTSTATIC lkmnosys lkmnosys nosys_args int
212 0 NODEF|NOTSTATIC lkmnosys lkmnosys nosys_args int
213 0 NODEF|NOTSTATIC lkmnosys lkmnosys nosys_args int
214 0 NODEF|NOTSTATIC lkmnosys lkmnosys nosys_args int
215 0 NODEF|NOTSTATIC lkmnosys lkmnosys nosys_args int
216 0 NODEF|NOTSTATIC lkmnosys lkmnosys nosys_args int
217 0 NODEF|NOTSTATIC lkmnosys lkmnosys nosys_args int
218 0 NODEF|NOTSTATIC lkmnosys lkmnosys nosys_args int
219 0 NODEF|NOTSTATIC lkmnosys lkmnosys nosys_args int

;
; The following were introduced with NetBSD/4.4Lite-2
220 98 COMPAT7|NOSTD { int __semctl(int semid, int semnum, int cmd, union semun_old *arg); }

221 109 NOSTD { int semget(key_t key, int nsems, int semflg); }

222 110 NOSTD { int semop(int semid, struct sembuf *sops, size_t nsops); }

223 0 UNIMPL semconfig
224 84 COMPAT7|NOSTD { int msgctl(int msqid, int cmd, struct msqid_ds_old *buf); }

225 88 NOSTD { int msgget(key_t key, int msgflg); }
226 90 NOSTD { int msgsnd(int msqid, const void *msgp, size_t msgsz, int msgflg); }

227 89 NOSTD { int msgrcv(int msqid, void *msgp, size_t msgsz, long msgtyp, int msgflg); }

228 96 NOSTD { int shmat(int shmid, const void *shmaddr, int shmflg); }

229 91 COMPAT7|NOSTD { int shmctl(int shmid, int cmd, struct shmid_ds_old *buf); }

230 97 NOSTD { int shmdt(const void *shmaddr); }
231 95 NOSTD { int shmget(key_t key, size_t size, int shmflg); }

;
232 0 STD { int clock_gettime(clockid_t clock_id, struct timespec *tp); }

233 287 STD { int clock_settime( clockid_t clock_id, const struct timespec *tp); }


234 0 STD { int clock_getres(clockid_t clock_id, struct timespec *tp); }

235 0 STD { int ktimer_create(clockid_t clock_id, struct sigevent *evp, int *timerid); }

236 0 STD { int ktimer_delete(int timerid); }
237 0 STD { int ktimer_settime(int timerid, int flags, const struct itimerspec *value, struct itimerspec *ovalue); }


238 0 STD { int ktimer_gettime(int timerid, struct itimerspec *value); }

239 0 STD { int ktimer_getoverrun(int timerid); }
240 0 STD { int nanosleep(const struct timespec *rqtp, struct timespec *rmtp); }

241 0 STD { int ffclock_getcounter(ffcounter *ffcount); }
242 0 STD { int ffclock_setestimate( struct ffclock_estimate *cest); }

243 0 STD { int ffclock_getestimate( struct ffclock_estimate *cest); }

244 0 UNIMPL nosys
245 0 UNIMPL nosys
246 0 UNIMPL nosys
247 0 STD { int clock_getcpuclockid2(id_t id, int which, clockid_t *clock_id); }

248 0 STD { int ntp_gettime(struct ntptimeval *ntvp); }
249 0 UNIMPL nosys
; syscall numbers initially used in OpenBSD
250 43030 STD { int minherit(void *addr, size_t len, int inherit); }

251 43043 STD { int rfork(int flags); }
252 43012 STD { int openbsd_poll(struct pollfd *fds, u_int nfds, int timeout); }

253 0 STD { int issetugid(void); }
254 237 STD { int lchown(char *path, int uid, int gid); }
255 0 NOSTD { int aio_read(struct aiocb *aiocbp); }
256 0 NOSTD { int aio_write(struct aiocb *aiocbp); }
257 0 NOSTD { int lio_listio(int mode, struct aiocb * const *acb_list, int nent, struct sigevent *sig); }


258 0 UNIMPL nosys
259 0 UNIMPL nosys
260 0 UNIMPL nosys
261 0 UNIMPL nosys
262 0 UNIMPL nosys
263 0 UNIMPL nosys
264 0 UNIMPL nosys
265 0 UNIMPL nosys
266 0 UNIMPL nosys
267 0 UNIMPL nosys
268 0 UNIMPL nosys
269 0 UNIMPL nosys
270 0 UNIMPL nosys
271 0 UNIMPL nosys
272 0 STD { int getdents(int fd, char *buf, size_t count); }

273 0 UNIMPL nosys
274 43044 STD { int lchmod(char *path, mode_t mode); }
275 237 NOPROTO { int lchown(char *path, uid_t uid, gid_t gid); } netbsd_lchown lchown_args int


276 43052 STD { int lutimes(char *path, struct timeval *tptr); }

277 0 NOPROTO { int msync(void *addr, size_t len, int flags); } netbsd_msync msync_args int

278 16 STD { int nstat(char *path, struct nstat *ub); }
279 208 STD { int nfstat(int fd, struct nstat *sb); }
280 17 STD { int nlstat(char *path, struct nstat *ub); }
281 0 UNIMPL nosys
282 0 UNIMPL nosys
283 0 UNIMPL nosys
284 0 UNIMPL nosys
285 0 UNIMPL nosys
286 0 UNIMPL nosys
287 0 UNIMPL nosys
288 0 UNIMPL nosys
; 289 and 290 from NetBSD (OpenBSD: 267 and 268)
289 0 STD { ssize_t preadv(int fd, struct iovec *iovp, u_int iovcnt, off_t offset); }

290 0 STD { ssize_t pwritev(int fd, struct iovec *iovp, u_int iovcnt, off_t offset); }

291 0 UNIMPL nosys
292 0 UNIMPL nosys
293 0 UNIMPL nosys
294 0 UNIMPL nosys
295 0 UNIMPL nosys
296 0 UNIMPL nosys
; XXX 297 is 300 in NetBSD
297 43062 COMPAT4 { int fhstatfs( const struct fhandle *u_fhp, struct ostatfs *buf); }


298 43063 STD { int fhopen(const struct fhandle *u_fhp, int flags); }

299 43064 STD { int fhstat(const struct fhandle *u_fhp, struct stat *sb); }

; syscall numbers for FreeBSD
300 0 STD { int modnext(int modid); }
301 0 STD { int modstat(int modid, struct module_stat *stat); }

302 0 STD { int modfnext(int modid); }
303 0 STD { int modfind(const char *name); }
304 243 STD { int kldload(const char *file); }
305 244 STD { int kldunload(int fileid); }
306 0 STD { int kldfind(const char *file); }
307 0 STD { int kldnext(int fileid); }
308 0 STD { int kldstat(int fileid, struct kld_file_stat* stat); }

309 0 STD { int kldfirstmod(int fileid); }
310 0 STD { int getsid(pid_t pid); }
311 43057 STD { int setresuid(uid_t ruid, uid_t euid, uid_t suid); }

312 43059 STD { int setresgid(gid_t rgid, gid_t egid, gid_t sgid); }

313 0 OBSOL signanosleep
314 0 NOSTD { int aio_return(struct aiocb *aiocbp); }
315 0 NOSTD { int aio_suspend( struct aiocb * const * aiocbp, int nent, const struct timespec *timeout); }


316 0 NOSTD { int aio_cancel(int fd, struct aiocb *aiocbp); }

317 0 NOSTD { int aio_error(struct aiocb *aiocbp); }
318 0 NOSTD { int oaio_read(struct oaiocb *aiocbp); }
319 0 NOSTD { int oaio_write(struct oaiocb *aiocbp); }
320 0 NOSTD { int olio_listio(int mode, struct oaiocb * const *acb_list, int nent, struct osigevent *sig); }


321 0 STD { int yield(void); }
322 0 OBSOL thr_sleep
323 0 OBSOL thr_wakeup
324 43141 STD { int mlockall(int how); }
325 43142 STD { int munlockall(void); }
326 43078 STD { int __getcwd(u_char *buf, u_int buflen); }

327 0 STD { int sched_setparam (pid_t pid, const struct sched_param *param); }

328 0 STD { int sched_getparam (pid_t pid, struct sched_param *param); }


329 0 STD { int sched_setscheduler (pid_t pid, int policy, const struct sched_param *param); }


330 0 STD { int sched_getscheduler (pid_t pid); }

331 0 STD { int sched_yield (void); }
332 0 STD { int sched_get_priority_max (int policy); }
333 0 STD { int sched_get_priority_min (int policy); }
334 0 STD { int sched_rr_get_interval (pid_t pid, struct timespec *interval); }

335 0 STD { int utrace(const void *addr, size_t len); }
336 43054 COMPAT4 { int sendfile(int fd, int s, off_t offset, size_t nbytes, struct sf_hdtr *hdtr, off_t *sbytes, int flags); }



337 0 STD { int kldsym(int fileid, int cmd, void *data); }

338 43065 STD { int jail(struct jail *jail); }
339 0 NOSTD|NOTSTATIC { int nnpfs_syscall(int operation, char *a_pathP, int a_opcode, void *a_paramsP, int a_followSymlinks); }


340 0 STD { int sigprocmask(int how, const sigset_t *set, sigset_t *oset); }

341 0 STD { int sigsuspend(const sigset_t *sigmask); }
342 0 COMPAT4 { int sigaction(int sig, const struct sigaction *act, struct sigaction *oact); }


343 0 STD { int sigpending(sigset_t *set); }
344 0 COMPAT4 { int sigreturn( const struct ucontext4 *sigcntxp); }

345 0 STD { int sigtimedwait(const sigset_t *set, siginfo_t *info, const struct timespec *timeout); }


346 0 STD { int sigwaitinfo(const sigset_t *set, siginfo_t *info); }

347 0 STD { int __acl_get_file(const char *path, acl_type_t type, struct acl *aclp); }

348 0 STD { int __acl_set_file(const char *path, acl_type_t type, struct acl *aclp); }

349 0 STD { int __acl_get_fd(int filedes, acl_type_t type, struct acl *aclp); }

350 0 STD { int __acl_set_fd(int filedes, acl_type_t type, struct acl *aclp); }

351 0 STD { int __acl_delete_file(const char *path, acl_type_t type); }

352 0 STD { int __acl_delete_fd(int filedes, acl_type_t type); }

353 0 STD { int __acl_aclcheck_file(const char *path, acl_type_t type, struct acl *aclp); }

354 0 STD { int __acl_aclcheck_fd(int filedes, acl_type_t type, struct acl *aclp); }

355 43101 STD { int extattrctl(const char *path, int cmd, const char *filename, int attrnamespace, const char *attrname); }


356 43103 STD { ssize_t extattr_set_file( const char *path, int attrnamespace, const char *attrname, void *data, size_t nbytes); }



357 43102 STD { ssize_t extattr_get_file( const char *path, int attrnamespace, const char *attrname, void *data, size_t nbytes); }



358 43105 STD { int extattr_delete_file(const char *path, int attrnamespace, const char *attrname); }


359 0 NOSTD { int aio_waitcomplete( struct aiocb **aiocbp, struct timespec *timeout); }


360 43056 STD { int getresuid(uid_t *ruid, uid_t *euid, uid_t *suid); }

361 43058 STD { int getresgid(gid_t *rgid, gid_t *egid, gid_t *sgid); }

362 43067 STD { int kqueue(void); }
363 0 STD { int kevent(int fd, struct kevent *changelist, int nchanges, struct kevent *eventlist, int nevents, const struct timespec *timeout); }



364 0 UNIMPL __cap_get_proc
365 0 UNIMPL __cap_set_proc
366 0 UNIMPL __cap_get_fd
367 0 UNIMPL __cap_get_file
368 0 UNIMPL __cap_set_fd
369 0 UNIMPL __cap_set_file
370 0 UNIMPL nosys
371 43107 STD { ssize_t extattr_set_fd(int fd, int attrnamespace, const char *attrname, void *data, size_t nbytes); }


372 43106 STD { ssize_t extattr_get_fd(int fd, int attrnamespace, const char *attrname, void *data, size_t nbytes); }


373 43109 STD { int extattr_delete_fd(int fd, int attrnamespace, const char *attrname); }


374 0 STD { int __setugid(int flag); }
375 0 UNIMPL nfsclnt
376 43066 STD { int eaccess(char *path, int amode); }
377 0 NOSTD|NOTSTATIC { int afs3_syscall(long syscall, long parm1, long parm2, long parm3, long parm4, long parm5, long parm6); }


378 43070 STD { int nmount(struct iovec *iovp, unsigned int iovcnt, int flags); }

379 0 UNIMPL kse_exit
380 0 UNIMPL kse_wakeup
381 0 UNIMPL kse_create
382 0 UNIMPL kse_thr_interrupt
383 0 UNIMPL kse_release
384 0 STD { int __mac_get_proc(struct mac *mac_p); }
385 0 STD { int __mac_set_proc(struct mac *mac_p); }
386 0 STD { int __mac_get_fd(int fd, struct mac *mac_p); }

387 0 STD { int __mac_get_file(const char *path_p, struct mac *mac_p); }

388 0 STD { int __mac_set_fd(int fd, struct mac *mac_p); }

389 0 STD { int __mac_set_file(const char *path_p, struct mac *mac_p); }

390 0 STD { int kenv(int what, const char *name, char *value, int len); }

391 43053 STD { int lchflags(const char *path, u_long flags); }

392 0 STD { int uuidgen(struct uuid *store, int count); }

393 43054 STD { int sendfile(int fd, int s, off_t offset, size_t nbytes, struct sf_hdtr *hdtr, off_t *sbytes, int flags); }


394 0 STD { int mac_syscall(const char *policy, int call, void *arg); }

395 43001 STD { int getfsstat(struct statfs *buf, long bufsize, int flags); }

396 54 STD { int statfs(char *path, struct statfs *buf); }

397 55 STD { int fstatfs(int fd, struct statfs *buf); }
398 43062 STD { int fhstatfs(const struct fhandle *u_fhp, struct statfs *buf); }

399 0 UNIMPL nosys
400 0 NOSTD { int ksem_close(semid_t id); }
401 0 NOSTD { int ksem_post(semid_t id); }
402 0 NOSTD { int ksem_wait(semid_t id); }
403 0 NOSTD { int ksem_trywait(semid_t id); }
404 0 NOSTD { int ksem_init(semid_t *idp, unsigned int value); }

405 0 NOSTD { int ksem_open(semid_t *idp, const char *name, int oflag, mode_t mode, unsigned int value); }


406 0 NOSTD { int ksem_unlink(const char *name); }
407 0 NOSTD { int ksem_getvalue(semid_t id, int *val); }
408 0 NOSTD { int ksem_destroy(semid_t id); }
409 0 STD { int __mac_get_pid(pid_t pid, struct mac *mac_p); }

410 0 STD { int __mac_get_link(const char *path_p, struct mac *mac_p); }

411 0 STD { int __mac_set_link(const char *path_p, struct mac *mac_p); }

412 43111 STD { ssize_t extattr_set_link( const char *path, int attrnamespace, const char *attrname, void *data, size_t nbytes); }



413 43110 STD { ssize_t extattr_get_link( const char *path, int attrnamespace, const char *attrname, void *data, size_t nbytes); }



414 43113 STD { int extattr_delete_link( const char *path, int attrnamespace, const char *attrname); }


415 0 STD { int __mac_execve(char *fname, char **argv, char **envv, struct mac *mac_p); }

416 0 STD { int sigaction(int sig, const struct sigaction *act, struct sigaction *oact); }


417 0 STD { int sigreturn( const struct __ucontext *sigcntxp); }

418 0 UNIMPL __xstat
419 0 UNIMPL __xfstat
420 0 UNIMPL __xlstat
421 0 STD { int getcontext(struct __ucontext *ucp); }
422 0 STD { int setcontext( const struct __ucontext *ucp); }

423 0 STD { int swapcontext(struct __ucontext *oucp, const struct __ucontext *ucp); }

424 43045 STD { int swapoff(const char *name); }
425 0 STD { int __acl_get_link(const char *path, acl_type_t type, struct acl *aclp); }

426 0 STD { int __acl_set_link(const char *path, acl_type_t type, struct acl *aclp); }

427 0 STD { int __acl_delete_link(const char *path, acl_type_t type); }

428 0 STD { int __acl_aclcheck_link(const char *path, acl_type_t type, struct acl *aclp); }

429 0 STD { int sigwait(const sigset_t *set, int *sig); }

430 0 STD { int thr_create(ucontext_t *ctx, long *id, int flags); }

431 0 STD { void thr_exit(long *state); }
432 0 STD { int thr_self(long *id); }
433 0 STD { int thr_kill(long id, int sig); }
434 0 STD { int _umtx_lock(struct umtx *umtx); }
435 0 STD { int _umtx_unlock(struct umtx *umtx); }
436 0 STD { int jail_attach(int jid); }
437 43108 STD { ssize_t extattr_list_fd(int fd, int attrnamespace, void *data, size_t nbytes); }


438 43104 STD { ssize_t extattr_list_file( const char *path, int attrnamespace, void *data, size_t nbytes); }


439 43112 STD { ssize_t extattr_list_link( const char *path, int attrnamespace, void *data, size_t nbytes); }


440 0 UNIMPL kse_switchin
441 0 NOSTD { int ksem_timedwait(semid_t id, const struct timespec *abstime); }

442 0 STD { int thr_suspend( const struct timespec *timeout); }

443 0 STD { int thr_wake(long id); }
444 244 STD { int kldunloadf(int fileid, int flags); }
445 211 STD { int audit(const void *record, u_int length); }

446 138 STD { int auditon(int cmd, void *data, u_int length); }

447 130 STD { int getauid(uid_t *auid); }
448 131 STD { int setauid(uid_t *auid); }
449 132 STD { int getaudit(struct auditinfo *auditinfo); }
450 133 STD { int setaudit(struct auditinfo *auditinfo); }
451 267 STD { int getaudit_addr( struct auditinfo_addr *auditinfo_addr, u_int length); }


452 266 STD { int setaudit_addr( struct auditinfo_addr *auditinfo_addr, u_int length); }


453 43042 STD { int auditctl(char *path); }
454 0 STD { int _umtx_op(void *obj, int op, u_long val, void *uaddr1, void *uaddr2); }

455 0 STD { int thr_new(struct thr_param *param, int param_size); }

456 0 STD { int sigqueue(pid_t pid, int signum, void *value); }
457 0 NOSTD { int kmq_open(const char *path, int flags, mode_t mode, const struct mq_attr *attr); }

458 0 NOSTD { int kmq_setattr(int mqd, const struct mq_attr *attr, struct mq_attr *oattr); }


459 0 NOSTD { int kmq_timedreceive(int mqd, char *msg_ptr, size_t msg_len, unsigned *msg_prio, const struct timespec *abs_timeout); }



460 0 NOSTD { int kmq_timedsend(int mqd, const char *msg_ptr, size_t msg_len, unsigned msg_prio, const struct timespec *abs_timeout);}



461 0 NOSTD { int kmq_notify(int mqd, const struct sigevent *sigev); }

462 0 NOSTD { int kmq_unlink(const char *path); }
463 0 STD { int abort2(const char *why, int nargs, void **args); }
464 0 STD { int thr_set_name(long id, const char *name); }
465 0 NOSTD { int aio_fsync(int op, struct aiocb *aiocbp); }
466 43082 STD { int rtprio_thread(int function, lwpid_t lwpid, struct rtprio *rtp); }

467 0 UNIMPL nosys
468 0 UNIMPL nosys
469 0 UNIMPL __getpath_fromfd
470 0 UNIMPL __getpath_fromaddr
471 0 STD { int sctp_peeloff(int sd, uint32_t name); }
472 0 STD { int sctp_generic_sendmsg(int sd, caddr_t msg, int mlen, caddr_t to, __socklen_t tolen, struct sctp_sndrcvinfo *sinfo, int flags); }


473 0 STD { int sctp_generic_sendmsg_iov(int sd, struct iovec *iov, int iovlen, caddr_t to, __socklen_t tolen, struct sctp_sndrcvinfo *sinfo, int flags); }


474 0 STD { int sctp_generic_recvmsg(int sd, struct iovec *iov, int iovlen, struct sockaddr * from, __socklen_t *fromlenaddr, struct sctp_sndrcvinfo *sinfo, int *msg_flags); }


475 43192 STD { ssize_t pread(int fd, void *buf, size_t nbyte, off_t offset); }

476 43193 STD { ssize_t pwrite(int fd, const void *buf, size_t nbyte, off_t offset); }

477 210 STD { caddr_t mmap(caddr_t addr, size_t len, int prot, int flags, int fd, off_t pos); }

478 194 STD { off_t lseek(int fd, off_t offset, int whence); }

479 43 STD { int truncate(char *path, off_t length); }
480 44 STD { int ftruncate(int fd, off_t length); }
481 15 STD { int thr_kill2(pid_t pid, long id, int sig); }
482 43035 STD { int shm_open(const char *path, int flags, mode_t mode); }

483 43036 STD { int shm_unlink(const char *path); }
484 0 STD { int cpuset(cpusetid_t *setid); }
485 0 STD { int cpuset_setid(cpuwhich_t which, id_t id, cpusetid_t setid); }

486 0 STD { int cpuset_getid(cpulevel_t level, cpuwhich_t which, id_t id, cpusetid_t *setid); }


487 0 STD { int cpuset_getaffinity(cpulevel_t level, cpuwhich_t which, id_t id, size_t cpusetsize, cpuset_t *mask); }


488 0 STD { int cpuset_setaffinity(cpulevel_t level, cpuwhich_t which, id_t id, size_t cpusetsize, const cpuset_t *mask); }


489 43145 STD { int faccessat(int fd, char *path, int amode, int flag); }

490 43146 STD { int fchmodat(int fd, char *path, mode_t mode, int flag); }

491 284 STD { int fchownat(int fd, char *path, uid_t uid, gid_t gid, int flag); }

492 43144 STD { int fexecve(int fd, char **argv, char **envv); }

493 283 STD { int fstatat(int fd, char *path, struct stat *buf, int flag); }

494 285 STD { int futimesat(int fd, char *path, struct timeval *times); }

495 43147 STD { int linkat(int fd1, char *path1, int fd2, char *path2, int flag); }

496 43148 STD { int mkdirat(int fd, char *path, mode_t mode); }
497 43149 STD { int mkfifoat(int fd, char *path, mode_t mode); }
498 43150 STD { int mknodat(int fd, char *path, mode_t mode, dev_t dev); }

; XXX: see the comment for open
499 281 STD { int openat(int fd, char *path, int flag, mode_t mode); }

500 43151 STD { int readlinkat(int fd, char *path, char *buf, size_t bufsize); }

501 282 STD { int renameat(int oldfd, char *old, int newfd, char *new); }

502 43152 STD { int symlinkat(char *path1, int fd, char *path2); }

503 286 STD { int unlinkat(int fd, char *path, int flag); }
504 43185 STD { int posix_openpt(int flags); }
; 505 is initialised by the kgssapi code, if present.
505 0 NOSTD { int gssd_syscall(char *path); }
506 0 STD { int jail_get(struct iovec *iovp, unsigned int iovcnt, int flags); }

507 0 STD { int jail_set(struct iovec *iovp, unsigned int iovcnt, int flags); }

508 0 STD { int jail_remove(int jid); }
509 43143 STD { int closefrom(int lowfd); }
510 98 NOSTD { int __semctl(int semid, int semnum, int cmd, union semun *arg); }

511 84 NOSTD { int msgctl(int msqid, int cmd, struct msqid_ds *buf); }

512 91 NOSTD { int shmctl(int shmid, int cmd, struct shmid_ds *buf); }

513 43196 STD { int lpathconf(char *path, int name); }
514 0 OBSOL cap_new
515 43187 STD { int __cap_rights_get(int version, int fd, cap_rights_t *rightsp); }

516 43188 STD { int cap_enter(void); }
517 43189 STD { int cap_getmode(u_int *modep); }
518 43197 STD { int pdfork(int *fdp, int flags); }
519 43198 STD { int pdkill(int fd, int signum); }
520 43199 STD { int pdgetpid(int fd, pid_t *pidp); }
521 43200 UNIMPL pdwait4
522 0 STD { int pselect(int nd, fd_set *in, fd_set *ou, fd_set *ex, const struct timespec *ts, const sigset_t *sm); }



523 0 STD { int getloginclass(char *namebuf, size_t namelen); }

524 0 STD { int setloginclass(const char *namebuf); }
525 0 STD { int rctl_get_racct(const void *inbufp, size_t inbuflen, void *outbufp, size_t outbuflen); }


526 0 STD { int rctl_get_rules(const void *inbufp, size_t inbuflen, void *outbufp, size_t outbuflen); }


527 0 STD { int rctl_get_limits(const void *inbufp, size_t inbuflen, void *outbufp, size_t outbuflen); }


528 0 STD { int rctl_add_rule(const void *inbufp, size_t inbuflen, void *outbufp, size_t outbuflen); }


529 0 STD { int rctl_remove_rule(const void *inbufp, size_t inbuflen, void *outbufp, size_t outbuflen); }


530 0 STD { int posix_fallocate(int fd, off_t offset, off_t len); }

531 0 STD { int posix_fadvise(int fd, off_t offset, off_t len, int advice); }

532 43201 STD { int wait6(idtype_t idtype, id_t id, int *status, int options, struct __wrusage *wrusage, siginfo_t *info); }



533 43202 STD { int cap_rights_limit(int fd, cap_rights_t *rightsp); }

534 43203 STD { int cap_ioctls_limit(int fd, const u_long *cmds, size_t ncmds); }

535 43204 STD { ssize_t cap_ioctls_get(int fd, u_long *cmds, size_t maxcmds); }

536 43205 STD { int cap_fcntls_limit(int fd, uint32_t fcntlrights); }

537 43206 STD { int cap_fcntls_get(int fd, uint32_t *fcntlrightsp); }

538 43207 STD { int bindat(int fd, int s, caddr_t name, int namelen); }

539 43208 STD { int connectat(int fd, int s, caddr_t name, int namelen); }

540 43209 STD { int chflagsat(int fd, const char *path, u_long flags, int atflag); }

541 33 STD { int accept4(int s, struct sockaddr * restrict name, __socklen_t * restrict anamelen, int flags); }



542 185 STD { int pipe2(int *fildes, int flags); }
543 0 NOSTD { int aio_mlock(struct aiocb *aiocbp); }
544 0 STD { int procctl(idtype_t idtype, id_t id, int com, void *data); }

; Please copy any additions and changes to the following compatability tables:
; sys/compat/freebsd32/syscalls.master
