# 1 "source/arch/x86/include/asm/syscalls.h"
# 1 "/home/sjw/linux-build//"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/kconfig.h" 1



# 1 "include/generated/autoconf.h" 1
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/kconfig.h" 2
# 1 "<command-line>" 2
# 1 "source/arch/x86/include/asm/syscalls.h"
# 13 "source/arch/x86/include/asm/syscalls.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/compiler.h" 1
# 54 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/compiler.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/compiler-gcc.h" 1
# 106 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/compiler-gcc.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/compiler-gcc4.h" 1
# 107 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/compiler-gcc.h" 2
# 55 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/compiler.h" 2
# 79 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/compiler.h"
struct ftrace_branch_data {
 const char *func;
 const char *file;
 unsigned line;
 union {
  struct {
   unsigned long correct;
   unsigned long incorrect;
  };
  struct {
   unsigned long miss;
   unsigned long hit;
  };
  unsigned long miss_hit[2];
 };
};
# 189 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/compiler.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/types.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/types.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/asm-generic/types.h" 1





# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/int-ll64.h" 1
# 10 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/int-ll64.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/asm-generic/int-ll64.h" 1
# 11 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/asm-generic/int-ll64.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/bitsperlong.h" 1
# 10 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/bitsperlong.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/bitsperlong.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/asm-generic/bitsperlong.h" 1
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/bitsperlong.h" 2
# 11 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/bitsperlong.h" 2
# 12 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/asm-generic/int-ll64.h" 2







typedef __signed__ char __s8;
typedef unsigned char __u8;

typedef __signed__ short __s16;
typedef unsigned short __u16;

typedef __signed__ int __s32;
typedef unsigned int __u32;


__extension__ typedef __signed__ long long __s64;
__extension__ typedef unsigned long long __u64;
# 11 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/int-ll64.h" 2




typedef signed char s8;
typedef unsigned char u8;

typedef signed short s16;
typedef unsigned short u16;

typedef signed int s32;
typedef unsigned int u32;

typedef signed long long s64;
typedef unsigned long long u64;
# 7 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/asm-generic/types.h" 2
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/types.h" 2
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/types.h" 2
# 13 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/types.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/posix_types.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/stddef.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/stddef.h" 1
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/compiler.h" 1
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/stddef.h" 2
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/stddef.h" 2





enum {
 false = 0,
 true = 1
};
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/posix_types.h" 2
# 24 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/posix_types.h"
typedef struct {
 unsigned long fds_bits[1024 / (8 * sizeof(long))];
} __kernel_fd_set;


typedef void (*__kernel_sighandler_t)(int);


typedef int __kernel_key_t;
typedef int __kernel_mqd_t;

# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/posix_types.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/posix_types_64.h" 1
# 10 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/posix_types_64.h"
typedef unsigned short __kernel_old_uid_t;
typedef unsigned short __kernel_old_gid_t;


typedef unsigned long __kernel_old_dev_t;


# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/asm-generic/posix_types.h" 1
# 14 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/asm-generic/posix_types.h"
typedef long __kernel_long_t;
typedef unsigned long __kernel_ulong_t;



typedef __kernel_ulong_t __kernel_ino_t;



typedef unsigned int __kernel_mode_t;



typedef int __kernel_pid_t;



typedef int __kernel_ipc_pid_t;



typedef unsigned int __kernel_uid_t;
typedef unsigned int __kernel_gid_t;



typedef __kernel_long_t __kernel_suseconds_t;



typedef int __kernel_daddr_t;



typedef unsigned int __kernel_uid32_t;
typedef unsigned int __kernel_gid32_t;
# 71 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/asm-generic/posix_types.h"
typedef __kernel_ulong_t __kernel_size_t;
typedef __kernel_long_t __kernel_ssize_t;
typedef __kernel_long_t __kernel_ptrdiff_t;




typedef struct {
 int val[2];
} __kernel_fsid_t;





typedef __kernel_long_t __kernel_off_t;
typedef long long __kernel_loff_t;
typedef __kernel_long_t __kernel_time_t;
typedef __kernel_long_t __kernel_clock_t;
typedef int __kernel_timer_t;
typedef int __kernel_clockid_t;
typedef char * __kernel_caddr_t;
typedef unsigned short __kernel_uid16_t;
typedef unsigned short __kernel_gid16_t;
# 18 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/posix_types_64.h" 2
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/posix_types.h" 2
# 36 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/posix_types.h" 2
# 14 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/types.h" 2
# 32 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/types.h"
typedef __u16 __le16;
typedef __u16 __be16;
typedef __u32 __le32;
typedef __u32 __be32;
typedef __u64 __le64;
typedef __u64 __be64;

typedef __u16 __sum16;
typedef __u32 __wsum;
# 190 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/compiler.h" 2

static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) void data_access_exceeds_word_size(void)

__attribute__((warning("data access exceeds word size and won't be atomic")))

;

static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) void data_access_exceeds_word_size(void)
{
}

static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) void __read_once_size(const volatile void *p, void *res, int size)
{
 switch (size) {
 case 1: *(__u8 *)res = *(volatile __u8 *)p; break;
 case 2: *(__u16 *)res = *(volatile __u16 *)p; break;
 case 4: *(__u32 *)res = *(volatile __u32 *)p; break;

 case 8: *(__u64 *)res = *(volatile __u64 *)p; break;

 default:
  __asm__ __volatile__("": : :"memory");
  __builtin_memcpy((void *)res, (const void *)p, size);
  data_access_exceeds_word_size();
  __asm__ __volatile__("": : :"memory");
 }
}

static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) void __write_once_size(volatile void *p, void *res, int size)
{
 switch (size) {
 case 1: *(volatile __u8 *)p = *(__u8 *)res; break;
 case 2: *(volatile __u16 *)p = *(__u16 *)res; break;
 case 4: *(volatile __u32 *)p = *(__u32 *)res; break;

 case 8: *(volatile __u64 *)p = *(__u64 *)res; break;

 default:
  __asm__ __volatile__("": : :"memory");
  __builtin_memcpy((void *)p, (const void *)res, size);
  data_access_exceeds_word_size();
  __asm__ __volatile__("": : :"memory");
 }
}
# 14 "source/arch/x86/include/asm/syscalls.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/linkage.h" 1




# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/stringify.h" 1
# 6 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/linkage.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/export.h" 1
# 26 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/export.h"
struct kernel_symbol
{
 unsigned long value;
 const char *name;
};
# 7 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/linkage.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/linkage.h" 1
# 8 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/linkage.h" 2
# 15 "source/arch/x86/include/asm/syscalls.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/signal.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/list.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/types.h" 1
# 12 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/types.h"
typedef __u32 __kernel_dev_t;

typedef __kernel_fd_set fd_set;
typedef __kernel_dev_t dev_t;
typedef __kernel_ino_t ino_t;
typedef __kernel_mode_t mode_t;
typedef unsigned short umode_t;
typedef __u32 nlink_t;
typedef __kernel_off_t off_t;
typedef __kernel_pid_t pid_t;
typedef __kernel_daddr_t daddr_t;
typedef __kernel_key_t key_t;
typedef __kernel_suseconds_t suseconds_t;
typedef __kernel_timer_t timer_t;
typedef __kernel_clockid_t clockid_t;
typedef __kernel_mqd_t mqd_t;

typedef _Bool bool;

typedef __kernel_uid32_t uid_t;
typedef __kernel_gid32_t gid_t;
typedef __kernel_uid16_t uid16_t;
typedef __kernel_gid16_t gid16_t;

typedef unsigned long uintptr_t;



typedef __kernel_old_uid_t old_uid_t;
typedef __kernel_old_gid_t old_gid_t;



typedef __kernel_loff_t loff_t;
# 54 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/types.h"
typedef __kernel_size_t size_t;




typedef __kernel_ssize_t ssize_t;




typedef __kernel_ptrdiff_t ptrdiff_t;




typedef __kernel_time_t time_t;




typedef __kernel_clock_t clock_t;




typedef __kernel_caddr_t caddr_t;



typedef unsigned char u_char;
typedef unsigned short u_short;
typedef unsigned int u_int;
typedef unsigned long u_long;


typedef unsigned char unchar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef unsigned long ulong;




typedef __u8 u_int8_t;
typedef __s8 int8_t;
typedef __u16 u_int16_t;
typedef __s16 int16_t;
typedef __u32 u_int32_t;
typedef __s32 int32_t;



typedef __u8 uint8_t;
typedef __u16 uint16_t;
typedef __u32 uint32_t;


typedef __u64 uint64_t;
typedef __u64 u_int64_t;
typedef __s64 int64_t;
# 133 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/types.h"
typedef unsigned long sector_t;
typedef unsigned long blkcnt_t;
# 155 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/types.h"
typedef u64 dma_addr_t;
# 166 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/types.h"
typedef unsigned gfp_t;
typedef unsigned fmode_t;
typedef unsigned oom_flags_t;


typedef u64 phys_addr_t;




typedef phys_addr_t resource_size_t;





typedef unsigned long irq_hw_number_t;

typedef struct {
 int counter;
} atomic_t;


typedef struct {
 long counter;
} atomic64_t;


struct list_head {
 struct list_head *next, *prev;
};

struct hlist_head {
 struct hlist_node *first;
};

struct hlist_node {
 struct hlist_node *next, **pprev;
};

struct ustat {
 __kernel_daddr_t f_tfree;
 __kernel_ino_t f_tinode;
 char f_fname[6];
 char f_fpack[6];
};






struct callback_head {
 struct callback_head *next;
 void (*func)(struct callback_head *head);
};
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/list.h" 2

# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/poison.h" 1
# 7 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/list.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/const.h" 1
# 8 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/list.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/kernel.h" 1




# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.9/include/stdarg.h" 1 3 4
# 40 "/usr/lib/gcc/x86_64-linux-gnu/4.9/include/stdarg.h" 3 4
typedef __builtin_va_list __gnuc_va_list;
# 98 "/usr/lib/gcc/x86_64-linux-gnu/4.9/include/stdarg.h" 3 4
typedef __gnuc_va_list va_list;
# 6 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/kernel.h" 2




# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/bitops.h" 1
# 27 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/bitops.h"
extern unsigned int __sw_hweight8(unsigned int w);
extern unsigned int __sw_hweight16(unsigned int w);
extern unsigned int __sw_hweight32(unsigned int w);
extern unsigned long __sw_hweight64(__u64 w);





# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h" 1
# 16 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/alternative.h" 1






# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/asm.h" 1
# 8 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/alternative.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/ptrace.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/segment.h" 1
# 147 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/segment.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/cache.h" 1
# 148 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/segment.h" 2
# 214 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/segment.h"
extern const char early_idt_handler_array[32][9];
# 265 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/segment.h"
static inline __attribute__((no_instrument_function)) unsigned long get_limit(unsigned long segment)
{
 unsigned long __limit;
 asm("lsll %1,%0" : "=r" (__limit) : "r" (segment));
 return __limit + 1;
}
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/ptrace.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/page_types.h" 1
# 42 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/page_types.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/page_64_types.h" 1
# 43 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/page_types.h" 2






extern int devmem_is_allowed(unsigned long pagenr);

extern unsigned long max_low_pfn_mapped;
extern unsigned long max_pfn_mapped;

static inline __attribute__((no_instrument_function)) phys_addr_t get_max_mapped(void)
{
 return (phys_addr_t)max_pfn_mapped << 12;
}

bool pfn_range_is_mapped(unsigned long start_pfn, unsigned long end_pfn);

extern unsigned long init_memory_mapping(unsigned long start,
      unsigned long end);

extern void initmem_init(void);
# 6 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/ptrace.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/ptrace.h" 1




# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/ptrace-abi.h" 1
# 6 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/ptrace.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor-flags.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/processor-flags.h" 1
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor-flags.h" 2
# 7 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/ptrace.h" 2
# 7 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/ptrace.h" 2
# 33 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/ptrace.h"
struct pt_regs {
 unsigned long r15;
 unsigned long r14;
 unsigned long r13;
 unsigned long r12;
 unsigned long bp;
 unsigned long bx;

 unsigned long r11;
 unsigned long r10;
 unsigned long r9;
 unsigned long r8;
 unsigned long ax;
 unsigned long cx;
 unsigned long dx;
 unsigned long si;
 unsigned long di;
 unsigned long orig_ax;


 unsigned long ip;
 unsigned long cs;
 unsigned long flags;
 unsigned long sp;
 unsigned long ss;

};




# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/paravirt_types.h" 1
# 42 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/paravirt_types.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/desc_defs.h" 1
# 22 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/desc_defs.h"
struct desc_struct {
 union {
  struct {
   unsigned int a;
   unsigned int b;
  };
  struct {
   u16 limit0;
   u16 base0;
   unsigned base1: 8, type: 4, s: 1, dpl: 2, p: 1;
   unsigned limit: 4, avl: 1, l: 1, d: 1, g: 1, base2: 8;
  };
 };
} __attribute__((packed));







enum {
 GATE_INTERRUPT = 0xE,
 GATE_TRAP = 0xF,
 GATE_CALL = 0xC,
 GATE_TASK = 0x5,
};


struct gate_struct64 {
 u16 offset_low;
 u16 segment;
 unsigned ist : 3, zero0 : 5, type : 5, dpl : 2, p : 1;
 u16 offset_middle;
 u32 offset_high;
 u32 zero1;
} __attribute__((packed));





enum {
 DESC_TSS = 0x9,
 DESC_LDT = 0x2,
 DESCTYPE_S = 0x10,
};


struct ldttss_desc64 {
 u16 limit0;
 u16 base0;
 unsigned base1 : 8, type : 5, dpl : 2, p : 1;
 unsigned limit1 : 4, zero0 : 3, g : 1, base2 : 8;
 u32 base3;
 u32 zero1;
} __attribute__((packed));


typedef struct gate_struct64 gate_desc;
typedef struct ldttss_desc64 ldt_desc;
typedef struct ldttss_desc64 tss_desc;
# 94 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/desc_defs.h"
struct desc_ptr {
 unsigned short size;
 unsigned long address;
} __attribute__((packed)) ;
# 43 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/paravirt_types.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/kmap_types.h" 1







# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/kmap_types.h" 1
# 9 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/kmap_types.h" 2
# 44 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/paravirt_types.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/pgtable_types.h" 1
# 140 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/pgtable_types.h"
enum page_cache_mode {
 _PAGE_CACHE_MODE_WB = 0,
 _PAGE_CACHE_MODE_WC = 1,
 _PAGE_CACHE_MODE_UC_MINUS = 2,
 _PAGE_CACHE_MODE_UC = 3,
 _PAGE_CACHE_MODE_WT = 4,
 _PAGE_CACHE_MODE_WP = 5,
 _PAGE_CACHE_MODE_NUM = 8
};
# 231 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/pgtable_types.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/pgtable_64_types.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/sparsemem.h" 1
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/pgtable_64_types.h" 2







typedef unsigned long pteval_t;
typedef unsigned long pmdval_t;
typedef unsigned long pudval_t;
typedef unsigned long pgdval_t;
typedef unsigned long pgprotval_t;

typedef struct { pteval_t pte; } pte_t;
# 232 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/pgtable_types.h" 2
# 244 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/pgtable_types.h"
typedef struct pgprot { pgprotval_t pgprot; } pgprot_t;

typedef struct { pgdval_t pgd; } pgd_t;

static inline __attribute__((no_instrument_function)) pgd_t native_make_pgd(pgdval_t val)
{
 return (pgd_t) { val };
}

static inline __attribute__((no_instrument_function)) pgdval_t native_pgd_val(pgd_t pgd)
{
 return pgd.pgd;
}

static inline __attribute__((no_instrument_function)) pgdval_t pgd_flags(pgd_t pgd)
{
 return native_pgd_val(pgd) & (~((pteval_t)(((signed long)(~(((1UL) << 12)-1))) & ((phys_addr_t)((1ULL << 46) - 1)))));
}


typedef struct { pudval_t pud; } pud_t;

static inline __attribute__((no_instrument_function)) pud_t native_make_pud(pmdval_t val)
{
 return (pud_t) { val };
}

static inline __attribute__((no_instrument_function)) pudval_t native_pud_val(pud_t pud)
{
 return pud.pud;
}
# 285 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/pgtable_types.h"
typedef struct { pmdval_t pmd; } pmd_t;

static inline __attribute__((no_instrument_function)) pmd_t native_make_pmd(pmdval_t val)
{
 return (pmd_t) { val };
}

static inline __attribute__((no_instrument_function)) pmdval_t native_pmd_val(pmd_t pmd)
{
 return pmd.pmd;
}
# 305 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/pgtable_types.h"
static inline __attribute__((no_instrument_function)) pudval_t pud_flags(pud_t pud)
{
 return native_pud_val(pud) & (~((pteval_t)(((signed long)(~(((1UL) << 12)-1))) & ((phys_addr_t)((1ULL << 46) - 1)))));
}

static inline __attribute__((no_instrument_function)) pmdval_t pmd_flags(pmd_t pmd)
{
 return native_pmd_val(pmd) & (~((pteval_t)(((signed long)(~(((1UL) << 12)-1))) & ((phys_addr_t)((1ULL << 46) - 1)))));
}

static inline __attribute__((no_instrument_function)) pte_t native_make_pte(pteval_t val)
{
 return (pte_t) { .pte = val };
}

static inline __attribute__((no_instrument_function)) pteval_t native_pte_val(pte_t pte)
{
 return pte.pte;
}

static inline __attribute__((no_instrument_function)) pteval_t pte_flags(pte_t pte)
{
 return native_pte_val(pte) & (~((pteval_t)(((signed long)(~(((1UL) << 12)-1))) & ((phys_addr_t)((1ULL << 46) - 1)))));
}




static inline __attribute__((no_instrument_function)) pteval_t ptenuma_flags(pte_t pte)
{
 return pte_flags(pte) & ((((pteval_t)(1)) << (8 +1))|(((pteval_t)(1)) << 8)|(((pteval_t)(1)) << 0));
}

static inline __attribute__((no_instrument_function)) pmdval_t pmdnuma_flags(pmd_t pmd)
{
 return pmd_flags(pmd) & ((((pteval_t)(1)) << (8 +1))|(((pteval_t)(1)) << 8)|(((pteval_t)(1)) << 0));
}





extern uint16_t __cachemode2pte_tbl[_PAGE_CACHE_MODE_NUM];
extern uint8_t __pte2cachemode_tbl[8];
# 359 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/pgtable_types.h"
static inline __attribute__((no_instrument_function)) unsigned long cachemode2protval(enum page_cache_mode pcm)
{
 if (__builtin_expect(!!(pcm == 0), 1))
  return 0;
 return __cachemode2pte_tbl[pcm];
}
static inline __attribute__((no_instrument_function)) pgprot_t cachemode2pgprot(enum page_cache_mode pcm)
{
 return ((pgprot_t) { (cachemode2protval(pcm)) } );
}
static inline __attribute__((no_instrument_function)) enum page_cache_mode pgprot2cachemode(pgprot_t pgprot)
{
 unsigned long masked;

 masked = ((pgprot).pgprot) & ((((pteval_t)(1)) << 7) | (((pteval_t)(1)) << 4) | (((pteval_t)(1)) << 3));
 if (__builtin_expect(!!(masked == 0), 1))
  return 0;
 return __pte2cachemode_tbl[((((masked) >> (7 - 2)) & 4) | (((masked) >> (4 - 1)) & 2) | (((masked) >> 3) & 1))];
}
static inline __attribute__((no_instrument_function)) pgprot_t pgprot_4k_2_large(pgprot_t pgprot)
{
 pgprot_t new;
 unsigned long val;

 val = ((pgprot).pgprot);
 ((new).pgprot) = (val & ~((((pteval_t)(1)) << 7) | (((pteval_t)(1)) << 12))) |
  ((val & (((pteval_t)(1)) << 7)) << (12 - 7));
 return new;
}
static inline __attribute__((no_instrument_function)) pgprot_t pgprot_large_2_4k(pgprot_t pgprot)
{
 pgprot_t new;
 unsigned long val;

 val = ((pgprot).pgprot);
 ((new).pgprot) = (val & ~((((pteval_t)(1)) << 7) | (((pteval_t)(1)) << 12))) |
     ((val & (((pteval_t)(1)) << 12)) >>
      (12 - 7));
 return new;
}


typedef struct page *pgtable_t;

extern pteval_t __supported_pte_mask;
extern void set_nx(void);
extern int nx_enabled;


extern pgprot_t pgprot_writecombine(pgprot_t prot);





struct file;
pgprot_t phys_mem_access_prot(struct file *file, unsigned long pfn,
                              unsigned long size, pgprot_t vma_prot);
int phys_mem_access_prot_allowed(struct file *file, unsigned long pfn,
                              unsigned long size, pgprot_t *vma_prot);


void set_pte_vaddr(unsigned long vaddr, pte_t pte);







struct seq_file;
extern void arch_report_meminfo(struct seq_file *m);

enum pg_level {
 PG_LEVEL_NONE,
 PG_LEVEL_4K,
 PG_LEVEL_2M,
 PG_LEVEL_1G,
 PG_LEVEL_NUM
};


extern void update_page_count(int level, unsigned long pages);
# 452 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/pgtable_types.h"
extern pte_t *lookup_address(unsigned long address, unsigned int *level);
extern pte_t *lookup_address_in_pgd(pgd_t *pgd, unsigned long address,
        unsigned int *level);
extern pmd_t *lookup_pmd_address(unsigned long address);
extern phys_addr_t slow_virt_to_phys(void *__address);
extern int kernel_map_pages_in_pgd(pgd_t *pgd, u64 pfn, unsigned long address,
       unsigned numpages, unsigned long page_flags);
void kernel_unmap_pages_in_pgd(pgd_t *root, unsigned long address,
          unsigned numpages);
# 45 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/paravirt_types.h" 2

struct page;
struct thread_struct;
struct desc_ptr;
struct tss_struct;
struct mm_struct;
struct desc_struct;
struct task_struct;
struct cpumask;





struct paravirt_callee_save {
 void *func;
};


struct pv_info {
 unsigned int kernel_rpl;
 int shared_kernel_pmd;


 u16 extra_user_64bit_cs;


 int paravirt_enabled;
 const char *name;
};

struct pv_init_ops {
# 85 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/paravirt_types.h"
 unsigned (*patch)(u8 type, u16 clobber, void *insnbuf,
     unsigned long addr, unsigned len);
};


struct pv_lazy_ops {

 void (*enter)(void);
 void (*leave)(void);
 void (*flush)(void);
};

struct pv_time_ops {
 unsigned long long (*sched_clock)(void);
 unsigned long long (*steal_clock)(int cpu);
 unsigned long (*get_tsc_khz)(void);
};

struct pv_cpu_ops {

 unsigned long (*get_debugreg)(int regno);
 void (*set_debugreg)(int regno, unsigned long value);

 void (*clts)(void);

 unsigned long (*read_cr0)(void);
 void (*write_cr0)(unsigned long);

 unsigned long (*read_cr4_safe)(void);
 unsigned long (*read_cr4)(void);
 void (*write_cr4)(unsigned long);


 unsigned long (*read_cr8)(void);
 void (*write_cr8)(unsigned long);



 void (*load_tr_desc)(void);
 void (*load_gdt)(const struct desc_ptr *);
 void (*load_idt)(const struct desc_ptr *);

 void (*store_idt)(struct desc_ptr *);
 void (*set_ldt)(const void *desc, unsigned entries);
 unsigned long (*store_tr)(void);
 void (*load_tls)(struct thread_struct *t, unsigned int cpu);

 void (*load_gs_index)(unsigned int idx);

 void (*write_ldt_entry)(struct desc_struct *ldt, int entrynum,
    const void *desc);
 void (*write_gdt_entry)(struct desc_struct *,
    int entrynum, const void *desc, int size);
 void (*write_idt_entry)(gate_desc *,
    int entrynum, const gate_desc *gate);
 void (*alloc_ldt)(struct desc_struct *ldt, unsigned entries);
 void (*free_ldt)(struct desc_struct *ldt, unsigned entries);

 void (*load_sp0)(struct tss_struct *tss, struct thread_struct *t);

 void (*set_iopl_mask)(unsigned mask);

 void (*wbinvd)(void);
 void (*io_delay)(void);


 void (*cpuid)(unsigned int *eax, unsigned int *ebx,
        unsigned int *ecx, unsigned int *edx);



 u64 (*read_msr)(unsigned int msr, int *err);
 int (*write_msr)(unsigned int msr, unsigned low, unsigned high);

 u64 (*read_tsc)(void);
 u64 (*read_pmc)(int counter);
 unsigned long long (*read_tscp)(unsigned int *aux);







 void (*irq_enable_sysexit)(void);







 void (*usergs_sysret64)(void);







 void (*usergs_sysret32)(void);



 void (*iret)(void);

 void (*swapgs)(void);

 void (*start_context_switch)(struct task_struct *prev);
 void (*end_context_switch)(struct task_struct *next);
};

struct pv_irq_ops {
# 207 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/paravirt_types.h"
 struct paravirt_callee_save save_fl;
 struct paravirt_callee_save restore_fl;
 struct paravirt_callee_save irq_disable;
 struct paravirt_callee_save irq_enable;

 void (*safe_halt)(void);
 void (*halt)(void);


 void (*adjust_exception_frame)(void);

};

struct pv_apic_ops {

 void (*startup_ipi_hook)(int phys_apicid,
     unsigned long start_eip,
     unsigned long start_esp);

};

struct pv_mmu_ops {
 unsigned long (*read_cr2)(void);
 void (*write_cr2)(unsigned long);

 unsigned long (*read_cr3)(void);
 void (*write_cr3)(unsigned long);





 void (*activate_mm)(struct mm_struct *prev,
       struct mm_struct *next);
 void (*dup_mmap)(struct mm_struct *oldmm,
    struct mm_struct *mm);
 void (*exit_mmap)(struct mm_struct *mm);



 void (*flush_tlb_user)(void);
 void (*flush_tlb_kernel)(void);
 void (*flush_tlb_single)(unsigned long addr);
 void (*flush_tlb_others)(const struct cpumask *cpus,
     struct mm_struct *mm,
     unsigned long start,
     unsigned long end);


 int (*pgd_alloc)(struct mm_struct *mm);
 void (*pgd_free)(struct mm_struct *mm, pgd_t *pgd);





 void (*alloc_pte)(struct mm_struct *mm, unsigned long pfn);
 void (*alloc_pmd)(struct mm_struct *mm, unsigned long pfn);
 void (*alloc_pud)(struct mm_struct *mm, unsigned long pfn);
 void (*release_pte)(unsigned long pfn);
 void (*release_pmd)(unsigned long pfn);
 void (*release_pud)(unsigned long pfn);


 void (*set_pte)(pte_t *ptep, pte_t pteval);
 void (*set_pte_at)(struct mm_struct *mm, unsigned long addr,
      pte_t *ptep, pte_t pteval);
 void (*set_pmd)(pmd_t *pmdp, pmd_t pmdval);
 void (*set_pmd_at)(struct mm_struct *mm, unsigned long addr,
      pmd_t *pmdp, pmd_t pmdval);
 void (*pte_update)(struct mm_struct *mm, unsigned long addr,
      pte_t *ptep);
 void (*pte_update_defer)(struct mm_struct *mm,
     unsigned long addr, pte_t *ptep);
 void (*pmd_update)(struct mm_struct *mm, unsigned long addr,
      pmd_t *pmdp);
 void (*pmd_update_defer)(struct mm_struct *mm,
     unsigned long addr, pmd_t *pmdp);

 pte_t (*ptep_modify_prot_start)(struct mm_struct *mm, unsigned long addr,
     pte_t *ptep);
 void (*ptep_modify_prot_commit)(struct mm_struct *mm, unsigned long addr,
     pte_t *ptep, pte_t pte);

 struct paravirt_callee_save pte_val;
 struct paravirt_callee_save make_pte;

 struct paravirt_callee_save pgd_val;
 struct paravirt_callee_save make_pgd;
# 306 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/paravirt_types.h"
 void (*set_pud)(pud_t *pudp, pud_t pudval);

 struct paravirt_callee_save pmd_val;
 struct paravirt_callee_save make_pmd;


 struct paravirt_callee_save pud_val;
 struct paravirt_callee_save make_pud;

 void (*set_pgd)(pgd_t *pudp, pgd_t pgdval);



 struct pv_lazy_ops lazy_mode;





 void (*set_fixmap)(unsigned idx,
      phys_addr_t phys, pgprot_t flags);
};

struct arch_spinlock;

# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/spinlock_types.h" 1
# 18 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/spinlock_types.h"
typedef u16 __ticket_t;
typedef u32 __ticketpair_t;






typedef struct arch_spinlock {
 union {
  __ticketpair_t head_tail;
  struct __raw_tickets {
   __ticket_t head, tail;
  } tickets;
 };
} arch_spinlock_t;



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/qrwlock_types.h" 1




# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/spinlock_types.h" 1
# 6 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/qrwlock_types.h" 2





typedef struct qrwlock {
 atomic_t cnts;
 arch_spinlock_t lock;
} arch_rwlock_t;
# 38 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/spinlock_types.h" 2
# 332 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/paravirt_types.h" 2




struct pv_lock_ops {
 struct paravirt_callee_save lock_spinning;
 void (*unlock_kick)(struct arch_spinlock *lock, __ticket_t ticket);
};




struct paravirt_patch_template {
 struct pv_init_ops pv_init_ops;
 struct pv_time_ops pv_time_ops;
 struct pv_cpu_ops pv_cpu_ops;
 struct pv_irq_ops pv_irq_ops;
 struct pv_apic_ops pv_apic_ops;
 struct pv_mmu_ops pv_mmu_ops;
 struct pv_lock_ops pv_lock_ops;
};

extern struct pv_info pv_info;
extern struct pv_init_ops pv_init_ops;
extern struct pv_time_ops pv_time_ops;
extern struct pv_cpu_ops pv_cpu_ops;
extern struct pv_irq_ops pv_irq_ops;
extern struct pv_apic_ops pv_apic_ops;
extern struct pv_mmu_ops pv_mmu_ops;
extern struct pv_lock_ops pv_lock_ops;
# 397 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/paravirt_types.h"
unsigned paravirt_patch_nop(void);
unsigned paravirt_patch_ident_32(void *insnbuf, unsigned len);
unsigned paravirt_patch_ident_64(void *insnbuf, unsigned len);
unsigned paravirt_patch_ignore(unsigned len);
unsigned paravirt_patch_call(void *insnbuf,
        const void *target, u16 tgt_clobbers,
        unsigned long addr, u16 site_clobbers,
        unsigned len);
unsigned paravirt_patch_jmp(void *insnbuf, const void *target,
       unsigned long addr, unsigned len);
unsigned paravirt_patch_default(u8 type, u16 clobbers, void *insnbuf,
    unsigned long addr, unsigned len);

unsigned paravirt_patch_insns(void *insnbuf, unsigned len,
         const char *start, const char *end);

unsigned native_patch(u8 type, u16 clobbers, void *ibuf,
        unsigned long addr, unsigned len);

int paravirt_disable_iospace(void);
# 675 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/paravirt_types.h"
enum paravirt_lazy_mode {
 PARAVIRT_LAZY_NONE,
 PARAVIRT_LAZY_MMU,
 PARAVIRT_LAZY_CPU,
};

enum paravirt_lazy_mode paravirt_get_lazy_mode(void);
void paravirt_start_context_switch(struct task_struct *prev);
void paravirt_end_context_switch(struct task_struct *next);

void paravirt_enter_lazy_mmu(void);
void paravirt_leave_lazy_mmu(void);
void paravirt_flush_lazy_mmu(void);

void _paravirt_nop(void);
u32 _paravirt_ident_32(u32);
u64 _paravirt_ident_64(u64);




struct paravirt_patch_site {
 u8 *instr;
 u8 instrtype;
 u8 len;
 u16 clobbers;
};

extern struct paravirt_patch_site __parainstructions[],
 __parainstructions_end[];
# 65 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/ptrace.h" 2


struct cpuinfo_x86;
struct task_struct;

extern unsigned long profile_pc(struct pt_regs *regs);


extern unsigned long
convert_ip_to_linear(struct task_struct *child, struct pt_regs *regs);
extern void send_sigtrap(struct task_struct *tsk, struct pt_regs *regs,
    int error_code, int si_code);


extern unsigned long syscall_trace_enter_phase1(struct pt_regs *, u32 arch);
extern long syscall_trace_enter_phase2(struct pt_regs *, u32 arch,
           unsigned long phase1_result);

extern long syscall_trace_enter(struct pt_regs *);
extern void syscall_trace_leave(struct pt_regs *);

static inline __attribute__((no_instrument_function)) unsigned long regs_return_value(struct pt_regs *regs)
{
 return regs->ax;
}
# 98 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/ptrace.h"
static inline __attribute__((no_instrument_function)) int user_mode(struct pt_regs *regs)
{



 return !!(regs->cs & 3);

}

static inline __attribute__((no_instrument_function)) int user_mode_vm(struct pt_regs *regs)
{




 return user_mode(regs);

}

static inline __attribute__((no_instrument_function)) int v8086_mode(struct pt_regs *regs)
{



 return 0;

}


static inline __attribute__((no_instrument_function)) bool user_64bit_mode(struct pt_regs *regs)
{
# 137 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/ptrace.h"
 return regs->cs == (6*8+3) || regs->cs == pv_info.extra_user_64bit_cs;

}
# 152 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/ptrace.h"
static inline __attribute__((no_instrument_function)) unsigned long kernel_stack_pointer(struct pt_regs *regs)
{
 return regs->sp;
}






# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/ptrace.h" 1
# 22 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/ptrace.h"
static inline __attribute__((no_instrument_function)) unsigned long instruction_pointer(struct pt_regs *regs)
{
 return ((regs)->ip);
}
static inline __attribute__((no_instrument_function)) void instruction_pointer_set(struct pt_regs *regs,
                                           unsigned long val)
{
 (((regs)->ip) = (val));
}
# 44 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/ptrace.h"
static inline __attribute__((no_instrument_function)) unsigned long user_stack_pointer(struct pt_regs *regs)
{
 return ((regs)->sp);
}
static inline __attribute__((no_instrument_function)) void user_stack_pointer_set(struct pt_regs *regs,
                                          unsigned long val)
{
 (((regs)->sp) = (val));
}
# 62 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/ptrace.h"
static inline __attribute__((no_instrument_function)) unsigned long frame_pointer(struct pt_regs *regs)
{
 return ((regs)->bp);
}
static inline __attribute__((no_instrument_function)) void frame_pointer_set(struct pt_regs *regs,
                                     unsigned long val)
{
 (((regs)->bp) = (val));
}
# 163 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/ptrace.h" 2


extern int regs_query_register_offset(const char *name);
extern const char *regs_query_register_name(unsigned int offset);
# 178 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/ptrace.h"
static inline __attribute__((no_instrument_function)) unsigned long regs_get_register(struct pt_regs *regs,
           unsigned int offset)
{
 if (__builtin_expect(!!(offset > (__builtin_offsetof(struct pt_regs,ss))), 0))
  return 0;
# 192 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/ptrace.h"
 return *(unsigned long *)((unsigned long)regs + offset);
}
# 203 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/ptrace.h"
static inline __attribute__((no_instrument_function)) int regs_within_kernel_stack(struct pt_regs *regs,
        unsigned long addr)
{
 return ((addr & ~((((1UL) << 12) << 2) - 1)) ==
  (kernel_stack_pointer(regs) & ~((((1UL) << 12) << 2) - 1)));
}
# 219 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/ptrace.h"
static inline __attribute__((no_instrument_function)) unsigned long regs_get_kernel_stack_nth(struct pt_regs *regs,
            unsigned int n)
{
 unsigned long *addr = (unsigned long *)kernel_stack_pointer(regs);
 addr += n;
 if (regs_within_kernel_stack(regs, (unsigned long)addr))
  return *addr;
 else
  return 0;
}
# 255 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/ptrace.h"
struct user_desc;
extern int do_get_thread_area(struct task_struct *p, int idx,
         struct user_desc *info);
extern int do_set_thread_area(struct task_struct *p, int idx,
         struct user_desc *info, int can_allocate);
# 9 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/alternative.h" 2
# 46 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/alternative.h"
struct alt_instr {
 s32 instr_offset;
 s32 repl_offset;
 u16 cpuid;
 u8 instrlen;
 u8 replacementlen;
};

extern void alternative_instructions(void);
extern void apply_alternatives(struct alt_instr *start, struct alt_instr *end);

struct module;


extern void alternatives_smp_module_add(struct module *mod, char *name,
     void *locks, void *locks_end,
     void *text, void *text_end);
extern void alternatives_smp_module_del(struct module *mod);
extern void alternatives_enable_smp(void);
extern int alternatives_text_reserved(void *start, void *end);
extern bool skip_smp_alternatives;
# 132 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/alternative.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/cpufeature.h" 1







# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/required-features.h" 1
# 9 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/cpufeature.h" 2



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/disabled-features.h" 1
# 13 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/cpufeature.h" 2
# 261 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/cpufeature.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/bitops.h" 1
# 262 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/cpufeature.h" 2


extern const char * const x86_cap_flags[11*32];
extern const char * const x86_power_flags[32];
# 277 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/cpufeature.h"
extern const char * const x86_bug_flags[1*32];
# 393 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/cpufeature.h"
extern void warn_pre_alternatives(void);
extern bool __static_cpu_has_safe(u16 bit);






static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) __attribute__((pure)) bool __static_cpu_has(u16 bit)
{
# 448 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/cpufeature.h"
  u8 flag;

  asm volatile("1: movb $0,%0\n"
        "2:\n"
        ".section .altinstructions,\"a\"\n"
        " .long 1b - .\n"
        " .long 3f - .\n"
        " .word %P1\n"
        " .byte 2b - 1b\n"
        " .byte 4f - 3f\n"
        ".previous\n"
        ".section .discard,\"aw\",@progbits\n"
        " .byte 0xff + (4f-3f) - (2b-1b)\n"
        ".previous\n"
        ".section .altinstr_replacement,\"ax\"\n"
        "3: movb $1,%0\n"
        "4:\n"
        ".previous\n"
        : "=qm" (flag) : "i" (bit));
  return flag;


}
# 481 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/cpufeature.h"
static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) __attribute__((pure)) bool _static_cpu_has_safe(u16 bit)
{
# 518 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/cpufeature.h"
  u8 flag;

  asm volatile("1: movb $2,%0\n"
        "2:\n"
        ".section .altinstructions,\"a\"\n"
        " .long 1b - .\n"
        " .long 3f - .\n"
        " .word %P2\n"
        " .byte 2b - 1b\n"
        " .byte 4f - 3f\n"
        ".previous\n"
        ".section .discard,\"aw\",@progbits\n"
        " .byte 0xff + (4f-3f) - (2b-1b)\n"
        ".previous\n"
        ".section .altinstr_replacement,\"ax\"\n"
        "3: movb $0,%0\n"
        "4:\n"
        ".previous\n"
        ".section .altinstructions,\"a\"\n"
        " .long 1b - .\n"
        " .long 5f - .\n"
        " .word %P1\n"
        " .byte 4b - 3b\n"
        " .byte 6f - 5f\n"
        ".previous\n"
        ".section .discard,\"aw\",@progbits\n"
        " .byte 0xff + (6f-5f) - (4b-3b)\n"
        ".previous\n"
        ".section .altinstr_replacement,\"ax\"\n"
        "5: movb $1,%0\n"
        "6:\n"
        ".previous\n"
        : "=qm" (flag)
        : "i" (bit), "i" (( 3*32+21)));
  return (flag == 2 ? __static_cpu_has_safe(bit) : flag);

}
# 133 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/alternative.h" 2
# 213 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/alternative.h"
struct paravirt_patch_site;

void apply_paravirt(struct paravirt_patch_site *start,
      struct paravirt_patch_site *end);
# 225 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/alternative.h"
extern void *text_poke_early(void *addr, const void *opcode, size_t len);
# 241 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/alternative.h"
extern void *text_poke(void *addr, const void *opcode, size_t len);
extern int poke_int3_handler(struct pt_regs *regs);
extern void *text_poke_bp(void *addr, const void *opcode, size_t len, void *handler);
# 17 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/rmwcc.h" 1
# 18 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/barrier.h" 1




# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/nops.h" 1
# 142 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/nops.h"
extern const unsigned char * const *ideal_nops;
extern void arch_init_ideal_nops(void);
# 6 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/barrier.h" 2
# 101 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/barrier.h"
static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) void rdtsc_barrier(void)
{
 asm volatile ("661:\n\t" ".byte " "0x66,0x66,0x90" "\n" "\n662:\n" ".pushsection .altinstructions,\"a\"\n" " .long 661b - .\n" " .long " "663""1""f - .\n" " .word " "( 3*32+17)" "\n" " .byte " "662b-661b" "\n" " .byte " "664""1""f-""663""1""f" "\n" ".popsection\n" ".pushsection .discard,\"aw\",@progbits\n" " .byte 0xff + (" "664""1""f-""663""1""f" ") - (" "662b-661b" ")\n" ".popsection\n" ".pushsection .altinstr_replacement, \"ax\"\n" "663""1"":\n\t" "mfence" "\n" "664""1" ":\n\t" ".popsection" : : : "memory");
 asm volatile ("661:\n\t" ".byte " "0x66,0x66,0x90" "\n" "\n662:\n" ".pushsection .altinstructions,\"a\"\n" " .long 661b - .\n" " .long " "663""1""f - .\n" " .word " "( 3*32+18)" "\n" " .byte " "662b-661b" "\n" " .byte " "664""1""f-""663""1""f" "\n" ".popsection\n" ".pushsection .discard,\"aw\",@progbits\n" " .byte 0xff + (" "664""1""f-""663""1""f" ") - (" "662b-661b" ")\n" ".popsection\n" ".pushsection .altinstr_replacement, \"ax\"\n" "663""1"":\n\t" "lfence" "\n" "664""1" ":\n\t" ".popsection" : : : "memory");
}
# 19 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h" 2
# 71 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) void
set_bit(long nr, volatile unsigned long *addr)
{
 if ((__builtin_constant_p(nr))) {
  asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "orb %1,%0"
   : "+m" (*(volatile long *) ((void *)(addr) + ((nr)>>3)))
   : "iq" ((u8)(1 << ((nr) & 7)))
   : "memory");
 } else {
  asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "bts %1,%0"
   : "+m" (*(volatile long *) (addr)) : "Ir" (nr) : "memory");
 }
}
# 94 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
static inline __attribute__((no_instrument_function)) void __set_bit(long nr, volatile unsigned long *addr)
{
 asm volatile("bts %1,%0" : "+m" (*(volatile long *) (addr)) : "Ir" (nr) : "memory");
}
# 109 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) void
clear_bit(long nr, volatile unsigned long *addr)
{
 if ((__builtin_constant_p(nr))) {
  asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "andb %1,%0"
   : "+m" (*(volatile long *) ((void *)(addr) + ((nr)>>3)))
   : "iq" ((u8)~(1 << ((nr) & 7))));
 } else {
  asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "btr %1,%0"
   : "+m" (*(volatile long *) (addr))
   : "Ir" (nr));
 }
}
# 131 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
static inline __attribute__((no_instrument_function)) void clear_bit_unlock(long nr, volatile unsigned long *addr)
{
 __asm__ __volatile__("": : :"memory");
 clear_bit(nr, addr);
}

static inline __attribute__((no_instrument_function)) void __clear_bit(long nr, volatile unsigned long *addr)
{
 asm volatile("btr %1,%0" : "+m" (*(volatile long *) (addr)) : "Ir" (nr));
}
# 154 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
static inline __attribute__((no_instrument_function)) void __clear_bit_unlock(long nr, volatile unsigned long *addr)
{
 __asm__ __volatile__("": : :"memory");
 __clear_bit(nr, addr);
}
# 169 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
static inline __attribute__((no_instrument_function)) void __change_bit(long nr, volatile unsigned long *addr)
{
 asm volatile("btc %1,%0" : "+m" (*(volatile long *) (addr)) : "Ir" (nr));
}
# 183 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
static inline __attribute__((no_instrument_function)) void change_bit(long nr, volatile unsigned long *addr)
{
 if ((__builtin_constant_p(nr))) {
  asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "xorb %1,%0"
   : "+m" (*(volatile long *) ((void *)(addr) + ((nr)>>3)))
   : "iq" ((u8)(1 << ((nr) & 7))));
 } else {
  asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "btc %1,%0"
   : "+m" (*(volatile long *) (addr))
   : "Ir" (nr));
 }
}
# 204 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
static inline __attribute__((no_instrument_function)) int test_and_set_bit(long nr, volatile unsigned long *addr)
{
 do { char c; asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "bts" " %2, " "%0" "; set" "c" " %1" : "+m" (*addr), "=qm" (c) : "Ir" (nr) : "memory"); return c != 0; } while (0);
}
# 216 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) int
test_and_set_bit_lock(long nr, volatile unsigned long *addr)
{
 return test_and_set_bit(nr, addr);
}
# 231 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
static inline __attribute__((no_instrument_function)) int __test_and_set_bit(long nr, volatile unsigned long *addr)
{
 int oldbit;

 asm("bts %2,%1\n\t"
     "sbb %0,%0"
     : "=r" (oldbit), "+m" (*(volatile long *) (addr))
     : "Ir" (nr));
 return oldbit;
}
# 250 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
static inline __attribute__((no_instrument_function)) int test_and_clear_bit(long nr, volatile unsigned long *addr)
{
 do { char c; asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "btr" " %2, " "%0" "; set" "c" " %1" : "+m" (*addr), "=qm" (c) : "Ir" (nr) : "memory"); return c != 0; } while (0);
}
# 271 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
static inline __attribute__((no_instrument_function)) int __test_and_clear_bit(long nr, volatile unsigned long *addr)
{
 int oldbit;

 asm volatile("btr %2,%1\n\t"
       "sbb %0,%0"
       : "=r" (oldbit), "+m" (*(volatile long *) (addr))
       : "Ir" (nr));
 return oldbit;
}


static inline __attribute__((no_instrument_function)) int __test_and_change_bit(long nr, volatile unsigned long *addr)
{
 int oldbit;

 asm volatile("btc %2,%1\n\t"
       "sbb %0,%0"
       : "=r" (oldbit), "+m" (*(volatile long *) (addr))
       : "Ir" (nr) : "memory");

 return oldbit;
}
# 303 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
static inline __attribute__((no_instrument_function)) int test_and_change_bit(long nr, volatile unsigned long *addr)
{
 do { char c; asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "btc" " %2, " "%0" "; set" "c" " %1" : "+m" (*addr), "=qm" (c) : "Ir" (nr) : "memory"); return c != 0; } while (0);
}

static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) int constant_test_bit(long nr, const volatile unsigned long *addr)
{
 return ((1UL << (nr & (64 -1))) &
  (addr[nr >> 6])) != 0;
}

static inline __attribute__((no_instrument_function)) int variable_test_bit(long nr, volatile const unsigned long *addr)
{
 int oldbit;

 asm volatile("bt %2,%1\n\t"
       "sbb %0,%0"
       : "=r" (oldbit)
       : "m" (*(unsigned long *)addr), "Ir" (nr));

 return oldbit;
}
# 346 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
static inline __attribute__((no_instrument_function)) unsigned long __ffs(unsigned long word)
{
 asm("rep; bsf %1,%0"
  : "=r" (word)
  : "rm" (word));
 return word;
}







static inline __attribute__((no_instrument_function)) unsigned long ffz(unsigned long word)
{
 asm("rep; bsf %1,%0"
  : "=r" (word)
  : "r" (~word));
 return word;
}







static inline __attribute__((no_instrument_function)) unsigned long __fls(unsigned long word)
{
 asm("bsr %1,%0"
     : "=r" (word)
     : "rm" (word));
 return word;
}
# 396 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
static inline __attribute__((no_instrument_function)) int ffs(int x)
{
 int r;
# 410 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
 asm("bsfl %1,%0"
     : "=r" (r)
     : "rm" (x), "0" (-1));
# 423 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
 return r + 1;
}
# 437 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
static inline __attribute__((no_instrument_function)) int fls(int x)
{
 int r;
# 451 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
 asm("bsrl %1,%0"
     : "=r" (r)
     : "rm" (x), "0" (-1));
# 464 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
 return r + 1;
}
# 479 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h"
static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) int fls64(__u64 x)
{
 int bitpos = -1;





 asm("bsrq %1,%q0"
     : "+r" (bitpos)
     : "rm" (x));
 return bitpos + 1;
}




# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/bitops/find.h" 1
# 14 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/bitops/find.h"
extern unsigned long find_next_bit(const unsigned long *addr, unsigned long
  size, unsigned long offset);
# 28 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/bitops/find.h"
extern unsigned long find_next_zero_bit(const unsigned long *addr, unsigned
  long size, unsigned long offset);
# 42 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/bitops/find.h"
extern unsigned long find_first_bit(const unsigned long *addr,
        unsigned long size);
# 53 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/bitops/find.h"
extern unsigned long find_first_zero_bit(const unsigned long *addr,
      unsigned long size);
# 497 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h" 2

# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/bitops/sched.h" 1
# 12 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/bitops/sched.h"
static inline __attribute__((no_instrument_function)) int sched_find_first_bit(const unsigned long *b)
{

 if (b[0])
  return __ffs(b[0]);
 return __ffs(b[1]) + 64;
# 29 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/bitops/sched.h"
}
# 499 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h" 2

# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/arch_hweight.h" 1
# 24 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/arch_hweight.h"
static inline __attribute__((no_instrument_function)) unsigned int __arch_hweight32(unsigned int w)
{
 unsigned int res = 0;

 asm ("661:\n\t" "call __sw_hweight32" "\n662:\n" ".pushsection .altinstructions,\"a\"\n" " .long 661b - .\n" " .long " "663""1""f - .\n" " .word " "( 4*32+23)" "\n" " .byte " "662b-661b" "\n" " .byte " "664""1""f-""663""1""f" "\n" ".popsection\n" ".pushsection .discard,\"aw\",@progbits\n" " .byte 0xff + (" "664""1""f-""663""1""f" ") - (" "662b-661b" ")\n" ".popsection\n" ".pushsection .altinstr_replacement, \"ax\"\n" "663""1"":\n\t" ".byte 0xf3,0x40,0x0f,0xb8,0xc7" "\n" "664""1" ":\n\t" ".popsection"
       : "=""a" (res)
       : "D" (w));

 return res;
}

static inline __attribute__((no_instrument_function)) unsigned int __arch_hweight16(unsigned int w)
{
 return __arch_hweight32(w & 0xffff);
}

static inline __attribute__((no_instrument_function)) unsigned int __arch_hweight8(unsigned int w)
{
 return __arch_hweight32(w & 0xff);
}

static inline __attribute__((no_instrument_function)) unsigned long __arch_hweight64(__u64 w)
{
 unsigned long res = 0;





 asm ("661:\n\t" "call __sw_hweight64" "\n662:\n" ".pushsection .altinstructions,\"a\"\n" " .long 661b - .\n" " .long " "663""1""f - .\n" " .word " "( 4*32+23)" "\n" " .byte " "662b-661b" "\n" " .byte " "664""1""f-""663""1""f" "\n" ".popsection\n" ".pushsection .discard,\"aw\",@progbits\n" " .byte 0xff + (" "664""1""f-""663""1""f" ") - (" "662b-661b" ")\n" ".popsection\n" ".pushsection .altinstr_replacement, \"ax\"\n" "663""1"":\n\t" ".byte 0xf3,0x48,0x0f,0xb8,0xc7" "\n" "664""1" ":\n\t" ".popsection"
       : "=""a" (res)
       : "D" (w));


 return res;
}
# 501 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h" 2

# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/bitops/const_hweight.h" 1
# 503 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h" 2

# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/bitops/le.h" 1




# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/byteorder.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/byteorder/little_endian.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/byteorder/little_endian.h" 1
# 12 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/byteorder/little_endian.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/swab.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/swab.h" 1





# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/swab.h" 1






static inline __attribute__((no_instrument_function)) __attribute__((__const__)) __u32 __arch_swab32(__u32 val)
{
 asm("bswapl %0" : "=r" (val) : "0" (val));
 return val;
}


static inline __attribute__((no_instrument_function)) __attribute__((__const__)) __u64 __arch_swab64(__u64 val)
{
# 30 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/swab.h"
 asm("bswapq %0" : "=r" (val) : "0" (val));
 return val;

}
# 7 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/swab.h" 2
# 46 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/swab.h"
static inline __attribute__((no_instrument_function)) __attribute__((__const__)) __u16 __fswab16(__u16 val)
{

 return __builtin_bswap16(val);





}

static inline __attribute__((no_instrument_function)) __attribute__((__const__)) __u32 __fswab32(__u32 val)
{

 return __builtin_bswap32(val);





}

static inline __attribute__((no_instrument_function)) __attribute__((__const__)) __u64 __fswab64(__u64 val)
{

 return __builtin_bswap64(val);
# 81 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/swab.h"
}

static inline __attribute__((no_instrument_function)) __attribute__((__const__)) __u32 __fswahw32(__u32 val)
{



 return ((__u32)( (((__u32)(val) & (__u32)0x0000ffffUL) << 16) | (((__u32)(val) & (__u32)0xffff0000UL) >> 16)));

}

static inline __attribute__((no_instrument_function)) __attribute__((__const__)) __u32 __fswahb32(__u32 val)
{



 return ((__u32)( (((__u32)(val) & (__u32)0x00ff00ffUL) << 8) | (((__u32)(val) & (__u32)0xff00ff00UL) >> 8)));

}
# 154 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/swab.h"
static inline __attribute__((no_instrument_function)) __u16 __swab16p(const __u16 *p)
{



 return (__builtin_constant_p((__u16)(*p)) ? ((__u16)( (((__u16)(*p) & (__u16)0x00ffU) << 8) | (((__u16)(*p) & (__u16)0xff00U) >> 8))) : __fswab16(*p));

}





static inline __attribute__((no_instrument_function)) __u32 __swab32p(const __u32 *p)
{



 return (__builtin_constant_p((__u32)(*p)) ? ((__u32)( (((__u32)(*p) & (__u32)0x000000ffUL) << 24) | (((__u32)(*p) & (__u32)0x0000ff00UL) << 8) | (((__u32)(*p) & (__u32)0x00ff0000UL) >> 8) | (((__u32)(*p) & (__u32)0xff000000UL) >> 24))) : __fswab32(*p));

}





static inline __attribute__((no_instrument_function)) __u64 __swab64p(const __u64 *p)
{



 return (__builtin_constant_p((__u64)(*p)) ? ((__u64)( (((__u64)(*p) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(*p) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(*p) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(*p) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(*p) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(*p) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(*p) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(*p) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(*p));

}







static inline __attribute__((no_instrument_function)) __u32 __swahw32p(const __u32 *p)
{



 return (__builtin_constant_p((__u32)(*p)) ? ((__u32)( (((__u32)(*p) & (__u32)0x0000ffffUL) << 16) | (((__u32)(*p) & (__u32)0xffff0000UL) >> 16))) : __fswahw32(*p));

}







static inline __attribute__((no_instrument_function)) __u32 __swahb32p(const __u32 *p)
{



 return (__builtin_constant_p((__u32)(*p)) ? ((__u32)( (((__u32)(*p) & (__u32)0x00ff00ffUL) << 8) | (((__u32)(*p) & (__u32)0xff00ff00UL) >> 8))) : __fswahb32(*p));

}





static inline __attribute__((no_instrument_function)) void __swab16s(__u16 *p)
{



 *p = __swab16p(p);

}




static inline __attribute__((no_instrument_function)) void __swab32s(__u32 *p)
{



 *p = __swab32p(p);

}





static inline __attribute__((no_instrument_function)) void __swab64s(__u64 *p)
{



 *p = __swab64p(p);

}







static inline __attribute__((no_instrument_function)) void __swahw32s(__u32 *p)
{



 *p = __swahw32p(p);

}







static inline __attribute__((no_instrument_function)) void __swahb32s(__u32 *p)
{



 *p = __swahb32p(p);

}
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/swab.h" 2
# 13 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/byteorder/little_endian.h" 2
# 43 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/byteorder/little_endian.h"
static inline __attribute__((no_instrument_function)) __le64 __cpu_to_le64p(const __u64 *p)
{
 return ( __le64)*p;
}
static inline __attribute__((no_instrument_function)) __u64 __le64_to_cpup(const __le64 *p)
{
 return ( __u64)*p;
}
static inline __attribute__((no_instrument_function)) __le32 __cpu_to_le32p(const __u32 *p)
{
 return ( __le32)*p;
}
static inline __attribute__((no_instrument_function)) __u32 __le32_to_cpup(const __le32 *p)
{
 return ( __u32)*p;
}
static inline __attribute__((no_instrument_function)) __le16 __cpu_to_le16p(const __u16 *p)
{
 return ( __le16)*p;
}
static inline __attribute__((no_instrument_function)) __u16 __le16_to_cpup(const __le16 *p)
{
 return ( __u16)*p;
}
static inline __attribute__((no_instrument_function)) __be64 __cpu_to_be64p(const __u64 *p)
{
 return ( __be64)__swab64p(p);
}
static inline __attribute__((no_instrument_function)) __u64 __be64_to_cpup(const __be64 *p)
{
 return __swab64p((__u64 *)p);
}
static inline __attribute__((no_instrument_function)) __be32 __cpu_to_be32p(const __u32 *p)
{
 return ( __be32)__swab32p(p);
}
static inline __attribute__((no_instrument_function)) __u32 __be32_to_cpup(const __be32 *p)
{
 return __swab32p((__u32 *)p);
}
static inline __attribute__((no_instrument_function)) __be16 __cpu_to_be16p(const __u16 *p)
{
 return ( __be16)__swab16p(p);
}
static inline __attribute__((no_instrument_function)) __u16 __be16_to_cpup(const __be16 *p)
{
 return __swab16p((__u16 *)p);
}
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/byteorder/little_endian.h" 2

# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/byteorder/generic.h" 1
# 143 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/byteorder/generic.h"
static inline __attribute__((no_instrument_function)) void le16_add_cpu(__le16 *var, u16 val)
{
 *var = (( __le16)(__u16)((( __u16)(__le16)(*var)) + val));
}

static inline __attribute__((no_instrument_function)) void le32_add_cpu(__le32 *var, u32 val)
{
 *var = (( __le32)(__u32)((( __u32)(__le32)(*var)) + val));
}

static inline __attribute__((no_instrument_function)) void le64_add_cpu(__le64 *var, u64 val)
{
 *var = (( __le64)(__u64)((( __u64)(__le64)(*var)) + val));
}

static inline __attribute__((no_instrument_function)) void be16_add_cpu(__be16 *var, u16 val)
{
 *var = (( __be16)(__builtin_constant_p((__u16)(((__builtin_constant_p((__u16)(( __u16)(__be16)(*var))) ? ((__u16)( (((__u16)(( __u16)(__be16)(*var)) & (__u16)0x00ffU) << 8) | (((__u16)(( __u16)(__be16)(*var)) & (__u16)0xff00U) >> 8))) : __fswab16(( __u16)(__be16)(*var))) + val))) ? ((__u16)( (((__u16)(((__builtin_constant_p((__u16)(( __u16)(__be16)(*var))) ? ((__u16)( (((__u16)(( __u16)(__be16)(*var)) & (__u16)0x00ffU) << 8) | (((__u16)(( __u16)(__be16)(*var)) & (__u16)0xff00U) >> 8))) : __fswab16(( __u16)(__be16)(*var))) + val)) & (__u16)0x00ffU) << 8) | (((__u16)(((__builtin_constant_p((__u16)(( __u16)(__be16)(*var))) ? ((__u16)( (((__u16)(( __u16)(__be16)(*var)) & (__u16)0x00ffU) << 8) | (((__u16)(( __u16)(__be16)(*var)) & (__u16)0xff00U) >> 8))) : __fswab16(( __u16)(__be16)(*var))) + val)) & (__u16)0xff00U) >> 8))) : __fswab16(((__builtin_constant_p((__u16)(( __u16)(__be16)(*var))) ? ((__u16)( (((__u16)(( __u16)(__be16)(*var)) & (__u16)0x00ffU) << 8) | (((__u16)(( __u16)(__be16)(*var)) & (__u16)0xff00U) >> 8))) : __fswab16(( __u16)(__be16)(*var))) + val))));
}

static inline __attribute__((no_instrument_function)) void be32_add_cpu(__be32 *var, u32 val)
{
 *var = (( __be32)(__builtin_constant_p((__u32)(((__builtin_constant_p((__u32)(( __u32)(__be32)(*var))) ? ((__u32)( (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x000000ffUL) << 24) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x0000ff00UL) << 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x00ff0000UL) >> 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0xff000000UL) >> 24))) : __fswab32(( __u32)(__be32)(*var))) + val))) ? ((__u32)( (((__u32)(((__builtin_constant_p((__u32)(( __u32)(__be32)(*var))) ? ((__u32)( (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x000000ffUL) << 24) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x0000ff00UL) << 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x00ff0000UL) >> 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0xff000000UL) >> 24))) : __fswab32(( __u32)(__be32)(*var))) + val)) & (__u32)0x000000ffUL) << 24) | (((__u32)(((__builtin_constant_p((__u32)(( __u32)(__be32)(*var))) ? ((__u32)( (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x000000ffUL) << 24) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x0000ff00UL) << 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x00ff0000UL) >> 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0xff000000UL) >> 24))) : __fswab32(( __u32)(__be32)(*var))) + val)) & (__u32)0x0000ff00UL) << 8) | (((__u32)(((__builtin_constant_p((__u32)(( __u32)(__be32)(*var))) ? ((__u32)( (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x000000ffUL) << 24) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x0000ff00UL) << 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x00ff0000UL) >> 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0xff000000UL) >> 24))) : __fswab32(( __u32)(__be32)(*var))) + val)) & (__u32)0x00ff0000UL) >> 8) | (((__u32)(((__builtin_constant_p((__u32)(( __u32)(__be32)(*var))) ? ((__u32)( (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x000000ffUL) << 24) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x0000ff00UL) << 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x00ff0000UL) >> 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0xff000000UL) >> 24))) : __fswab32(( __u32)(__be32)(*var))) + val)) & (__u32)0xff000000UL) >> 24))) : __fswab32(((__builtin_constant_p((__u32)(( __u32)(__be32)(*var))) ? ((__u32)( (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x000000ffUL) << 24) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x0000ff00UL) << 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x00ff0000UL) >> 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0xff000000UL) >> 24))) : __fswab32(( __u32)(__be32)(*var))) + val))));
}

static inline __attribute__((no_instrument_function)) void be64_add_cpu(__be64 *var, u64 val)
{
 *var = (( __be64)(__builtin_constant_p((__u64)(((__builtin_constant_p((__u64)(( __u64)(__be64)(*var))) ? ((__u64)( (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(( __u64)(__be64)(*var))) + val))) ? ((__u64)( (((__u64)(((__builtin_constant_p((__u64)(( __u64)(__be64)(*var))) ? ((__u64)( (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(( __u64)(__be64)(*var))) + val)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(((__builtin_constant_p((__u64)(( __u64)(__be64)(*var))) ? ((__u64)( (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(( __u64)(__be64)(*var))) + val)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(((__builtin_constant_p((__u64)(( __u64)(__be64)(*var))) ? ((__u64)( (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(( __u64)(__be64)(*var))) + val)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(((__builtin_constant_p((__u64)(( __u64)(__be64)(*var))) ? ((__u64)( (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(( __u64)(__be64)(*var))) + val)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(((__builtin_constant_p((__u64)(( __u64)(__be64)(*var))) ? ((__u64)( (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(( __u64)(__be64)(*var))) + val)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(((__builtin_constant_p((__u64)(( __u64)(__be64)(*var))) ? ((__u64)( (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(( __u64)(__be64)(*var))) + val)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(((__builtin_constant_p((__u64)(( __u64)(__be64)(*var))) ? ((__u64)( (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(( __u64)(__be64)(*var))) + val)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(((__builtin_constant_p((__u64)(( __u64)(__be64)(*var))) ? ((__u64)( (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(( __u64)(__be64)(*var))) + val)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(((__builtin_constant_p((__u64)(( __u64)(__be64)(*var))) ? ((__u64)( (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(( __u64)(__be64)(*var))) + val))));
}
# 7 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/byteorder/little_endian.h" 2
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/byteorder.h" 2
# 6 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/bitops/le.h" 2





static inline __attribute__((no_instrument_function)) unsigned long find_next_zero_bit_le(const void *addr,
  unsigned long size, unsigned long offset)
{
 return find_next_zero_bit(addr, size, offset);
}

static inline __attribute__((no_instrument_function)) unsigned long find_next_bit_le(const void *addr,
  unsigned long size, unsigned long offset)
{
 return find_next_bit(addr, size, offset);
}

static inline __attribute__((no_instrument_function)) unsigned long find_first_zero_bit_le(const void *addr,
  unsigned long size)
{
 return find_first_zero_bit(addr, size);
}
# 52 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/bitops/le.h"
static inline __attribute__((no_instrument_function)) int test_bit_le(int nr, const void *addr)
{
 return (__builtin_constant_p((nr ^ 0)) ? constant_test_bit((nr ^ 0), (addr)) : variable_test_bit((nr ^ 0), (addr)));
}

static inline __attribute__((no_instrument_function)) void set_bit_le(int nr, void *addr)
{
 set_bit(nr ^ 0, addr);
}

static inline __attribute__((no_instrument_function)) void clear_bit_le(int nr, void *addr)
{
 clear_bit(nr ^ 0, addr);
}

static inline __attribute__((no_instrument_function)) void __set_bit_le(int nr, void *addr)
{
 __set_bit(nr ^ 0, addr);
}

static inline __attribute__((no_instrument_function)) void __clear_bit_le(int nr, void *addr)
{
 __clear_bit(nr ^ 0, addr);
}

static inline __attribute__((no_instrument_function)) int test_and_set_bit_le(int nr, void *addr)
{
 return test_and_set_bit(nr ^ 0, addr);
}

static inline __attribute__((no_instrument_function)) int test_and_clear_bit_le(int nr, void *addr)
{
 return test_and_clear_bit(nr ^ 0, addr);
}

static inline __attribute__((no_instrument_function)) int __test_and_set_bit_le(int nr, void *addr)
{
 return __test_and_set_bit(nr ^ 0, addr);
}

static inline __attribute__((no_instrument_function)) int __test_and_clear_bit_le(int nr, void *addr)
{
 return __test_and_clear_bit(nr ^ 0, addr);
}
# 505 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h" 2

# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/bitops/ext2-atomic-setbit.h" 1
# 507 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bitops.h" 2
# 37 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/bitops.h" 2
# 60 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/bitops.h"
static __inline__ __attribute__((no_instrument_function)) int get_bitmask_order(unsigned int count)
{
 int order;

 order = fls(count);
 return order;
}

static __inline__ __attribute__((no_instrument_function)) int get_count_order(unsigned int count)
{
 int order;

 order = fls(count) - 1;
 if (count & (count - 1))
  order++;
 return order;
}

static inline __attribute__((no_instrument_function)) unsigned long hweight_long(unsigned long w)
{
 return sizeof(w) == 4 ? (__builtin_constant_p(w) ? ((((unsigned int) ((!!((w) & (1ULL << 0))) + (!!((w) & (1ULL << 1))) + (!!((w) & (1ULL << 2))) + (!!((w) & (1ULL << 3))) + (!!((w) & (1ULL << 4))) + (!!((w) & (1ULL << 5))) + (!!((w) & (1ULL << 6))) + (!!((w) & (1ULL << 7))))) + ((unsigned int) ((!!(((w) >> 8) & (1ULL << 0))) + (!!(((w) >> 8) & (1ULL << 1))) + (!!(((w) >> 8) & (1ULL << 2))) + (!!(((w) >> 8) & (1ULL << 3))) + (!!(((w) >> 8) & (1ULL << 4))) + (!!(((w) >> 8) & (1ULL << 5))) + (!!(((w) >> 8) & (1ULL << 6))) + (!!(((w) >> 8) & (1ULL << 7)))))) + (((unsigned int) ((!!(((w) >> 16) & (1ULL << 0))) + (!!(((w) >> 16) & (1ULL << 1))) + (!!(((w) >> 16) & (1ULL << 2))) + (!!(((w) >> 16) & (1ULL << 3))) + (!!(((w) >> 16) & (1ULL << 4))) + (!!(((w) >> 16) & (1ULL << 5))) + (!!(((w) >> 16) & (1ULL << 6))) + (!!(((w) >> 16) & (1ULL << 7))))) + ((unsigned int) ((!!((((w) >> 16) >> 8) & (1ULL << 0))) + (!!((((w) >> 16) >> 8) & (1ULL << 1))) + (!!((((w) >> 16) >> 8) & (1ULL << 2))) + (!!((((w) >> 16) >> 8) & (1ULL << 3))) + (!!((((w) >> 16) >> 8) & (1ULL << 4))) + (!!((((w) >> 16) >> 8) & (1ULL << 5))) + (!!((((w) >> 16) >> 8) & (1ULL << 6))) + (!!((((w) >> 16) >> 8) & (1ULL << 7))))))) : __arch_hweight32(w)) : (__builtin_constant_p(w) ? (((((unsigned int) ((!!((w) & (1ULL << 0))) + (!!((w) & (1ULL << 1))) + (!!((w) & (1ULL << 2))) + (!!((w) & (1ULL << 3))) + (!!((w) & (1ULL << 4))) + (!!((w) & (1ULL << 5))) + (!!((w) & (1ULL << 6))) + (!!((w) & (1ULL << 7))))) + ((unsigned int) ((!!(((w) >> 8) & (1ULL << 0))) + (!!(((w) >> 8) & (1ULL << 1))) + (!!(((w) >> 8) & (1ULL << 2))) + (!!(((w) >> 8) & (1ULL << 3))) + (!!(((w) >> 8) & (1ULL << 4))) + (!!(((w) >> 8) & (1ULL << 5))) + (!!(((w) >> 8) & (1ULL << 6))) + (!!(((w) >> 8) & (1ULL << 7)))))) + (((unsigned int) ((!!(((w) >> 16) & (1ULL << 0))) + (!!(((w) >> 16) & (1ULL << 1))) + (!!(((w) >> 16) & (1ULL << 2))) + (!!(((w) >> 16) & (1ULL << 3))) + (!!(((w) >> 16) & (1ULL << 4))) + (!!(((w) >> 16) & (1ULL << 5))) + (!!(((w) >> 16) & (1ULL << 6))) + (!!(((w) >> 16) & (1ULL << 7))))) + ((unsigned int) ((!!((((w) >> 16) >> 8) & (1ULL << 0))) + (!!((((w) >> 16) >> 8) & (1ULL << 1))) + (!!((((w) >> 16) >> 8) & (1ULL << 2))) + (!!((((w) >> 16) >> 8) & (1ULL << 3))) + (!!((((w) >> 16) >> 8) & (1ULL << 4))) + (!!((((w) >> 16) >> 8) & (1ULL << 5))) + (!!((((w) >> 16) >> 8) & (1ULL << 6))) + (!!((((w) >> 16) >> 8) & (1ULL << 7))))))) + ((((unsigned int) ((!!(((w) >> 32) & (1ULL << 0))) + (!!(((w) >> 32) & (1ULL << 1))) + (!!(((w) >> 32) & (1ULL << 2))) + (!!(((w) >> 32) & (1ULL << 3))) + (!!(((w) >> 32) & (1ULL << 4))) + (!!(((w) >> 32) & (1ULL << 5))) + (!!(((w) >> 32) & (1ULL << 6))) + (!!(((w) >> 32) & (1ULL << 7))))) + ((unsigned int) ((!!((((w) >> 32) >> 8) & (1ULL << 0))) + (!!((((w) >> 32) >> 8) & (1ULL << 1))) + (!!((((w) >> 32) >> 8) & (1ULL << 2))) + (!!((((w) >> 32) >> 8) & (1ULL << 3))) + (!!((((w) >> 32) >> 8) & (1ULL << 4))) + (!!((((w) >> 32) >> 8) & (1ULL << 5))) + (!!((((w) >> 32) >> 8) & (1ULL << 6))) + (!!((((w) >> 32) >> 8) & (1ULL << 7)))))) + (((unsigned int) ((!!((((w) >> 32) >> 16) & (1ULL << 0))) + (!!((((w) >> 32) >> 16) & (1ULL << 1))) + (!!((((w) >> 32) >> 16) & (1ULL << 2))) + (!!((((w) >> 32) >> 16) & (1ULL << 3))) + (!!((((w) >> 32) >> 16) & (1ULL << 4))) + (!!((((w) >> 32) >> 16) & (1ULL << 5))) + (!!((((w) >> 32) >> 16) & (1ULL << 6))) + (!!((((w) >> 32) >> 16) & (1ULL << 7))))) + ((unsigned int) ((!!(((((w) >> 32) >> 16) >> 8) & (1ULL << 0))) + (!!(((((w) >> 32) >> 16) >> 8) & (1ULL << 1))) + (!!(((((w) >> 32) >> 16) >> 8) & (1ULL << 2))) + (!!(((((w) >> 32) >> 16) >> 8) & (1ULL << 3))) + (!!(((((w) >> 32) >> 16) >> 8) & (1ULL << 4))) + (!!(((((w) >> 32) >> 16) >> 8) & (1ULL << 5))) + (!!(((((w) >> 32) >> 16) >> 8) & (1ULL << 6))) + (!!(((((w) >> 32) >> 16) >> 8) & (1ULL << 7)))))))) : __arch_hweight64(w));
}






static inline __attribute__((no_instrument_function)) __u64 rol64(__u64 word, unsigned int shift)
{
 return (word << shift) | (word >> (64 - shift));
}






static inline __attribute__((no_instrument_function)) __u64 ror64(__u64 word, unsigned int shift)
{
 return (word >> shift) | (word << (64 - shift));
}






static inline __attribute__((no_instrument_function)) __u32 rol32(__u32 word, unsigned int shift)
{
 return (word << shift) | (word >> (32 - shift));
}






static inline __attribute__((no_instrument_function)) __u32 ror32(__u32 word, unsigned int shift)
{
 return (word >> shift) | (word << (32 - shift));
}






static inline __attribute__((no_instrument_function)) __u16 rol16(__u16 word, unsigned int shift)
{
 return (word << shift) | (word >> (16 - shift));
}






static inline __attribute__((no_instrument_function)) __u16 ror16(__u16 word, unsigned int shift)
{
 return (word >> shift) | (word << (16 - shift));
}






static inline __attribute__((no_instrument_function)) __u8 rol8(__u8 word, unsigned int shift)
{
 return (word << shift) | (word >> (8 - shift));
}






static inline __attribute__((no_instrument_function)) __u8 ror8(__u8 word, unsigned int shift)
{
 return (word >> shift) | (word << (8 - shift));
}






static inline __attribute__((no_instrument_function)) __s32 sign_extend32(__u32 value, int index)
{
 __u8 shift = 31 - index;
 return (__s32)(value << shift) >> shift;
}

static inline __attribute__((no_instrument_function)) unsigned fls_long(unsigned long l)
{
 if (sizeof(l) == 4)
  return fls(l);
 return fls64(l);
}
# 189 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/bitops.h"
static inline __attribute__((no_instrument_function)) unsigned long __ffs64(u64 word)
{






 return __ffs((unsigned long)word);
}
# 225 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/bitops.h"
extern unsigned long find_last_bit(const unsigned long *addr,
       unsigned long size);
# 11 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/kernel.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/log2.h" 1
# 21 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/log2.h"
extern __attribute__((const, noreturn))
int ____ilog2_NaN(void);
# 31 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/log2.h"
static inline __attribute__((no_instrument_function)) __attribute__((const))
int __ilog2_u32(u32 n)
{
 return fls(n) - 1;
}



static inline __attribute__((no_instrument_function)) __attribute__((const))
int __ilog2_u64(u64 n)
{
 return fls64(n) - 1;
}







static inline __attribute__((no_instrument_function)) __attribute__((const))
bool is_power_of_2(unsigned long n)
{
 return (n != 0 && ((n & (n - 1)) == 0));
}




static inline __attribute__((no_instrument_function)) __attribute__((const))
unsigned long __roundup_pow_of_two(unsigned long n)
{
 return 1UL << fls_long(n - 1);
}




static inline __attribute__((no_instrument_function)) __attribute__((const))
unsigned long __rounddown_pow_of_two(unsigned long n)
{
 return 1UL << (fls_long(n) - 1);
}
# 12 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/kernel.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/typecheck.h" 1
# 13 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/kernel.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/printk.h" 1




# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/init.h" 1
# 135 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/init.h"
typedef int (*initcall_t)(void);
typedef void (*exitcall_t)(void);

extern initcall_t __con_initcall_start[], __con_initcall_end[];
extern initcall_t __security_initcall_start[], __security_initcall_end[];


typedef void (*ctor_fn_t)(void);


extern int do_one_initcall(initcall_t fn);
extern char __attribute__ ((__section__(".init.data"))) boot_command_line[];
extern char *saved_command_line;
extern unsigned int reset_devices;


void setup_arch(char **);
void prepare_namespace(void);
void __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) load_default_modules(void);
int __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) init_rootfs(void);

extern void (*late_time_init)(void);

extern bool initcall_debug;
# 243 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/init.h"
struct obs_kernel_param {
 const char *str;
 int (*setup_func)(char *);
 int early;
};
# 272 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/init.h"
void __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) parse_early_param(void);
void __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) parse_early_options(char *cmdline);
# 6 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/printk.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/kern_levels.h" 1
# 7 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/printk.h" 2

# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cache.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/kernel.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/sysinfo.h" 1






struct sysinfo {
 __kernel_long_t uptime;
 __kernel_ulong_t loads[3];
 __kernel_ulong_t totalram;
 __kernel_ulong_t freeram;
 __kernel_ulong_t sharedram;
 __kernel_ulong_t bufferram;
 __kernel_ulong_t totalswap;
 __kernel_ulong_t freeswap;
 __u16 procs;
 __u16 pad;
 __kernel_ulong_t totalhigh;
 __kernel_ulong_t freehigh;
 __u32 mem_unit;
 char _f[20-2*sizeof(__kernel_ulong_t)-sizeof(__u32)];
};
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/kernel.h" 2
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cache.h" 2
# 9 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/printk.h" 2

extern const char linux_banner[];
extern const char linux_proc_banner[];

static inline __attribute__((no_instrument_function)) int printk_get_level(const char *buffer)
{
 if (buffer[0] == '\001' && buffer[1]) {
  switch (buffer[1]) {
  case '0' ... '7':
  case 'd':
   return buffer[1];
  }
 }
 return 0;
}

static inline __attribute__((no_instrument_function)) const char *printk_skip_level(const char *buffer)
{
 if (printk_get_level(buffer))
  return buffer + 2;

 return buffer;
}
# 44 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/printk.h"
extern int console_printk[];






static inline __attribute__((no_instrument_function)) void console_silent(void)
{
 (console_printk[0]) = 0;
}

static inline __attribute__((no_instrument_function)) void console_verbose(void)
{
 if ((console_printk[0]))
  (console_printk[0]) = 15;
}

struct va_format {
 const char *fmt;
 va_list *va;
};
# 109 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/printk.h"
static inline __attribute__((no_instrument_function)) __attribute__((format(printf, 1, 2)))
int no_printk(const char *fmt, ...)
{
 return 0;
}


extern __attribute__((format(printf, 1, 2)))
void early_printk(const char *fmt, ...);





typedef int(*printk_func_t)(const char *fmt, va_list args);


 __attribute__((format(printf, 5, 0)))
int vprintk_emit(int facility, int level,
   const char *dict, size_t dictlen,
   const char *fmt, va_list args);

 __attribute__((format(printf, 1, 0)))
int vprintk(const char *fmt, va_list args);

 __attribute__((format(printf, 5, 6))) __attribute__((__cold__))
int printk_emit(int facility, int level,
  const char *dict, size_t dictlen,
  const char *fmt, ...);

 __attribute__((format(printf, 1, 2))) __attribute__((__cold__))
int printk(const char *fmt, ...);




__attribute__((format(printf, 1, 2))) __attribute__((__cold__)) int printk_deferred(const char *fmt, ...);






extern int __printk_ratelimit(const char *func);

extern bool printk_timed_ratelimit(unsigned long *caller_jiffies,
       unsigned int interval_msec);

extern int printk_delay_msec;
extern int dmesg_restrict;
extern int kptr_restrict;

extern void wake_up_klogd(void);

char *log_buf_addr_get(void);
u32 log_buf_len_get(void);
void log_buf_kexec_setup(void);
void __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) setup_log_buf(int early);
void dump_stack_set_arch_desc(const char *fmt, ...);
void dump_stack_print_info(const char *log_lvl);
void show_regs_print_info(const char *log_lvl);
# 231 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/printk.h"
extern void dump_stack(void) __attribute__((__cold__));
# 270 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/printk.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/dynamic_debug.h" 1
# 9 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/dynamic_debug.h"
struct _ddebug {




 const char *modname;
 const char *function;
 const char *filename;
 const char *format;
 unsigned int lineno:18;
# 35 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/dynamic_debug.h"
 unsigned int flags:8;
} __attribute__((aligned(8)));


int ddebug_add_module(struct _ddebug *tab, unsigned int n,
    const char *modname);


extern int ddebug_remove_module(const char *mod_name);
extern __attribute__((format(printf, 2, 3)))
void __dynamic_pr_debug(struct _ddebug *descriptor, const char *fmt, ...);

extern int ddebug_dyndbg_module_param_cb(char *param, char *val,
     const char *modname);

struct device;

extern __attribute__((format(printf, 3, 4)))
void __dynamic_dev_dbg(struct _ddebug *descriptor, const struct device *dev,
         const char *fmt, ...);

struct net_device;

extern __attribute__((format(printf, 3, 4)))
void __dynamic_netdev_dbg(struct _ddebug *descriptor,
     const struct net_device *dev,
     const char *fmt, ...);
# 271 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/printk.h" 2
# 413 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/printk.h"
extern const struct file_operations kmsg_fops;

enum {
 DUMP_PREFIX_NONE,
 DUMP_PREFIX_ADDRESS,
 DUMP_PREFIX_OFFSET
};
extern void hex_dump_to_buffer(const void *buf, size_t len,
          int rowsize, int groupsize,
          char *linebuf, size_t linebuflen, bool ascii);

extern void print_hex_dump(const char *level, const char *prefix_str,
      int prefix_type, int rowsize, int groupsize,
      const void *buf, size_t len, bool ascii);
# 14 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/kernel.h" 2
# 165 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/kernel.h"
struct completion;
struct pt_regs;
struct user;


extern int _cond_resched(void);
# 193 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/kernel.h"
  static inline __attribute__((no_instrument_function)) void ___might_sleep(const char *file, int line,
       int preempt_offset) { }
  static inline __attribute__((no_instrument_function)) void __might_sleep(const char *file, int line,
       int preempt_offset) { }
# 240 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/kernel.h"
static inline __attribute__((no_instrument_function)) u32 reciprocal_scale(u32 val, u32 ep_ro)
{
 return (u32)(((u64) val * ep_ro) >> 32);
}





static inline __attribute__((no_instrument_function)) void might_fault(void) { }


extern struct atomic_notifier_head panic_notifier_list;
extern long (*panic_blink)(int state);
__attribute__((format(printf, 1, 2)))
void panic(const char *fmt, ...)
 __attribute__((noreturn)) __attribute__((__cold__));
extern void oops_enter(void);
extern void oops_exit(void);
void print_oops_end_marker(void);
extern int oops_may_print(void);
void do_exit(long error_code)
 __attribute__((noreturn));
void complete_and_exit(struct completion *, long)
 __attribute__((noreturn));


int _kstrtoul(const char *s, unsigned int base, unsigned long *res);
int _kstrtol(const char *s, unsigned int base, long *res);

int kstrtoull(const char *s, unsigned int base, unsigned long long *res);
int kstrtoll(const char *s, unsigned int base, long long *res);
# 289 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/kernel.h"
static inline __attribute__((no_instrument_function)) int kstrtoul(const char *s, unsigned int base, unsigned long *res)
{




 if (sizeof(unsigned long) == sizeof(unsigned long long) &&
     __alignof__(unsigned long) == __alignof__(unsigned long long))
  return kstrtoull(s, base, (unsigned long long *)res);
 else
  return _kstrtoul(s, base, res);
}
# 318 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/kernel.h"
static inline __attribute__((no_instrument_function)) int kstrtol(const char *s, unsigned int base, long *res)
{




 if (sizeof(long) == sizeof(long long) &&
     __alignof__(long) == __alignof__(long long))
  return kstrtoll(s, base, (long long *)res);
 else
  return _kstrtol(s, base, res);
}

int kstrtouint(const char *s, unsigned int base, unsigned int *res);
int kstrtoint(const char *s, unsigned int base, int *res);

static inline __attribute__((no_instrument_function)) int kstrtou64(const char *s, unsigned int base, u64 *res)
{
 return kstrtoull(s, base, res);
}

static inline __attribute__((no_instrument_function)) int kstrtos64(const char *s, unsigned int base, s64 *res)
{
 return kstrtoll(s, base, res);
}

static inline __attribute__((no_instrument_function)) int kstrtou32(const char *s, unsigned int base, u32 *res)
{
 return kstrtouint(s, base, res);
}

static inline __attribute__((no_instrument_function)) int kstrtos32(const char *s, unsigned int base, s32 *res)
{
 return kstrtoint(s, base, res);
}

int kstrtou16(const char *s, unsigned int base, u16 *res);
int kstrtos16(const char *s, unsigned int base, s16 *res);
int kstrtou8(const char *s, unsigned int base, u8 *res);
int kstrtos8(const char *s, unsigned int base, s8 *res);

int kstrtoull_from_user(const char *s, size_t count, unsigned int base, unsigned long long *res);
int kstrtoll_from_user(const char *s, size_t count, unsigned int base, long long *res);
int kstrtoul_from_user(const char *s, size_t count, unsigned int base, unsigned long *res);
int kstrtol_from_user(const char *s, size_t count, unsigned int base, long *res);
int kstrtouint_from_user(const char *s, size_t count, unsigned int base, unsigned int *res);
int kstrtoint_from_user(const char *s, size_t count, unsigned int base, int *res);
int kstrtou16_from_user(const char *s, size_t count, unsigned int base, u16 *res);
int kstrtos16_from_user(const char *s, size_t count, unsigned int base, s16 *res);
int kstrtou8_from_user(const char *s, size_t count, unsigned int base, u8 *res);
int kstrtos8_from_user(const char *s, size_t count, unsigned int base, s8 *res);

static inline __attribute__((no_instrument_function)) int kstrtou64_from_user(const char *s, size_t count, unsigned int base, u64 *res)
{
 return kstrtoull_from_user(s, count, base, res);
}

static inline __attribute__((no_instrument_function)) int kstrtos64_from_user(const char *s, size_t count, unsigned int base, s64 *res)
{
 return kstrtoll_from_user(s, count, base, res);
}

static inline __attribute__((no_instrument_function)) int kstrtou32_from_user(const char *s, size_t count, unsigned int base, u32 *res)
{
 return kstrtouint_from_user(s, count, base, res);
}

static inline __attribute__((no_instrument_function)) int kstrtos32_from_user(const char *s, size_t count, unsigned int base, s32 *res)
{
 return kstrtoint_from_user(s, count, base, res);
}



extern unsigned long simple_strtoul(const char *,char **,unsigned int);
extern long simple_strtol(const char *,char **,unsigned int);
extern unsigned long long simple_strtoull(const char *,char **,unsigned int);
extern long long simple_strtoll(const char *,char **,unsigned int);

extern int num_to_str(char *buf, int size, unsigned long long num);



extern __attribute__((format(printf, 2, 3))) int sprintf(char *buf, const char * fmt, ...);
extern __attribute__((format(printf, 2, 0))) int vsprintf(char *buf, const char *, va_list);
extern __attribute__((format(printf, 3, 4)))
int snprintf(char *buf, size_t size, const char *fmt, ...);
extern __attribute__((format(printf, 3, 0)))
int vsnprintf(char *buf, size_t size, const char *fmt, va_list args);
extern __attribute__((format(printf, 3, 4)))
int scnprintf(char *buf, size_t size, const char *fmt, ...);
extern __attribute__((format(printf, 3, 0)))
int vscnprintf(char *buf, size_t size, const char *fmt, va_list args);
extern __attribute__((format(printf, 2, 3)))
char *kasprintf(gfp_t gfp, const char *fmt, ...);
extern char *kvasprintf(gfp_t gfp, const char *fmt, va_list args);

extern __attribute__((format(scanf, 2, 3)))
int sscanf(const char *, const char *, ...);
extern __attribute__((format(scanf, 2, 0)))
int vsscanf(const char *, const char *, va_list);

extern int get_option(char **str, int *pint);
extern char *get_options(const char *str, int nints, int *ints);
extern unsigned long long memparse(const char *ptr, char **retptr);
extern bool parse_option_str(const char *str, const char *option);

extern int core_kernel_text(unsigned long addr);
extern int core_kernel_data(unsigned long addr);
extern int __kernel_text_address(unsigned long addr);
extern int kernel_text_address(unsigned long addr);
extern int func_ptr_is_kernel_text(void *ptr);

unsigned long int_sqrt(unsigned long);

extern void bust_spinlocks(int yes);
extern int oops_in_progress;
extern int panic_timeout;
extern int panic_on_oops;
extern int panic_on_unrecovered_nmi;
extern int panic_on_io_nmi;
extern int panic_on_warn;
extern int sysctl_panic_on_stackoverflow;




static inline __attribute__((no_instrument_function)) void set_arch_panic_timeout(int timeout, int arch_default_timeout)
{
 if (panic_timeout == arch_default_timeout)
  panic_timeout = timeout;
}
extern const char *print_tainted(void);
enum lockdep_ok {
 LOCKDEP_STILL_OK,
 LOCKDEP_NOW_UNRELIABLE
};
extern void add_taint(unsigned flag, enum lockdep_ok);
extern int test_taint(unsigned flag);
extern unsigned long get_taint(void);
extern int root_mountflags;

extern bool early_boot_irqs_disabled;


extern enum system_states {
 SYSTEM_BOOTING,
 SYSTEM_RUNNING,
 SYSTEM_HALT,
 SYSTEM_POWER_OFF,
 SYSTEM_RESTART,
} system_state;
# 487 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/kernel.h"
extern const char hex_asc[];



static inline __attribute__((no_instrument_function)) char *hex_byte_pack(char *buf, u8 byte)
{
 *buf++ = hex_asc[((byte) & 0xf0) >> 4];
 *buf++ = hex_asc[((byte) & 0x0f)];
 return buf;
}

extern const char hex_asc_upper[];



static inline __attribute__((no_instrument_function)) char *hex_byte_pack_upper(char *buf, u8 byte)
{
 *buf++ = hex_asc_upper[((byte) & 0xf0) >> 4];
 *buf++ = hex_asc_upper[((byte) & 0x0f)];
 return buf;
}

extern int hex_to_bin(char ch);
extern int hex2bin(u8 *dst, const char *src, size_t count);
extern char *bin2hex(char *dst, const void *src, size_t count);

bool mac_pton(const char *s, u8 *mac);
# 536 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/kernel.h"
void tracing_off_permanent(void);




enum ftrace_dump_mode {
 DUMP_NONE,
 DUMP_ALL,
 DUMP_ORIG,
};


void tracing_on(void);
void tracing_off(void);
int tracing_is_on(void);
void tracing_snapshot(void);
void tracing_snapshot_alloc(void);

extern void tracing_start(void);
extern void tracing_stop(void);

static inline __attribute__((no_instrument_function)) __attribute__((format(printf, 1, 2)))
void ____trace_printk_check_format(const char *fmt, ...)
{
}
# 620 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/kernel.h"
extern __attribute__((format(printf, 2, 3)))
int __trace_bprintk(unsigned long ip, const char *fmt, ...);

extern __attribute__((format(printf, 2, 3)))
int __trace_printk(unsigned long ip, const char *fmt, ...);
# 661 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/kernel.h"
extern int __trace_bputs(unsigned long ip, const char *str);
extern int __trace_puts(unsigned long ip, const char *str, int size);

extern void trace_dump_stack(int skip);
# 683 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/kernel.h"
extern int
__ftrace_vbprintk(unsigned long ip, const char *fmt, va_list ap);

extern int
__ftrace_vprintk(unsigned long ip, const char *fmt, va_list ap);

extern void ftrace_dump(enum ftrace_dump_mode oops_dump_mode);
# 9 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/list.h" 2
# 25 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/list.h"
static inline __attribute__((no_instrument_function)) void INIT_LIST_HEAD(struct list_head *list)
{
 list->next = list;
 list->prev = list;
}
# 38 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/list.h"
static inline __attribute__((no_instrument_function)) void __list_add(struct list_head *new,
         struct list_head *prev,
         struct list_head *next)
{
 next->prev = new;
 new->next = next;
 new->prev = prev;
 prev->next = new;
}
# 61 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/list.h"
static inline __attribute__((no_instrument_function)) void list_add(struct list_head *new, struct list_head *head)
{
 __list_add(new, head, head->next);
}
# 75 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/list.h"
static inline __attribute__((no_instrument_function)) void list_add_tail(struct list_head *new, struct list_head *head)
{
 __list_add(new, head->prev, head);
}
# 87 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/list.h"
static inline __attribute__((no_instrument_function)) void __list_del(struct list_head * prev, struct list_head * next)
{
 next->prev = prev;
 prev->next = next;
}
# 100 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/list.h"
static inline __attribute__((no_instrument_function)) void __list_del_entry(struct list_head *entry)
{
 __list_del(entry->prev, entry->next);
}

static inline __attribute__((no_instrument_function)) void list_del(struct list_head *entry)
{
 __list_del(entry->prev, entry->next);
 entry->next = ((void *) 0x00100100 + (0xdead000000000000UL));
 entry->prev = ((void *) 0x00200200 + (0xdead000000000000UL));
}
# 123 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/list.h"
static inline __attribute__((no_instrument_function)) void list_replace(struct list_head *old,
    struct list_head *new)
{
 new->next = old->next;
 new->next->prev = new;
 new->prev = old->prev;
 new->prev->next = new;
}

static inline __attribute__((no_instrument_function)) void list_replace_init(struct list_head *old,
     struct list_head *new)
{
 list_replace(old, new);
 INIT_LIST_HEAD(old);
}





static inline __attribute__((no_instrument_function)) void list_del_init(struct list_head *entry)
{
 __list_del_entry(entry);
 INIT_LIST_HEAD(entry);
}






static inline __attribute__((no_instrument_function)) void list_move(struct list_head *list, struct list_head *head)
{
 __list_del_entry(list);
 list_add(list, head);
}






static inline __attribute__((no_instrument_function)) void list_move_tail(struct list_head *list,
      struct list_head *head)
{
 __list_del_entry(list);
 list_add_tail(list, head);
}






static inline __attribute__((no_instrument_function)) int list_is_last(const struct list_head *list,
    const struct list_head *head)
{
 return list->next == head;
}





static inline __attribute__((no_instrument_function)) int list_empty(const struct list_head *head)
{
 return head->next == head;
}
# 205 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/list.h"
static inline __attribute__((no_instrument_function)) int list_empty_careful(const struct list_head *head)
{
 struct list_head *next = head->next;
 return (next == head) && (next == head->prev);
}





static inline __attribute__((no_instrument_function)) void list_rotate_left(struct list_head *head)
{
 struct list_head *first;

 if (!list_empty(head)) {
  first = head->next;
  list_move_tail(first, head);
 }
}





static inline __attribute__((no_instrument_function)) int list_is_singular(const struct list_head *head)
{
 return !list_empty(head) && (head->next == head->prev);
}

static inline __attribute__((no_instrument_function)) void __list_cut_position(struct list_head *list,
  struct list_head *head, struct list_head *entry)
{
 struct list_head *new_first = entry->next;
 list->next = head->next;
 list->next->prev = list;
 list->prev = entry;
 entry->next = list;
 head->next = new_first;
 new_first->prev = head;
}
# 260 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/list.h"
static inline __attribute__((no_instrument_function)) void list_cut_position(struct list_head *list,
  struct list_head *head, struct list_head *entry)
{
 if (list_empty(head))
  return;
 if (list_is_singular(head) &&
  (head->next != entry && head != entry))
  return;
 if (entry == head)
  INIT_LIST_HEAD(list);
 else
  __list_cut_position(list, head, entry);
}

static inline __attribute__((no_instrument_function)) void __list_splice(const struct list_head *list,
     struct list_head *prev,
     struct list_head *next)
{
 struct list_head *first = list->next;
 struct list_head *last = list->prev;

 first->prev = prev;
 prev->next = first;

 last->next = next;
 next->prev = last;
}






static inline __attribute__((no_instrument_function)) void list_splice(const struct list_head *list,
    struct list_head *head)
{
 if (!list_empty(list))
  __list_splice(list, head, head->next);
}






static inline __attribute__((no_instrument_function)) void list_splice_tail(struct list_head *list,
    struct list_head *head)
{
 if (!list_empty(list))
  __list_splice(list, head->prev, head);
}
# 319 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/list.h"
static inline __attribute__((no_instrument_function)) void list_splice_init(struct list_head *list,
        struct list_head *head)
{
 if (!list_empty(list)) {
  __list_splice(list, head, head->next);
  INIT_LIST_HEAD(list);
 }
}
# 336 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/list.h"
static inline __attribute__((no_instrument_function)) void list_splice_tail_init(struct list_head *list,
      struct list_head *head)
{
 if (!list_empty(list)) {
  __list_splice(list, head->prev, head);
  INIT_LIST_HEAD(list);
 }
}
# 598 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/list.h"
static inline __attribute__((no_instrument_function)) void INIT_HLIST_NODE(struct hlist_node *h)
{
 h->next = ((void *)0);
 h->pprev = ((void *)0);
}

static inline __attribute__((no_instrument_function)) int hlist_unhashed(const struct hlist_node *h)
{
 return !h->pprev;
}

static inline __attribute__((no_instrument_function)) int hlist_empty(const struct hlist_head *h)
{
 return !h->first;
}

static inline __attribute__((no_instrument_function)) void __hlist_del(struct hlist_node *n)
{
 struct hlist_node *next = n->next;
 struct hlist_node **pprev = n->pprev;
 *pprev = next;
 if (next)
  next->pprev = pprev;
}

static inline __attribute__((no_instrument_function)) void hlist_del(struct hlist_node *n)
{
 __hlist_del(n);
 n->next = ((void *) 0x00100100 + (0xdead000000000000UL));
 n->pprev = ((void *) 0x00200200 + (0xdead000000000000UL));
}

static inline __attribute__((no_instrument_function)) void hlist_del_init(struct hlist_node *n)
{
 if (!hlist_unhashed(n)) {
  __hlist_del(n);
  INIT_HLIST_NODE(n);
 }
}

static inline __attribute__((no_instrument_function)) void hlist_add_head(struct hlist_node *n, struct hlist_head *h)
{
 struct hlist_node *first = h->first;
 n->next = first;
 if (first)
  first->pprev = &n->next;
 h->first = n;
 n->pprev = &h->first;
}


static inline __attribute__((no_instrument_function)) void hlist_add_before(struct hlist_node *n,
     struct hlist_node *next)
{
 n->pprev = next->pprev;
 n->next = next;
 next->pprev = &n->next;
 *(n->pprev) = n;
}

static inline __attribute__((no_instrument_function)) void hlist_add_behind(struct hlist_node *n,
        struct hlist_node *prev)
{
 n->next = prev->next;
 prev->next = n;
 n->pprev = &prev->next;

 if (n->next)
  n->next->pprev = &n->next;
}


static inline __attribute__((no_instrument_function)) void hlist_add_fake(struct hlist_node *n)
{
 n->pprev = &n->next;
}





static inline __attribute__((no_instrument_function)) void hlist_move_list(struct hlist_head *old,
       struct hlist_head *new)
{
 new->first = old->first;
 if (new->first)
  new->first->pprev = &new->first;
 old->first = ((void *)0);
}
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/signal.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/bug.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bug.h" 1
# 35 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bug.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/bug.h" 1
# 18 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/bug.h"
struct bug_entry {



 signed int bug_addr_disp;





 signed int file_disp;

 unsigned short line;

 unsigned short flags;
};
# 65 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/bug.h"
extern __attribute__((format(printf, 3, 4)))
void warn_slowpath_fmt(const char *file, const int line,
         const char *fmt, ...);
extern __attribute__((format(printf, 4, 5)))
void warn_slowpath_fmt_taint(const char *file, const int line, unsigned taint,
        const char *fmt, ...);
extern void warn_slowpath_null(const char *file, const int line);
# 36 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/bug.h" 2
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/bug.h" 2


enum bug_trap_type {
 BUG_TRAP_TYPE_NONE = 0,
 BUG_TRAP_TYPE_WARN = 1,
 BUG_TRAP_TYPE_BUG = 2,
};

struct pt_regs;
# 91 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/bug.h"
static inline __attribute__((no_instrument_function)) int is_warning_bug(const struct bug_entry *bug)
{
 return bug->flags & (1 << 0);
}

const struct bug_entry *find_bug(unsigned long bugaddr);

enum bug_trap_type report_bug(unsigned long bug_addr, struct pt_regs *regs);


int is_valid_bugaddr(unsigned long addr);
# 6 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/signal.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/signal.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/signal.h" 1
# 20 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/signal.h"
typedef unsigned long old_sigset_t;

typedef struct {
 unsigned long sig[(64 / 64)];
} sigset_t;






# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/signal.h" 1





# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/time.h" 1




# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/seqlock.h" 1
# 35 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/seqlock.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock.h" 1
# 50 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/preempt.h" 1
# 18 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/preempt.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/preempt.h" 1




# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/percpu.h" 1
# 87 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/percpu.h"
extern void __bad_percpu_size(void);
# 520 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/percpu.h"
static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) int x86_this_cpu_constant_test_bit(unsigned int nr,
                        const unsigned long *addr)
{
 unsigned long *a = (unsigned long *)addr + nr / 64;


 return ((1UL << (nr % 64)) & ({ typeof(*a) pfo_ret__; switch (sizeof(*a)) { case 1: asm("mov" "b ""%%""gs"":" "%" "1"",%0" : "=q" (pfo_ret__) : "m" (*a)); break; case 2: asm("mov" "w ""%%""gs"":" "%" "1"",%0" : "=r" (pfo_ret__) : "m" (*a)); break; case 4: asm("mov" "l ""%%""gs"":" "%" "1"",%0" : "=r" (pfo_ret__) : "m" (*a)); break; case 8: asm("mov" "q ""%%""gs"":" "%" "1"",%0" : "=r" (pfo_ret__) : "m" (*a)); break; default: __bad_percpu_size(); } pfo_ret__; })) != 0;



}

static inline __attribute__((no_instrument_function)) int x86_this_cpu_variable_test_bit(int nr,
                        const unsigned long *addr)
{
 int oldbit;

 asm volatile("bt ""%%""gs"":" "%" "2"",%1\n\t"
   "sbb %0,%0"
   : "=r" (oldbit)
   : "m" (*(unsigned long *)addr), "Ir" (nr));

 return oldbit;
}







# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/percpu.h" 1




# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/threads.h" 1
# 6 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/percpu.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/percpu-defs.h" 1
# 295 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/percpu-defs.h"
extern void __bad_size_call_parameter(void);




static inline __attribute__((no_instrument_function)) void __this_cpu_preempt_check(const char *op) { }
# 7 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/percpu.h" 2
# 18 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/percpu.h"
extern unsigned long __per_cpu_offset[256];
# 47 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/percpu.h"
extern void setup_per_cpu_areas(void);
# 552 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/percpu.h" 2


extern __attribute__((section(".data..percpu" "..read_mostly"))) __typeof__(unsigned long) this_cpu_off;
# 6 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/preempt.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/thread_info.h" 1
# 13 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/thread_info.h"
struct timespec;
struct compat_timespec;




struct restart_block {
 long (*fn)(struct restart_block *);
 union {

  struct {
   u32 *uaddr;
   u32 val;
   u32 flags;
   u32 bitset;
   u64 time;
   u32 *uaddr2;
  } futex;

  struct {
   clockid_t clockid;
   struct timespec *rmtp;

   struct compat_timespec *compat_rmtp;

   u64 expires;
  } nanosleep;

  struct {
   struct pollfd *ufds;
   int nfds;
   int has_timeout;
   unsigned long tv_sec;
   unsigned long tv_nsec;
  } poll;
 };
};

extern long do_no_restart_syscall(struct restart_block *parm);


# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/thread_info.h" 1
# 11 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/thread_info.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/page.h" 1
# 11 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/page.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/page_64.h" 1
# 9 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/page_64.h"
extern unsigned long max_pfn;
extern unsigned long phys_base;

static inline __attribute__((no_instrument_function)) unsigned long __phys_addr_nodebug(unsigned long x)
{
 unsigned long y = x - (0xffffffff80000000UL);


 x = y + ((x > y) ? phys_base : ((0xffffffff80000000UL) - ((unsigned long)(0xffff880000000000UL))));

 return x;
}
# 37 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/page_64.h"
void clear_page(void *page);
void copy_page(void *to, void *from);
# 12 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/page.h" 2






struct page;

# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/range.h" 1



struct range {
 u64 start;
 u64 end;
};

int add_range(struct range *range, int az, int nr_range,
  u64 start, u64 end);


int add_range_with_merge(struct range *range, int az, int nr_range,
    u64 start, u64 end);

void subtract_range(struct range *range, int az, u64 start, u64 end);

int clean_sort_range(struct range *range, int az);

void sort_range(struct range *range, int nr_range);


static inline __attribute__((no_instrument_function)) resource_size_t cap_resource(u64 val)
{
 if (val > ((resource_size_t)~0))
  return ((resource_size_t)~0);

 return val;
}
# 21 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/page.h" 2
extern struct range pfn_mapped[];
extern int nr_pfn_mapped;

static inline __attribute__((no_instrument_function)) void clear_user_page(void *page, unsigned long vaddr,
       struct page *pg)
{
 clear_page(page);
}

static inline __attribute__((no_instrument_function)) void copy_user_page(void *to, void *from, unsigned long vaddr,
      struct page *topage)
{
 copy_page(to, from);
}
# 65 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/page.h"
extern bool __virt_addr_valid(unsigned long kaddr);




# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/memory_model.h" 1
# 71 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/page.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/getorder.h" 1
# 12 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/getorder.h"
static inline __attribute__((no_instrument_function)) __attribute__((__const__))
int __get_order(unsigned long size)
{
 int order;

 size--;
 size >>= 12;



 order = fls64(size);

 return order;
}
# 72 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/page.h" 2
# 12 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/thread_info.h" 2
# 48 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/thread_info.h"
struct task_struct;
struct exec_domain;
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h" 1






struct task_struct;
struct mm_struct;

# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/vm86.h" 1





# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/vm86.h" 1
# 62 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/vm86.h"
struct vm86_regs {



 long ebx;
 long ecx;
 long edx;
 long esi;
 long edi;
 long ebp;
 long eax;
 long __null_ds;
 long __null_es;
 long __null_fs;
 long __null_gs;
 long orig_eax;
 long eip;
 unsigned short cs, __csh;
 long eflags;
 long esp;
 unsigned short ss, __ssh;



 unsigned short es, __esh;
 unsigned short ds, __dsh;
 unsigned short fs, __fsh;
 unsigned short gs, __gsh;
};

struct revectored_struct {
 unsigned long __map[8];
};

struct vm86_struct {
 struct vm86_regs regs;
 unsigned long flags;
 unsigned long screen_bitmap;
 unsigned long cpu_type;
 struct revectored_struct int_revectored;
 struct revectored_struct int21_revectored;
};






struct vm86plus_info_struct {
 unsigned long force_return_for_pic:1;
 unsigned long vm86dbg_active:1;
 unsigned long vm86dbg_TFpendig:1;
 unsigned long unused:28;
 unsigned long is_vm86pus:1;
 unsigned char vm86dbg_intxxtab[32];
};
struct vm86plus_struct {
 struct vm86_regs regs;
 unsigned long flags;
 unsigned long screen_bitmap;
 unsigned long cpu_type;
 struct revectored_struct int_revectored;
 struct revectored_struct int21_revectored;
 struct vm86plus_info_struct vm86plus;
};
# 7 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/vm86.h" 2
# 17 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/vm86.h"
struct kernel_vm86_regs {



 struct pt_regs pt;



 unsigned short es, __esh;
 unsigned short ds, __dsh;
 unsigned short fs, __fsh;
 unsigned short gs, __gsh;
};

struct kernel_vm86_struct {
 struct kernel_vm86_regs regs;
# 42 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/vm86.h"
 unsigned long flags;
 unsigned long screen_bitmap;
 unsigned long cpu_type;
 struct revectored_struct int_revectored;
 struct revectored_struct int21_revectored;
 struct vm86plus_info_struct vm86plus;
 struct pt_regs *regs32;
# 59 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/vm86.h"
};
# 75 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/vm86.h"
static inline __attribute__((no_instrument_function)) int handle_vm86_trap(struct kernel_vm86_regs *a, long b, int c)
{
 return 0;
}
# 11 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/math_emu.h" 1
# 11 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/math_emu.h"
struct math_emu_info {
 long ___orig_eip;
 union {
  struct pt_regs *regs;
  struct kernel_vm86_regs *vm86;
 };
};
# 12 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h" 2


# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/sigcontext.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/sigcontext.h" 1
# 23 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/sigcontext.h"
struct _fpx_sw_bytes {
 __u32 magic1;
 __u32 extended_size;


 __u64 xstate_bv;




 __u32 xstate_size;




 __u32 padding[7];
};
# 136 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/sigcontext.h"
struct _fpstate {
 __u16 cwd;
 __u16 swd;
 __u16 twd;

 __u16 fop;
 __u64 rip;
 __u64 rdp;
 __u32 mxcsr;
 __u32 mxcsr_mask;
 __u32 st_space[32];
 __u32 xmm_space[64];
 __u32 reserved2[12];
 union {
  __u32 reserved3[12];
  struct _fpx_sw_bytes sw_reserved;

 };
};
# 197 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/sigcontext.h"
struct _xsave_hdr {
 __u64 xstate_bv;
 __u64 reserved1[2];
 __u64 reserved2[5];
};

struct _ymmh_state {

 __u32 ymmh_space[64];
};







struct _xstate {
 struct _fpstate fpstate;
 struct _xsave_hdr xstate_hdr;
 struct _ymmh_state ymmh;

};
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/sigcontext.h" 2
# 40 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/sigcontext.h"
struct sigcontext {
 unsigned long r8;
 unsigned long r9;
 unsigned long r10;
 unsigned long r11;
 unsigned long r12;
 unsigned long r13;
 unsigned long r14;
 unsigned long r15;
 unsigned long di;
 unsigned long si;
 unsigned long bp;
 unsigned long bx;
 unsigned long dx;
 unsigned long ax;
 unsigned long cx;
 unsigned long sp;
 unsigned long ip;
 unsigned long flags;
 unsigned short cs;
 unsigned short gs;
 unsigned short fs;
 unsigned short __pad0;
 unsigned long err;
 unsigned long trapno;
 unsigned long oldmask;
 unsigned long cr2;
# 75 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/sigcontext.h"
 void *fpstate;
 unsigned long reserved1[8];
};
# 15 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/current.h" 1







struct task_struct;

extern __attribute__((section(".data..percpu" ""))) __typeof__(struct task_struct *) current_task;

static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) struct task_struct *get_current(void)
{
 return ({ typeof(current_task) pfo_ret__; switch (sizeof(current_task)) { case 1: asm("mov" "b ""%%""gs"":" "%" "P1"",%0" : "=q" (pfo_ret__) : "p" (&(current_task))); break; case 2: asm("mov" "w ""%%""gs"":" "%" "P1"",%0" : "=r" (pfo_ret__) : "p" (&(current_task))); break; case 4: asm("mov" "l ""%%""gs"":" "%" "P1"",%0" : "=r" (pfo_ret__) : "p" (&(current_task))); break; case 8: asm("mov" "q ""%%""gs"":" "%" "P1"",%0" : "=r" (pfo_ret__) : "p" (&(current_task))); break; default: __bad_percpu_size(); } pfo_ret__; });
}
# 16 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h" 2




# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/msr.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/msr.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/msr-index.h" 1
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/msr.h" 2




# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/ioctl.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/ioctl.h" 1
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/ioctl.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/asm-generic/ioctl.h" 1
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/ioctl.h" 2





extern unsigned int __invalid_size_argument_for_IOC;
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/ioctl.h" 2
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/ioctl.h" 2
# 10 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/msr.h" 2
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/msr.h" 2




# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/errno.h" 1
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/asm-generic/errno.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/asm-generic/errno-base.h" 1
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/asm-generic/errno.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/errno.h" 2
# 10 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/msr.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/cpumask.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h" 1
# 11 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/bitmap.h" 1







# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/string.h" 1
# 9 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/string.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/string.h" 1
# 10 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/string.h" 2

extern char *strndup_user(const char *, long);
extern void *memdup_user(const void *, size_t);




# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/string.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/string_64.h" 1
# 9 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/string_64.h"
static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) void *__inline_memcpy(void *to, const void *from, size_t n)
{
 unsigned long d0, d1, d2;
 asm volatile("rep ; movsl\n\t"
       "testb $2,%b4\n\t"
       "je 1f\n\t"
       "movsw\n"
       "1:\ttestb $1,%b4\n\t"
       "je 2f\n\t"
       "movsb\n"
       "2:"
       : "=&c" (d0), "=&D" (d1), "=&S" (d2)
       : "0" (n / 4), "q" (n), "1" ((long)to), "2" ((long)from)
       : "memory");
 return to;
}







extern void *memcpy(void *to, const void *from, size_t len);
# 55 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/string_64.h"
void *memset(void *s, int c, size_t n);


void *memmove(void *dest, const void *src, size_t count);

int memcmp(const void *cs, const void *ct, size_t count);
size_t strlen(const char *s);
char *strcpy(char *dest, const char *src);
char *strcat(char *dest, const char *src);
int strcmp(const char *cs, const char *ct);
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/string.h" 2
# 18 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/string.h" 2


extern char * strcpy(char *,const char *);


extern char * strncpy(char *,const char *, __kernel_size_t);


size_t strlcpy(char *, const char *, size_t);


extern char * strcat(char *, const char *);


extern char * strncat(char *, const char *, __kernel_size_t);


extern size_t strlcat(char *, const char *, __kernel_size_t);


extern int strcmp(const char *,const char *);


extern int strncmp(const char *,const char *,__kernel_size_t);





extern int strcasecmp(const char *s1, const char *s2);


extern int strncasecmp(const char *s1, const char *s2, size_t n);


extern char * strchr(const char *,int);


extern char * strchrnul(const char *,int);


extern char * strnchr(const char *, size_t, int);


extern char * strrchr(const char *,int);

extern char * skip_spaces(const char *);

extern char *strim(char *);

static inline __attribute__((no_instrument_function)) char *strstrip(char *str)
{
 return strim(str);
}


extern char * strstr(const char *, const char *);


extern char * strnstr(const char *, const char *, size_t);


extern __kernel_size_t strlen(const char *);


extern __kernel_size_t strnlen(const char *,__kernel_size_t);


extern char * strpbrk(const char *,const char *);


extern char * strsep(char **,const char *);


extern __kernel_size_t strspn(const char *,const char *);


extern __kernel_size_t strcspn(const char *,const char *);
# 108 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/string.h"
extern void * memscan(void *,int,__kernel_size_t);


extern int memcmp(const void *,const void *,__kernel_size_t);


extern void * memchr(const void *,int,__kernel_size_t);

void *memchr_inv(const void *s, int c, size_t n);

extern char *kstrdup(const char *s, gfp_t gfp);
extern char *kstrndup(const char *s, size_t len, gfp_t gfp);
extern void *kmemdup(const void *src, size_t len, gfp_t gfp);

extern char **argv_split(gfp_t gfp, const char *str, int *argcp);
extern void argv_free(char **argv);

extern bool sysfs_streq(const char *s1, const char *s2);
extern int strtobool(const char *s, bool *res);


int vbin_printf(u32 *bin_buf, size_t size, const char *fmt, va_list args);
int bstr_printf(char *buf, size_t size, const char *fmt, const u32 *bin_buf);
int bprintf(u32 *bin_buf, size_t size, const char *fmt, ...) __attribute__((format(printf, 3, 4)));


extern ssize_t memory_read_from_buffer(void *to, size_t count, loff_t *ppos,
           const void *from, size_t available);






static inline __attribute__((no_instrument_function)) bool strstarts(const char *str, const char *prefix)
{
 return strncmp(str, prefix, strlen(prefix)) == 0;
}

size_t memweight(const void *ptr, size_t bytes);
void memzero_explicit(void *s, size_t count);






static inline __attribute__((no_instrument_function)) const char *kbasename(const char *path)
{
 const char *tail = strrchr(path, '/');
 return tail ? tail + 1 : path;
}
# 9 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/bitmap.h" 2
# 93 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/bitmap.h"
extern int __bitmap_empty(const unsigned long *bitmap, unsigned int nbits);
extern int __bitmap_full(const unsigned long *bitmap, unsigned int nbits);
extern int __bitmap_equal(const unsigned long *bitmap1,
     const unsigned long *bitmap2, unsigned int nbits);
extern void __bitmap_complement(unsigned long *dst, const unsigned long *src,
   unsigned int nbits);
extern void __bitmap_shift_right(unsigned long *dst,
                        const unsigned long *src, int shift, int bits);
extern void __bitmap_shift_left(unsigned long *dst,
                        const unsigned long *src, int shift, int bits);
extern int __bitmap_and(unsigned long *dst, const unsigned long *bitmap1,
   const unsigned long *bitmap2, unsigned int nbits);
extern void __bitmap_or(unsigned long *dst, const unsigned long *bitmap1,
   const unsigned long *bitmap2, unsigned int nbits);
extern void __bitmap_xor(unsigned long *dst, const unsigned long *bitmap1,
   const unsigned long *bitmap2, unsigned int nbits);
extern int __bitmap_andnot(unsigned long *dst, const unsigned long *bitmap1,
   const unsigned long *bitmap2, unsigned int nbits);
extern int __bitmap_intersects(const unsigned long *bitmap1,
   const unsigned long *bitmap2, unsigned int nbits);
extern int __bitmap_subset(const unsigned long *bitmap1,
   const unsigned long *bitmap2, unsigned int nbits);
extern int __bitmap_weight(const unsigned long *bitmap, unsigned int nbits);

extern void bitmap_set(unsigned long *map, unsigned int start, int len);
extern void bitmap_clear(unsigned long *map, unsigned int start, int len);

extern unsigned long bitmap_find_next_zero_area_off(unsigned long *map,
          unsigned long size,
          unsigned long start,
          unsigned int nr,
          unsigned long align_mask,
          unsigned long align_offset);
# 139 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/bitmap.h"
static inline __attribute__((no_instrument_function)) unsigned long
bitmap_find_next_zero_area(unsigned long *map,
      unsigned long size,
      unsigned long start,
      unsigned int nr,
      unsigned long align_mask)
{
 return bitmap_find_next_zero_area_off(map, size, start, nr,
           align_mask, 0);
}

extern int bitmap_scnprintf(char *buf, unsigned int len,
   const unsigned long *src, int nbits);
extern int __bitmap_parse(const char *buf, unsigned int buflen, int is_user,
   unsigned long *dst, int nbits);
extern int bitmap_parse_user(const char *ubuf, unsigned int ulen,
   unsigned long *dst, int nbits);
extern int bitmap_scnlistprintf(char *buf, unsigned int len,
   const unsigned long *src, int nbits);
extern int bitmap_parselist(const char *buf, unsigned long *maskp,
   int nmaskbits);
extern int bitmap_parselist_user(const char *ubuf, unsigned int ulen,
   unsigned long *dst, int nbits);
extern void bitmap_remap(unsigned long *dst, const unsigned long *src,
  const unsigned long *old, const unsigned long *new, int bits);
extern int bitmap_bitremap(int oldbit,
  const unsigned long *old, const unsigned long *new, int bits);
extern void bitmap_onto(unsigned long *dst, const unsigned long *orig,
  const unsigned long *relmap, int bits);
extern void bitmap_fold(unsigned long *dst, const unsigned long *orig,
  int sz, int bits);
extern int bitmap_find_free_region(unsigned long *bitmap, unsigned int bits, int order);
extern void bitmap_release_region(unsigned long *bitmap, unsigned int pos, int order);
extern int bitmap_allocate_region(unsigned long *bitmap, unsigned int pos, int order);
extern void bitmap_copy_le(void *dst, const unsigned long *src, int nbits);
extern int bitmap_ord_to_pos(const unsigned long *bitmap, int n, int bits);
extern int bitmap_print_to_pagebuf(bool list, char *buf,
       const unsigned long *maskp, int nmaskbits);
# 188 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/bitmap.h"
static inline __attribute__((no_instrument_function)) void bitmap_zero(unsigned long *dst, int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  *dst = 0UL;
 else {
  int len = (((nbits) + (8 * sizeof(long)) - 1) / (8 * sizeof(long))) * sizeof(unsigned long);
  memset(dst, 0, len);
 }
}

static inline __attribute__((no_instrument_function)) void bitmap_fill(unsigned long *dst, int nbits)
{
 size_t nlongs = (((nbits) + (8 * sizeof(long)) - 1) / (8 * sizeof(long)));
 if (!(__builtin_constant_p(nbits) && (nbits) <= 64)) {
  int len = (nlongs - 1) * sizeof(unsigned long);
  memset(dst, 0xff, len);
 }
 dst[nlongs - 1] = ( ((nbits) % 64) ? (1UL<<((nbits) % 64))-1 : ~0UL );
}

static inline __attribute__((no_instrument_function)) void bitmap_copy(unsigned long *dst, const unsigned long *src,
   int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  *dst = *src;
 else {
  int len = (((nbits) + (8 * sizeof(long)) - 1) / (8 * sizeof(long))) * sizeof(unsigned long);
  memcpy(dst, src, len);
 }
}

static inline __attribute__((no_instrument_function)) int bitmap_and(unsigned long *dst, const unsigned long *src1,
   const unsigned long *src2, unsigned int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  return (*dst = *src1 & *src2 & ( ((nbits) % 64) ? (1UL<<((nbits) % 64))-1 : ~0UL )) != 0;
 return __bitmap_and(dst, src1, src2, nbits);
}

static inline __attribute__((no_instrument_function)) void bitmap_or(unsigned long *dst, const unsigned long *src1,
   const unsigned long *src2, unsigned int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  *dst = *src1 | *src2;
 else
  __bitmap_or(dst, src1, src2, nbits);
}

static inline __attribute__((no_instrument_function)) void bitmap_xor(unsigned long *dst, const unsigned long *src1,
   const unsigned long *src2, unsigned int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  *dst = *src1 ^ *src2;
 else
  __bitmap_xor(dst, src1, src2, nbits);
}

static inline __attribute__((no_instrument_function)) int bitmap_andnot(unsigned long *dst, const unsigned long *src1,
   const unsigned long *src2, unsigned int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  return (*dst = *src1 & ~(*src2) & ( ((nbits) % 64) ? (1UL<<((nbits) % 64))-1 : ~0UL )) != 0;
 return __bitmap_andnot(dst, src1, src2, nbits);
}

static inline __attribute__((no_instrument_function)) void bitmap_complement(unsigned long *dst, const unsigned long *src,
   unsigned int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  *dst = ~(*src);
 else
  __bitmap_complement(dst, src, nbits);
}

static inline __attribute__((no_instrument_function)) int bitmap_equal(const unsigned long *src1,
   const unsigned long *src2, unsigned int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  return ! ((*src1 ^ *src2) & ( ((nbits) % 64) ? (1UL<<((nbits) % 64))-1 : ~0UL ));
 else
  return __bitmap_equal(src1, src2, nbits);
}

static inline __attribute__((no_instrument_function)) int bitmap_intersects(const unsigned long *src1,
   const unsigned long *src2, unsigned int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  return ((*src1 & *src2) & ( ((nbits) % 64) ? (1UL<<((nbits) % 64))-1 : ~0UL )) != 0;
 else
  return __bitmap_intersects(src1, src2, nbits);
}

static inline __attribute__((no_instrument_function)) int bitmap_subset(const unsigned long *src1,
   const unsigned long *src2, unsigned int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  return ! ((*src1 & ~(*src2)) & ( ((nbits) % 64) ? (1UL<<((nbits) % 64))-1 : ~0UL ));
 else
  return __bitmap_subset(src1, src2, nbits);
}

static inline __attribute__((no_instrument_function)) int bitmap_empty(const unsigned long *src, unsigned nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  return ! (*src & ( ((nbits) % 64) ? (1UL<<((nbits) % 64))-1 : ~0UL ));
 else
  return __bitmap_empty(src, nbits);
}

static inline __attribute__((no_instrument_function)) int bitmap_full(const unsigned long *src, unsigned int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  return ! (~(*src) & ( ((nbits) % 64) ? (1UL<<((nbits) % 64))-1 : ~0UL ));
 else
  return __bitmap_full(src, nbits);
}

static inline __attribute__((no_instrument_function)) int bitmap_weight(const unsigned long *src, unsigned int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  return hweight_long(*src & ( ((nbits) % 64) ? (1UL<<((nbits) % 64))-1 : ~0UL ));
 return __bitmap_weight(src, nbits);
}

static inline __attribute__((no_instrument_function)) void bitmap_shift_right(unsigned long *dst,
   const unsigned long *src, int n, int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  *dst = (*src & ( ((nbits) % 64) ? (1UL<<((nbits) % 64))-1 : ~0UL )) >> n;
 else
  __bitmap_shift_right(dst, src, n, nbits);
}

static inline __attribute__((no_instrument_function)) void bitmap_shift_left(unsigned long *dst,
   const unsigned long *src, int n, int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  *dst = (*src << n) & ( ((nbits) % 64) ? (1UL<<((nbits) % 64))-1 : ~0UL );
 else
  __bitmap_shift_left(dst, src, n, nbits);
}

static inline __attribute__((no_instrument_function)) int bitmap_parse(const char *buf, unsigned int buflen,
   unsigned long *maskp, int nmaskbits)
{
 return __bitmap_parse(buf, buflen, 0, maskp, nmaskbits);
}
# 12 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h" 2


typedef struct cpumask { unsigned long bits[(((256) + (8 * sizeof(long)) - 1) / (8 * sizeof(long)))]; } cpumask_t;
# 28 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
extern int nr_cpu_ids;
# 79 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
extern const struct cpumask *const cpu_possible_mask;
extern const struct cpumask *const cpu_online_mask;
extern const struct cpumask *const cpu_present_mask;
extern const struct cpumask *const cpu_active_mask;
# 105 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
static inline __attribute__((no_instrument_function)) unsigned int cpumask_check(unsigned int cpu)
{



 return cpu;
}
# 165 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
static inline __attribute__((no_instrument_function)) unsigned int cpumask_first(const struct cpumask *srcp)
{
 return find_first_bit(((srcp)->bits), 256);
}
# 177 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
static inline __attribute__((no_instrument_function)) unsigned int cpumask_next(int n, const struct cpumask *srcp)
{

 if (n != -1)
  cpumask_check(n);
 return find_next_bit(((srcp)->bits), 256, n+1);
}
# 192 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
static inline __attribute__((no_instrument_function)) unsigned int cpumask_next_zero(int n, const struct cpumask *srcp)
{

 if (n != -1)
  cpumask_check(n);
 return find_next_zero_bit(((srcp)->bits), 256, n+1);
}

int cpumask_next_and(int n, const struct cpumask *, const struct cpumask *);
int cpumask_any_but(const struct cpumask *mask, unsigned int cpu);
int cpumask_set_cpu_local_first(int i, int numa_node, cpumask_t *dstp);
# 263 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
static inline __attribute__((no_instrument_function)) void cpumask_set_cpu(unsigned int cpu, struct cpumask *dstp)
{
 set_bit(cpumask_check(cpu), ((dstp)->bits));
}






static inline __attribute__((no_instrument_function)) void cpumask_clear_cpu(int cpu, struct cpumask *dstp)
{
 clear_bit(cpumask_check(cpu), ((dstp)->bits));
}
# 299 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
static inline __attribute__((no_instrument_function)) int cpumask_test_and_set_cpu(int cpu, struct cpumask *cpumask)
{
 return test_and_set_bit(cpumask_check(cpu), ((cpumask)->bits));
}
# 313 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
static inline __attribute__((no_instrument_function)) int cpumask_test_and_clear_cpu(int cpu, struct cpumask *cpumask)
{
 return test_and_clear_bit(cpumask_check(cpu), ((cpumask)->bits));
}





static inline __attribute__((no_instrument_function)) void cpumask_setall(struct cpumask *dstp)
{
 bitmap_fill(((dstp)->bits), 256);
}





static inline __attribute__((no_instrument_function)) void cpumask_clear(struct cpumask *dstp)
{
 bitmap_zero(((dstp)->bits), 256);
}
# 344 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
static inline __attribute__((no_instrument_function)) int cpumask_and(struct cpumask *dstp,
          const struct cpumask *src1p,
          const struct cpumask *src2p)
{
 return bitmap_and(((dstp)->bits), ((src1p)->bits),
           ((src2p)->bits), 256);
}







static inline __attribute__((no_instrument_function)) void cpumask_or(struct cpumask *dstp, const struct cpumask *src1p,
         const struct cpumask *src2p)
{
 bitmap_or(((dstp)->bits), ((src1p)->bits),
          ((src2p)->bits), 256);
}







static inline __attribute__((no_instrument_function)) void cpumask_xor(struct cpumask *dstp,
          const struct cpumask *src1p,
          const struct cpumask *src2p)
{
 bitmap_xor(((dstp)->bits), ((src1p)->bits),
           ((src2p)->bits), 256);
}
# 387 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
static inline __attribute__((no_instrument_function)) int cpumask_andnot(struct cpumask *dstp,
      const struct cpumask *src1p,
      const struct cpumask *src2p)
{
 return bitmap_andnot(((dstp)->bits), ((src1p)->bits),
       ((src2p)->bits), 256);
}






static inline __attribute__((no_instrument_function)) void cpumask_complement(struct cpumask *dstp,
          const struct cpumask *srcp)
{
 bitmap_complement(((dstp)->bits), ((srcp)->bits),
           256);
}






static inline __attribute__((no_instrument_function)) bool cpumask_equal(const struct cpumask *src1p,
    const struct cpumask *src2p)
{
 return bitmap_equal(((src1p)->bits), ((src2p)->bits),
       256);
}






static inline __attribute__((no_instrument_function)) bool cpumask_intersects(const struct cpumask *src1p,
         const struct cpumask *src2p)
{
 return bitmap_intersects(((src1p)->bits), ((src2p)->bits),
            256);
}
# 438 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
static inline __attribute__((no_instrument_function)) int cpumask_subset(const struct cpumask *src1p,
     const struct cpumask *src2p)
{
 return bitmap_subset(((src1p)->bits), ((src2p)->bits),
        256);
}





static inline __attribute__((no_instrument_function)) bool cpumask_empty(const struct cpumask *srcp)
{
 return bitmap_empty(((srcp)->bits), 256);
}





static inline __attribute__((no_instrument_function)) bool cpumask_full(const struct cpumask *srcp)
{
 return bitmap_full(((srcp)->bits), 256);
}





static inline __attribute__((no_instrument_function)) unsigned int cpumask_weight(const struct cpumask *srcp)
{
 return bitmap_weight(((srcp)->bits), 256);
}







static inline __attribute__((no_instrument_function)) void cpumask_shift_right(struct cpumask *dstp,
           const struct cpumask *srcp, int n)
{
 bitmap_shift_right(((dstp)->bits), ((srcp)->bits), n,
            256);
}







static inline __attribute__((no_instrument_function)) void cpumask_shift_left(struct cpumask *dstp,
          const struct cpumask *srcp, int n)
{
 bitmap_shift_left(((dstp)->bits), ((srcp)->bits), n,
           256);
}






static inline __attribute__((no_instrument_function)) void cpumask_copy(struct cpumask *dstp,
    const struct cpumask *srcp)
{
 bitmap_copy(((dstp)->bits), ((srcp)->bits), 256);
}
# 550 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
static inline __attribute__((no_instrument_function)) int cpumask_scnprintf(char *buf, int len,
        const struct cpumask *srcp)
{
 return bitmap_scnprintf(buf, len, ((srcp)->bits), 256);
}
# 564 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
static inline __attribute__((no_instrument_function)) int cpumask_parse_user(const char *buf, int len,
         struct cpumask *dstp)
{
 return bitmap_parse_user(buf, len, ((dstp)->bits), 256);
}
# 578 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
static inline __attribute__((no_instrument_function)) int cpumask_parselist_user(const char *buf, int len,
         struct cpumask *dstp)
{
 return bitmap_parselist_user(buf, len, ((dstp)->bits),
       256);
}
# 594 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
static inline __attribute__((no_instrument_function)) int cpulist_scnprintf(char *buf, int len,
        const struct cpumask *srcp)
{
 return bitmap_scnlistprintf(buf, len, ((srcp)->bits),
        256);
}
# 608 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
static inline __attribute__((no_instrument_function)) int cpumask_parse(const char *buf, struct cpumask *dstp)
{
 char *nl = strchr(buf, '\n');
 unsigned int len = nl ? (unsigned int)(nl - buf) : strlen(buf);

 return bitmap_parse(buf, len, ((dstp)->bits), 256);
}
# 623 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
static inline __attribute__((no_instrument_function)) int cpulist_parse(const char *buf, struct cpumask *dstp)
{
 return bitmap_parselist(buf, ((dstp)->bits), 256);
}






static inline __attribute__((no_instrument_function)) size_t cpumask_size(void)
{


 return (((256) + (8 * sizeof(long)) - 1) / (8 * sizeof(long))) * sizeof(long);
}
# 691 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
typedef struct cpumask cpumask_var_t[1];



static inline __attribute__((no_instrument_function)) bool alloc_cpumask_var(cpumask_var_t *mask, gfp_t flags)
{
 return true;
}

static inline __attribute__((no_instrument_function)) bool alloc_cpumask_var_node(cpumask_var_t *mask, gfp_t flags,
       int node)
{
 return true;
}

static inline __attribute__((no_instrument_function)) bool zalloc_cpumask_var(cpumask_var_t *mask, gfp_t flags)
{
 cpumask_clear(*mask);
 return true;
}

static inline __attribute__((no_instrument_function)) bool zalloc_cpumask_var_node(cpumask_var_t *mask, gfp_t flags,
       int node)
{
 cpumask_clear(*mask);
 return true;
}

static inline __attribute__((no_instrument_function)) void alloc_bootmem_cpumask_var(cpumask_var_t *mask)
{
}

static inline __attribute__((no_instrument_function)) void free_cpumask_var(cpumask_var_t mask)
{
}

static inline __attribute__((no_instrument_function)) void free_bootmem_cpumask_var(cpumask_var_t mask)
{
}




extern const unsigned long cpu_all_bits[(((256) + (8 * sizeof(long)) - 1) / (8 * sizeof(long)))];
# 745 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
void set_cpu_possible(unsigned int cpu, bool possible);
void set_cpu_present(unsigned int cpu, bool present);
void set_cpu_online(unsigned int cpu, bool online);
void set_cpu_active(unsigned int cpu, bool active);
void init_cpu_present(const struct cpumask *src);
void init_cpu_possible(const struct cpumask *src);
void init_cpu_online(const struct cpumask *src);
# 767 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
static inline __attribute__((no_instrument_function)) int __check_is_bitmap(const unsigned long *bitmap)
{
 return 1;
}
# 779 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
extern const unsigned long
 cpu_bit_bitmap[64 +1][(((256) + (8 * sizeof(long)) - 1) / (8 * sizeof(long)))];

static inline __attribute__((no_instrument_function)) const struct cpumask *get_cpu_mask(unsigned int cpu)
{
 const unsigned long *p = cpu_bit_bitmap[1 + cpu % 64];
 p -= cpu / 64;
 return ((struct cpumask *)(1 ? (p) : (void *)sizeof(__check_is_bitmap(p))));
}
# 816 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
static inline __attribute__((no_instrument_function)) ssize_t
cpumap_print_to_pagebuf(bool list, char *buf, const struct cpumask *mask)
{
 return bitmap_print_to_pagebuf(list, buf, ((mask)->bits),
          256);
}
# 867 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
int __first_cpu(const cpumask_t *srcp);
int __next_cpu(int n, const cpumask_t *srcp);
# 885 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
int __next_cpu_nr(int n, const cpumask_t *srcp);
# 896 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/cpumask.h"
static inline __attribute__((no_instrument_function)) void __cpu_set(int cpu, volatile cpumask_t *dstp)
{
 set_bit(cpu, dstp->bits);
}


static inline __attribute__((no_instrument_function)) void __cpu_clear(int cpu, volatile cpumask_t *dstp)
{
 clear_bit(cpu, dstp->bits);
}


static inline __attribute__((no_instrument_function)) void __cpus_setall(cpumask_t *dstp, int nbits)
{
 bitmap_fill(dstp->bits, nbits);
}


static inline __attribute__((no_instrument_function)) void __cpus_clear(cpumask_t *dstp, int nbits)
{
 bitmap_zero(dstp->bits, nbits);
}





static inline __attribute__((no_instrument_function)) int __cpu_test_and_set(int cpu, cpumask_t *addr)
{
 return test_and_set_bit(cpu, addr->bits);
}


static inline __attribute__((no_instrument_function)) int __cpus_and(cpumask_t *dstp, const cpumask_t *src1p,
     const cpumask_t *src2p, int nbits)
{
 return bitmap_and(dstp->bits, src1p->bits, src2p->bits, nbits);
}


static inline __attribute__((no_instrument_function)) void __cpus_or(cpumask_t *dstp, const cpumask_t *src1p,
     const cpumask_t *src2p, int nbits)
{
 bitmap_or(dstp->bits, src1p->bits, src2p->bits, nbits);
}


static inline __attribute__((no_instrument_function)) void __cpus_xor(cpumask_t *dstp, const cpumask_t *src1p,
     const cpumask_t *src2p, int nbits)
{
 bitmap_xor(dstp->bits, src1p->bits, src2p->bits, nbits);
}



static inline __attribute__((no_instrument_function)) int __cpus_andnot(cpumask_t *dstp, const cpumask_t *src1p,
     const cpumask_t *src2p, int nbits)
{
 return bitmap_andnot(dstp->bits, src1p->bits, src2p->bits, nbits);
}


static inline __attribute__((no_instrument_function)) int __cpus_equal(const cpumask_t *src1p,
     const cpumask_t *src2p, int nbits)
{
 return bitmap_equal(src1p->bits, src2p->bits, nbits);
}


static inline __attribute__((no_instrument_function)) int __cpus_intersects(const cpumask_t *src1p,
     const cpumask_t *src2p, int nbits)
{
 return bitmap_intersects(src1p->bits, src2p->bits, nbits);
}


static inline __attribute__((no_instrument_function)) int __cpus_subset(const cpumask_t *src1p,
     const cpumask_t *src2p, int nbits)
{
 return bitmap_subset(src1p->bits, src2p->bits, nbits);
}


static inline __attribute__((no_instrument_function)) int __cpus_empty(const cpumask_t *srcp, int nbits)
{
 return bitmap_empty(srcp->bits, nbits);
}


static inline __attribute__((no_instrument_function)) int __cpus_weight(const cpumask_t *srcp, int nbits)
{
 return bitmap_weight(srcp->bits, nbits);
}



static inline __attribute__((no_instrument_function)) void __cpus_shift_left(cpumask_t *dstp,
     const cpumask_t *srcp, int n, int nbits)
{
 bitmap_shift_left(dstp->bits, srcp->bits, n, nbits);
}
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/cpumask.h" 2

extern cpumask_var_t cpu_callin_mask;
extern cpumask_var_t cpu_callout_mask;
extern cpumask_var_t cpu_initialized_mask;
extern cpumask_var_t cpu_sibling_setup_mask;

extern void setup_cpu_local_masks(void);
# 11 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/msr.h" 2

struct msr {
 union {
  struct {
   u32 l;
   u32 h;
  };
  u64 q;
 };
};

struct msr_info {
 u32 msr_no;
 struct msr reg;
 struct msr *msrs;
 int err;
};

struct msr_regs_info {
 u32 *regs;
 int err;
};

static inline __attribute__((no_instrument_function)) unsigned long long native_read_tscp(unsigned int *aux)
{
 unsigned long low, high;
 asm volatile(".byte 0x0f,0x01,0xf9"
       : "=a" (low), "=d" (high), "=c" (*aux));
 return low | ((u64)high << 32);
}
# 60 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/msr.h"
static inline __attribute__((no_instrument_function)) unsigned long long native_read_msr(unsigned int msr)
{
 unsigned low, high;

 asm volatile("rdmsr" : "=a" (low), "=d" (high) : "c" (msr));
 return ((low) | ((u64)(high) << 32));
}

static inline __attribute__((no_instrument_function)) unsigned long long native_read_msr_safe(unsigned int msr,
            int *err)
{
 unsigned low, high;

 asm volatile("2: rdmsr ; xor %[err],%[err]\n"
       "1:\n\t"
       ".section .fixup,\"ax\"\n\t"
       "3:  mov %[fault],%[err] ; jmp 1b\n\t"
       ".previous\n\t"
       " .pushsection \"__ex_table\",\"a\"\n" " .balign 8\n" " .long (" "2b" ") - .\n" " .long (" "3b" ") - .\n" " .popsection\n"
       : [err] "=r" (*err), "=a" (low), "=d" (high)
       : "c" (msr), [fault] "i" (-5));
 return ((low) | ((u64)(high) << 32));
}

static inline __attribute__((no_instrument_function)) void native_write_msr(unsigned int msr,
        unsigned low, unsigned high)
{
 asm volatile("wrmsr" : : "c" (msr), "a"(low), "d" (high) : "memory");
}


__attribute__((no_instrument_function)) static inline __attribute__((no_instrument_function)) int native_write_msr_safe(unsigned int msr,
     unsigned low, unsigned high)
{
 int err;
 asm volatile("2: wrmsr ; xor %[err],%[err]\n"
       "1:\n\t"
       ".section .fixup,\"ax\"\n\t"
       "3:  mov %[fault],%[err] ; jmp 1b\n\t"
       ".previous\n\t"
       " .pushsection \"__ex_table\",\"a\"\n" " .balign 8\n" " .long (" "2b" ") - .\n" " .long (" "3b" ") - .\n" " .popsection\n"
       : [err] "=a" (err)
       : "c" (msr), "0" (low), "d" (high),
         [fault] "i" (-5)
       : "memory");
 return err;
}

extern unsigned long long native_read_tsc(void);

extern int rdmsr_safe_regs(u32 regs[8]);
extern int wrmsr_safe_regs(u32 regs[8]);

static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) unsigned long long __native_read_tsc(void)
{
 unsigned low, high;

 asm volatile("rdtsc" : "=a" (low), "=d" (high));

 return ((low) | ((u64)(high) << 32));
}

static inline __attribute__((no_instrument_function)) unsigned long long native_read_pmc(int counter)
{
 unsigned low, high;

 asm volatile("rdpmc" : "=a" (low), "=d" (high) : "c" (counter));
 return ((low) | ((u64)(high) << 32));
}


# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/paravirt.h" 1
# 17 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/paravirt.h"
static inline __attribute__((no_instrument_function)) int paravirt_enabled(void)
{
 return pv_info.paravirt_enabled;
}

static inline __attribute__((no_instrument_function)) void load_sp0(struct tss_struct *tss,
        struct thread_struct *thread)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.load_sp0); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.load_sp0) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.load_sp0)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(tss)), "S" ((unsigned long)(thread)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}


static inline __attribute__((no_instrument_function)) void __cpuid(unsigned int *eax, unsigned int *ebx,
      unsigned int *ecx, unsigned int *edx)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.cpuid); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.cpuid) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.cpuid)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(eax)), "S" ((unsigned long)(ebx)), "d" ((unsigned long)(ecx)), "c" ((unsigned long)(edx)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}




static inline __attribute__((no_instrument_function)) unsigned long paravirt_get_debugreg(int reg)
{
 return ({ unsigned long __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.get_debugreg); if (sizeof(unsigned long) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.get_debugreg) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.get_debugreg)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(reg)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (unsigned long)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.get_debugreg) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.get_debugreg)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(reg)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (unsigned long)__eax; } __ret; });
}

static inline __attribute__((no_instrument_function)) void set_debugreg(unsigned long val, int reg)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.set_debugreg); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.set_debugreg) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.set_debugreg)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(reg)), "S" ((unsigned long)(val)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void clts(void)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.clts); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.clts) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.clts)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) unsigned long read_cr0(void)
{
 return ({ unsigned long __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.read_cr0); if (sizeof(unsigned long) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.read_cr0) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.read_cr0)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (unsigned long)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.read_cr0) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.read_cr0)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (unsigned long)__eax; } __ret; });
}

static inline __attribute__((no_instrument_function)) void write_cr0(unsigned long x)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.write_cr0); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.write_cr0) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.write_cr0)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(x)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) unsigned long read_cr2(void)
{
 return ({ unsigned long __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.read_cr2); if (sizeof(unsigned long) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.read_cr2) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.read_cr2)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (unsigned long)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.read_cr2) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.read_cr2)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (unsigned long)__eax; } __ret; });
}

static inline __attribute__((no_instrument_function)) void write_cr2(unsigned long x)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.write_cr2); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.write_cr2) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.write_cr2)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(x)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) unsigned long read_cr3(void)
{
 return ({ unsigned long __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.read_cr3); if (sizeof(unsigned long) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.read_cr3) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.read_cr3)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (unsigned long)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.read_cr3) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.read_cr3)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (unsigned long)__eax; } __ret; });
}

static inline __attribute__((no_instrument_function)) void write_cr3(unsigned long x)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.write_cr3); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.write_cr3) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.write_cr3)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(x)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) unsigned long read_cr4(void)
{
 return ({ unsigned long __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.read_cr4); if (sizeof(unsigned long) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.read_cr4) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.read_cr4)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (unsigned long)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.read_cr4) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.read_cr4)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (unsigned long)__eax; } __ret; });
}
static inline __attribute__((no_instrument_function)) unsigned long read_cr4_safe(void)
{
 return ({ unsigned long __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.read_cr4_safe); if (sizeof(unsigned long) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.read_cr4_safe) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.read_cr4_safe)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (unsigned long)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.read_cr4_safe) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.read_cr4_safe)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (unsigned long)__eax; } __ret; });
}

static inline __attribute__((no_instrument_function)) void write_cr4(unsigned long x)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.write_cr4); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.write_cr4) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.write_cr4)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(x)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}


static inline __attribute__((no_instrument_function)) unsigned long read_cr8(void)
{
 return ({ unsigned long __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.read_cr8); if (sizeof(unsigned long) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.read_cr8) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.read_cr8)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (unsigned long)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.read_cr8) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.read_cr8)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (unsigned long)__eax; } __ret; });
}

static inline __attribute__((no_instrument_function)) void write_cr8(unsigned long x)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.write_cr8); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.write_cr8) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.write_cr8)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(x)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}


static inline __attribute__((no_instrument_function)) void arch_safe_halt(void)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_irq_ops.safe_halt); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_irq_ops.safe_halt) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_irq_ops.safe_halt)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void halt(void)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_irq_ops.halt); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_irq_ops.halt) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_irq_ops.halt)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void wbinvd(void)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.wbinvd); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.wbinvd) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.wbinvd)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}



static inline __attribute__((no_instrument_function)) u64 paravirt_read_msr(unsigned msr, int *err)
{
 return ({ u64 __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.read_msr); if (sizeof(u64) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.read_msr) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.read_msr)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(msr)), "S" ((unsigned long)(err)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (u64)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.read_msr) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.read_msr)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(msr)), "S" ((unsigned long)(err)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (u64)__eax; } __ret; });
}

static inline __attribute__((no_instrument_function)) int paravirt_write_msr(unsigned msr, unsigned low, unsigned high)
{
 return ({ int __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.write_msr); if (sizeof(int) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.write_msr) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.write_msr)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(msr)), "S" ((unsigned long)(low)), "d" ((unsigned long)(high)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (int)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.write_msr) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.write_msr)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(msr)), "S" ((unsigned long)(low)), "d" ((unsigned long)(high)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (int)__eax; } __ret; });
}
# 169 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/paravirt.h"
static inline __attribute__((no_instrument_function)) int rdmsrl_safe(unsigned msr, unsigned long long *p)
{
 int err;

 *p = paravirt_read_msr(msr, &err);
 return err;
}

static inline __attribute__((no_instrument_function)) u64 paravirt_read_tsc(void)
{
 return ({ u64 __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.read_tsc); if (sizeof(u64) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.read_tsc) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.read_tsc)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (u64)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.read_tsc) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.read_tsc)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (u64)__eax; } __ret; });
}
# 190 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/paravirt.h"
static inline __attribute__((no_instrument_function)) unsigned long long paravirt_sched_clock(void)
{
 return ({ unsigned long long __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_time_ops.sched_clock); if (sizeof(unsigned long long) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_time_ops.sched_clock) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_time_ops.sched_clock)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (unsigned long long)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_time_ops.sched_clock) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_time_ops.sched_clock)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (unsigned long long)__eax; } __ret; });
}

struct static_key;
extern struct static_key paravirt_steal_enabled;
extern struct static_key paravirt_steal_rq_enabled;

static inline __attribute__((no_instrument_function)) u64 paravirt_steal_clock(int cpu)
{
 return ({ u64 __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_time_ops.steal_clock); if (sizeof(u64) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_time_ops.steal_clock) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_time_ops.steal_clock)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(cpu)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (u64)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_time_ops.steal_clock) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_time_ops.steal_clock)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(cpu)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (u64)__eax; } __ret; });
}

static inline __attribute__((no_instrument_function)) unsigned long long paravirt_read_pmc(int counter)
{
 return ({ u64 __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.read_pmc); if (sizeof(u64) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.read_pmc) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.read_pmc)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(counter)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (u64)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.read_pmc) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.read_pmc)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(counter)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (u64)__eax; } __ret; });
}
# 218 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/paravirt.h"
static inline __attribute__((no_instrument_function)) unsigned long long paravirt_rdtscp(unsigned int *aux)
{
 return ({ u64 __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.read_tscp); if (sizeof(u64) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.read_tscp) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.read_tscp)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(aux)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (u64)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.read_tscp) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.read_tscp)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(aux)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (u64)__eax; } __ret; });
}
# 239 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/paravirt.h"
static inline __attribute__((no_instrument_function)) void paravirt_alloc_ldt(struct desc_struct *ldt, unsigned entries)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.alloc_ldt); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.alloc_ldt) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.alloc_ldt)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(ldt)), "S" ((unsigned long)(entries)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void paravirt_free_ldt(struct desc_struct *ldt, unsigned entries)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.free_ldt); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.free_ldt) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.free_ldt)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(ldt)), "S" ((unsigned long)(entries)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void load_TR_desc(void)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.load_tr_desc); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.load_tr_desc) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.load_tr_desc)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}
static inline __attribute__((no_instrument_function)) void load_gdt(const struct desc_ptr *dtr)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.load_gdt); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.load_gdt) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.load_gdt)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(dtr)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}
static inline __attribute__((no_instrument_function)) void load_idt(const struct desc_ptr *dtr)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.load_idt); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.load_idt) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.load_idt)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(dtr)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}
static inline __attribute__((no_instrument_function)) void set_ldt(const void *addr, unsigned entries)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.set_ldt); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.set_ldt) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.set_ldt)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(addr)), "S" ((unsigned long)(entries)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}
static inline __attribute__((no_instrument_function)) void store_idt(struct desc_ptr *dtr)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.store_idt); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.store_idt) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.store_idt)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(dtr)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}
static inline __attribute__((no_instrument_function)) unsigned long paravirt_store_tr(void)
{
 return ({ unsigned long __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.store_tr); if (sizeof(unsigned long) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.store_tr) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.store_tr)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (unsigned long)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.store_tr) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.store_tr)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (unsigned long)__eax; } __ret; });
}

static inline __attribute__((no_instrument_function)) void load_TLS(struct thread_struct *t, unsigned cpu)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.load_tls); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.load_tls) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.load_tls)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(t)), "S" ((unsigned long)(cpu)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}


static inline __attribute__((no_instrument_function)) void load_gs_index(unsigned int gs)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.load_gs_index); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.load_gs_index) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.load_gs_index)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(gs)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}


static inline __attribute__((no_instrument_function)) void write_ldt_entry(struct desc_struct *dt, int entry,
       const void *desc)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.write_ldt_entry); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.write_ldt_entry) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.write_ldt_entry)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(dt)), "S" ((unsigned long)(entry)), "d" ((unsigned long)(desc)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void write_gdt_entry(struct desc_struct *dt, int entry,
       void *desc, int type)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.write_gdt_entry); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.write_gdt_entry) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.write_gdt_entry)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(dt)), "S" ((unsigned long)(entry)), "d" ((unsigned long)(desc)), "c" ((unsigned long)(type)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void write_idt_entry(gate_desc *dt, int entry, const gate_desc *g)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.write_idt_entry); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.write_idt_entry) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.write_idt_entry)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(dt)), "S" ((unsigned long)(entry)), "d" ((unsigned long)(g)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}
static inline __attribute__((no_instrument_function)) void set_iopl_mask(unsigned mask)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.set_iopl_mask); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.set_iopl_mask) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.set_iopl_mask)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(mask)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}


static inline __attribute__((no_instrument_function)) void slow_down_io(void)
{
 pv_cpu_ops.io_delay();





}


static inline __attribute__((no_instrument_function)) void startup_ipi_hook(int phys_apicid, unsigned long start_eip,
        unsigned long start_esp)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_apic_ops.startup_ipi_hook); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_apic_ops.startup_ipi_hook) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_apic_ops.startup_ipi_hook)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(phys_apicid)), "S" ((unsigned long)(start_eip)), "d" ((unsigned long)(start_esp)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); })
                                        ;
}


static inline __attribute__((no_instrument_function)) void paravirt_activate_mm(struct mm_struct *prev,
     struct mm_struct *next)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.activate_mm); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.activate_mm) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.activate_mm)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(prev)), "S" ((unsigned long)(next)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void paravirt_arch_dup_mmap(struct mm_struct *oldmm,
       struct mm_struct *mm)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.dup_mmap); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.dup_mmap) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.dup_mmap)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(oldmm)), "S" ((unsigned long)(mm)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void paravirt_arch_exit_mmap(struct mm_struct *mm)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.exit_mmap); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.exit_mmap) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.exit_mmap)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(mm)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void __flush_tlb(void)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.flush_tlb_user); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.flush_tlb_user) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.flush_tlb_user)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}
static inline __attribute__((no_instrument_function)) void __flush_tlb_global(void)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.flush_tlb_kernel); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.flush_tlb_kernel) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.flush_tlb_kernel)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}
static inline __attribute__((no_instrument_function)) void __flush_tlb_single(unsigned long addr)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.flush_tlb_single); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.flush_tlb_single) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.flush_tlb_single)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(addr)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void flush_tlb_others(const struct cpumask *cpumask,
        struct mm_struct *mm,
        unsigned long start,
        unsigned long end)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.flush_tlb_others); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.flush_tlb_others) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.flush_tlb_others)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(cpumask)), "S" ((unsigned long)(mm)), "d" ((unsigned long)(start)), "c" ((unsigned long)(end)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) int paravirt_pgd_alloc(struct mm_struct *mm)
{
 return ({ int __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.pgd_alloc); if (sizeof(int) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pgd_alloc) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pgd_alloc)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(mm)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (int)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pgd_alloc) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pgd_alloc)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(mm)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (int)__eax; } __ret; });
}

static inline __attribute__((no_instrument_function)) void paravirt_pgd_free(struct mm_struct *mm, pgd_t *pgd)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.pgd_free); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pgd_free) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pgd_free)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(mm)), "S" ((unsigned long)(pgd)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void paravirt_alloc_pte(struct mm_struct *mm, unsigned long pfn)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.alloc_pte); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.alloc_pte) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.alloc_pte)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(mm)), "S" ((unsigned long)(pfn)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}
static inline __attribute__((no_instrument_function)) void paravirt_release_pte(unsigned long pfn)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.release_pte); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.release_pte) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.release_pte)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(pfn)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void paravirt_alloc_pmd(struct mm_struct *mm, unsigned long pfn)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.alloc_pmd); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.alloc_pmd) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.alloc_pmd)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(mm)), "S" ((unsigned long)(pfn)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void paravirt_release_pmd(unsigned long pfn)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.release_pmd); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.release_pmd) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.release_pmd)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(pfn)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void paravirt_alloc_pud(struct mm_struct *mm, unsigned long pfn)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.alloc_pud); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.alloc_pud) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.alloc_pud)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(mm)), "S" ((unsigned long)(pfn)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}
static inline __attribute__((no_instrument_function)) void paravirt_release_pud(unsigned long pfn)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.release_pud); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.release_pud) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.release_pud)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(pfn)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void pte_update(struct mm_struct *mm, unsigned long addr,
         pte_t *ptep)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.pte_update); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pte_update) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pte_update)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(mm)), "S" ((unsigned long)(addr)), "d" ((unsigned long)(ptep)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}
static inline __attribute__((no_instrument_function)) void pmd_update(struct mm_struct *mm, unsigned long addr,
         pmd_t *pmdp)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.pmd_update); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pmd_update) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pmd_update)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(mm)), "S" ((unsigned long)(addr)), "d" ((unsigned long)(pmdp)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void pte_update_defer(struct mm_struct *mm, unsigned long addr,
        pte_t *ptep)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.pte_update_defer); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pte_update_defer) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pte_update_defer)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(mm)), "S" ((unsigned long)(addr)), "d" ((unsigned long)(ptep)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void pmd_update_defer(struct mm_struct *mm, unsigned long addr,
        pmd_t *pmdp)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.pmd_update_defer); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pmd_update_defer) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pmd_update_defer)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(mm)), "S" ((unsigned long)(addr)), "d" ((unsigned long)(pmdp)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) pte_t __pte(pteval_t val)
{
 pteval_t ret;

 if (sizeof(pteval_t) > sizeof(long))
  ret = ({ pteval_t __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.make_pte.func); if (sizeof(pteval_t) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.make_pte.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.make_pte.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(val)), "S" ((unsigned long)((u64)val >> 32)) : "memory", "cc" ); __ret = (pteval_t)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.make_pte.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.make_pte.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(val)), "S" ((unsigned long)((u64)val >> 32)) : "memory", "cc" ); __ret = (pteval_t)__eax; } __ret; })

                           ;
 else
  ret = ({ pteval_t __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.make_pte.func); if (sizeof(pteval_t) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.make_pte.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.make_pte.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(val)) : "memory", "cc" ); __ret = (pteval_t)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.make_pte.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.make_pte.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(val)) : "memory", "cc" ); __ret = (pteval_t)__eax; } __ret; })

           ;

 return (pte_t) { .pte = ret };
}

static inline __attribute__((no_instrument_function)) pteval_t pte_val(pte_t pte)
{
 pteval_t ret;

 if (sizeof(pteval_t) > sizeof(long))
  ret = ({ pteval_t __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.pte_val.func); if (sizeof(pteval_t) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pte_val.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pte_val.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(pte.pte)), "S" ((unsigned long)((u64)pte.pte >> 32)) : "memory", "cc" ); __ret = (pteval_t)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pte_val.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pte_val.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(pte.pte)), "S" ((unsigned long)((u64)pte.pte >> 32)) : "memory", "cc" ); __ret = (pteval_t)__eax; } __ret; })
                                   ;
 else
  ret = ({ pteval_t __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.pte_val.func); if (sizeof(pteval_t) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pte_val.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pte_val.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(pte.pte)) : "memory", "cc" ); __ret = (pteval_t)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pte_val.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pte_val.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(pte.pte)) : "memory", "cc" ); __ret = (pteval_t)__eax; } __ret; })
               ;

 return ret;
}

static inline __attribute__((no_instrument_function)) pgd_t __pgd(pgdval_t val)
{
 pgdval_t ret;

 if (sizeof(pgdval_t) > sizeof(long))
  ret = ({ pgdval_t __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.make_pgd.func); if (sizeof(pgdval_t) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.make_pgd.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.make_pgd.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(val)), "S" ((unsigned long)((u64)val >> 32)) : "memory", "cc" ); __ret = (pgdval_t)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.make_pgd.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.make_pgd.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(val)), "S" ((unsigned long)((u64)val >> 32)) : "memory", "cc" ); __ret = (pgdval_t)__eax; } __ret; })
                           ;
 else
  ret = ({ pgdval_t __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.make_pgd.func); if (sizeof(pgdval_t) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.make_pgd.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.make_pgd.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(val)) : "memory", "cc" ); __ret = (pgdval_t)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.make_pgd.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.make_pgd.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(val)) : "memory", "cc" ); __ret = (pgdval_t)__eax; } __ret; })
           ;

 return (pgd_t) { ret };
}

static inline __attribute__((no_instrument_function)) pgdval_t pgd_val(pgd_t pgd)
{
 pgdval_t ret;

 if (sizeof(pgdval_t) > sizeof(long))
  ret = ({ pgdval_t __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.pgd_val.func); if (sizeof(pgdval_t) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pgd_val.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pgd_val.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(pgd.pgd)), "S" ((unsigned long)((u64)pgd.pgd >> 32)) : "memory", "cc" ); __ret = (pgdval_t)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pgd_val.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pgd_val.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(pgd.pgd)), "S" ((unsigned long)((u64)pgd.pgd >> 32)) : "memory", "cc" ); __ret = (pgdval_t)__eax; } __ret; })
                                    ;
 else
  ret = ({ pgdval_t __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.pgd_val.func); if (sizeof(pgdval_t) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pgd_val.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pgd_val.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(pgd.pgd)) : "memory", "cc" ); __ret = (pgdval_t)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pgd_val.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pgd_val.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(pgd.pgd)) : "memory", "cc" ); __ret = (pgdval_t)__eax; } __ret; })
                ;

 return ret;
}


static inline __attribute__((no_instrument_function)) pte_t ptep_modify_prot_start(struct mm_struct *mm, unsigned long addr,
        pte_t *ptep)
{
 pteval_t ret;

 ret = ({ pteval_t __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.ptep_modify_prot_start); if (sizeof(pteval_t) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.ptep_modify_prot_start) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.ptep_modify_prot_start)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(mm)), "S" ((unsigned long)(addr)), "d" ((unsigned long)(ptep)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (pteval_t)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx), "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.ptep_modify_prot_start) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.ptep_modify_prot_start)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(mm)), "S" ((unsigned long)(addr)), "d" ((unsigned long)(ptep)) : "memory", "cc" , "r8", "r9", "r10", "r11"); __ret = (pteval_t)__eax; } __ret; })
                   ;

 return (pte_t) { .pte = ret };
}

static inline __attribute__((no_instrument_function)) void ptep_modify_prot_commit(struct mm_struct *mm, unsigned long addr,
        pte_t *ptep, pte_t pte)
{
 if (sizeof(pteval_t) > sizeof(long))

  pv_mmu_ops.ptep_modify_prot_commit(mm, addr, ptep, pte);
 else
  ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.ptep_modify_prot_commit); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.ptep_modify_prot_commit) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.ptep_modify_prot_commit)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(mm)), "S" ((unsigned long)(addr)), "d" ((unsigned long)(ptep)), "c" ((unsigned long)(pte.pte)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); })
                               ;
}

static inline __attribute__((no_instrument_function)) void set_pte(pte_t *ptep, pte_t pte)
{
 if (sizeof(pteval_t) > sizeof(long))
  ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.set_pte); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.set_pte) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.set_pte)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(ptep)), "S" ((unsigned long)(pte.pte)), "d" ((unsigned long)((u64)pte.pte >> 32)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); })
                                   ;
 else
  ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.set_pte); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.set_pte) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.set_pte)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(ptep)), "S" ((unsigned long)(pte.pte)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); })
               ;
}

static inline __attribute__((no_instrument_function)) void set_pte_at(struct mm_struct *mm, unsigned long addr,
         pte_t *ptep, pte_t pte)
{
 if (sizeof(pteval_t) > sizeof(long))

  pv_mmu_ops.set_pte_at(mm, addr, ptep, pte);
 else
  ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.set_pte_at); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.set_pte_at) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.set_pte_at)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(mm)), "S" ((unsigned long)(addr)), "d" ((unsigned long)(ptep)), "c" ((unsigned long)(pte.pte)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void set_pmd_at(struct mm_struct *mm, unsigned long addr,
         pmd_t *pmdp, pmd_t pmd)
{
 if (sizeof(pmdval_t) > sizeof(long))

  pv_mmu_ops.set_pmd_at(mm, addr, pmdp, pmd);
 else
  ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.set_pmd_at); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.set_pmd_at) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.set_pmd_at)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(mm)), "S" ((unsigned long)(addr)), "d" ((unsigned long)(pmdp)), "c" ((unsigned long)(native_pmd_val(pmd))) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); })
                           ;
}

static inline __attribute__((no_instrument_function)) void set_pmd(pmd_t *pmdp, pmd_t pmd)
{
 pmdval_t val = native_pmd_val(pmd);

 if (sizeof(pmdval_t) > sizeof(long))
  ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.set_pmd); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.set_pmd) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.set_pmd)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(pmdp)), "S" ((unsigned long)(val)), "d" ((unsigned long)((u64)val >> 32)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
 else
  ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.set_pmd); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.set_pmd) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.set_pmd)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(pmdp)), "S" ((unsigned long)(val)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}


static inline __attribute__((no_instrument_function)) pmd_t __pmd(pmdval_t val)
{
 pmdval_t ret;

 if (sizeof(pmdval_t) > sizeof(long))
  ret = ({ pmdval_t __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.make_pmd.func); if (sizeof(pmdval_t) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.make_pmd.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.make_pmd.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(val)), "S" ((unsigned long)((u64)val >> 32)) : "memory", "cc" ); __ret = (pmdval_t)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.make_pmd.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.make_pmd.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(val)), "S" ((unsigned long)((u64)val >> 32)) : "memory", "cc" ); __ret = (pmdval_t)__eax; } __ret; })
                           ;
 else
  ret = ({ pmdval_t __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.make_pmd.func); if (sizeof(pmdval_t) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.make_pmd.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.make_pmd.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(val)) : "memory", "cc" ); __ret = (pmdval_t)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.make_pmd.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.make_pmd.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(val)) : "memory", "cc" ); __ret = (pmdval_t)__eax; } __ret; })
           ;

 return (pmd_t) { ret };
}

static inline __attribute__((no_instrument_function)) pmdval_t pmd_val(pmd_t pmd)
{
 pmdval_t ret;

 if (sizeof(pmdval_t) > sizeof(long))
  ret = ({ pmdval_t __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.pmd_val.func); if (sizeof(pmdval_t) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pmd_val.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pmd_val.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(pmd.pmd)), "S" ((unsigned long)((u64)pmd.pmd >> 32)) : "memory", "cc" ); __ret = (pmdval_t)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pmd_val.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pmd_val.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(pmd.pmd)), "S" ((unsigned long)((u64)pmd.pmd >> 32)) : "memory", "cc" ); __ret = (pmdval_t)__eax; } __ret; })
                                    ;
 else
  ret = ({ pmdval_t __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.pmd_val.func); if (sizeof(pmdval_t) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pmd_val.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pmd_val.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(pmd.pmd)) : "memory", "cc" ); __ret = (pmdval_t)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pmd_val.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pmd_val.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(pmd.pmd)) : "memory", "cc" ); __ret = (pmdval_t)__eax; } __ret; })
                ;

 return ret;
}

static inline __attribute__((no_instrument_function)) void set_pud(pud_t *pudp, pud_t pud)
{
 pudval_t val = native_pud_val(pud);

 if (sizeof(pudval_t) > sizeof(long))
  ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.set_pud); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.set_pud) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.set_pud)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(pudp)), "S" ((unsigned long)(val)), "d" ((unsigned long)((u64)val >> 32)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); })
                           ;
 else
  ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.set_pud); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.set_pud) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.set_pud)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(pudp)), "S" ((unsigned long)(val)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); })
           ;
}

static inline __attribute__((no_instrument_function)) pud_t __pud(pudval_t val)
{
 pudval_t ret;

 if (sizeof(pudval_t) > sizeof(long))
  ret = ({ pudval_t __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.make_pud.func); if (sizeof(pudval_t) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.make_pud.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.make_pud.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(val)), "S" ((unsigned long)((u64)val >> 32)) : "memory", "cc" ); __ret = (pudval_t)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.make_pud.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.make_pud.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(val)), "S" ((unsigned long)((u64)val >> 32)) : "memory", "cc" ); __ret = (pudval_t)__eax; } __ret; })
                           ;
 else
  ret = ({ pudval_t __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.make_pud.func); if (sizeof(pudval_t) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.make_pud.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.make_pud.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(val)) : "memory", "cc" ); __ret = (pudval_t)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.make_pud.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.make_pud.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(val)) : "memory", "cc" ); __ret = (pudval_t)__eax; } __ret; })
           ;

 return (pud_t) { ret };
}

static inline __attribute__((no_instrument_function)) pudval_t pud_val(pud_t pud)
{
 pudval_t ret;

 if (sizeof(pudval_t) > sizeof(long))
  ret = ({ pudval_t __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.pud_val.func); if (sizeof(pudval_t) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pud_val.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pud_val.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(pud.pud)), "S" ((unsigned long)((u64)pud.pud >> 32)) : "memory", "cc" ); __ret = (pudval_t)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pud_val.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pud_val.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(pud.pud)), "S" ((unsigned long)((u64)pud.pud >> 32)) : "memory", "cc" ); __ret = (pudval_t)__eax; } __ret; })
                                    ;
 else
  ret = ({ pudval_t __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.pud_val.func); if (sizeof(pudval_t) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pud_val.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pud_val.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(pud.pud)) : "memory", "cc" ); __ret = (pudval_t)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.pud_val.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.pud_val.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(pud.pud)) : "memory", "cc" ); __ret = (pudval_t)__eax; } __ret; })
                ;

 return ret;
}

static inline __attribute__((no_instrument_function)) void set_pgd(pgd_t *pgdp, pgd_t pgd)
{
 pgdval_t val = native_pgd_val(pgd);

 if (sizeof(pgdval_t) > sizeof(long))
  ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.set_pgd); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.set_pgd) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.set_pgd)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(pgdp)), "S" ((unsigned long)(val)), "d" ((unsigned long)((u64)val >> 32)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); })
                           ;
 else
  ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.set_pgd); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.set_pgd) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.set_pgd)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(pgdp)), "S" ((unsigned long)(val)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); })
           ;
}

static inline __attribute__((no_instrument_function)) void pgd_clear(pgd_t *pgdp)
{
 set_pgd(pgdp, __pgd(0));
}

static inline __attribute__((no_instrument_function)) void pud_clear(pud_t *pudp)
{
 set_pud(pudp, __pud(0));
}
# 663 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/paravirt.h"
static inline __attribute__((no_instrument_function)) void set_pte_atomic(pte_t *ptep, pte_t pte)
{
 set_pte(ptep, pte);
}

static inline __attribute__((no_instrument_function)) void pte_clear(struct mm_struct *mm, unsigned long addr,
        pte_t *ptep)
{
 set_pte_at(mm, addr, ptep, __pte(0));
}

static inline __attribute__((no_instrument_function)) void pmd_clear(pmd_t *pmdp)
{
 set_pmd(pmdp, __pmd(0));
}



static inline __attribute__((no_instrument_function)) void arch_start_context_switch(struct task_struct *prev)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.start_context_switch); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.start_context_switch) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.start_context_switch)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(prev)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void arch_end_context_switch(struct task_struct *next)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_cpu_ops.end_context_switch); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_cpu_ops.end_context_switch) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_cpu_ops.end_context_switch)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(next)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}


static inline __attribute__((no_instrument_function)) void arch_enter_lazy_mmu_mode(void)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.lazy_mode.enter); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.lazy_mode.enter) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.lazy_mode.enter)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void arch_leave_lazy_mmu_mode(void)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.lazy_mode.leave); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.lazy_mode.leave) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.lazy_mode.leave)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void arch_flush_lazy_mmu_mode(void)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_mmu_ops.lazy_mode.flush); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.lazy_mode.flush) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_mmu_ops.lazy_mode.flush)), [paravirt_clobber] "i" (((1 << 9) - 1)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}

static inline __attribute__((no_instrument_function)) void __set_fixmap(unsigned idx,
    phys_addr_t phys, pgprot_t flags)
{
 pv_mmu_ops.set_fixmap(idx, phys, flags);
}



static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) void __ticket_lock_spinning(struct arch_spinlock *lock,
       __ticket_t ticket)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_lock_ops.lock_spinning.func); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_lock_ops.lock_spinning.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_lock_ops.lock_spinning.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(lock)), "S" ((unsigned long)(ticket)) : "memory", "cc" ); });
}

static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) void __ticket_unlock_kick(struct arch_spinlock *lock,
       __ticket_t ticket)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_lock_ops.unlock_kick); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=D" (__edi), "=S" (__esi), "=d" (__edx), "=c" (__ecx) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_lock_ops.unlock_kick) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_lock_ops.unlock_kick)), [paravirt_clobber] "i" (((1 << 9) - 1)), "D" ((unsigned long)(lock)), "S" ((unsigned long)(ticket)) : "memory", "cc" , "rax", "r8", "r9", "r10", "r11"); });
}
# 802 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/paravirt.h"
static inline __attribute__((no_instrument_function)) __attribute__((no_instrument_function)) unsigned long arch_local_save_flags(void)
{
 return ({ unsigned long __ret; unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_irq_ops.save_fl.func); if (sizeof(unsigned long) > sizeof(unsigned long)) { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_irq_ops.save_fl.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_irq_ops.save_fl.func)), [paravirt_clobber] "i" (((1 << 0))) : "memory", "cc" ); __ret = (unsigned long)((((u64)__edx) << 32) | __eax); } else { asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_irq_ops.save_fl.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_irq_ops.save_fl.func)), [paravirt_clobber] "i" (((1 << 0))) : "memory", "cc" ); __ret = (unsigned long)__eax; } __ret; });
}

static inline __attribute__((no_instrument_function)) __attribute__((no_instrument_function)) void arch_local_irq_restore(unsigned long f)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_irq_ops.restore_fl.func); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_irq_ops.restore_fl.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_irq_ops.restore_fl.func)), [paravirt_clobber] "i" (((1 << 0))), "D" ((unsigned long)(f)) : "memory", "cc" ); });
}

static inline __attribute__((no_instrument_function)) __attribute__((no_instrument_function)) void arch_local_irq_disable(void)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_irq_ops.irq_disable.func); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_irq_ops.irq_disable.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_irq_ops.irq_disable.func)), [paravirt_clobber] "i" (((1 << 0))) : "memory", "cc" ); });
}

static inline __attribute__((no_instrument_function)) __attribute__((no_instrument_function)) void arch_local_irq_enable(void)
{
 ({ unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax; ((void)pv_irq_ops.irq_enable.func); asm volatile("" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "" : "=a" (__eax) : [paravirt_typenum] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_irq_ops.irq_enable.func) / sizeof(void *))), [paravirt_opptr] "i" (&(pv_irq_ops.irq_enable.func)), [paravirt_clobber] "i" (((1 << 0))) : "memory", "cc" ); });
}

static inline __attribute__((no_instrument_function)) __attribute__((no_instrument_function)) unsigned long arch_local_irq_save(void)
{
 unsigned long f;

 f = arch_local_save_flags();
 arch_local_irq_disable();
 return f;
}
# 847 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/paravirt.h"
extern void default_banner(void);
# 132 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/msr.h" 2
# 215 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/msr.h"
struct msr *msrs_alloc(void);
void msrs_free(struct msr *msrs);
int msr_set_bit(u32 msr, u8 bit);
int msr_clear_bit(u32 msr, u8 bit);


int rdmsr_on_cpu(unsigned int cpu, u32 msr_no, u32 *l, u32 *h);
int wrmsr_on_cpu(unsigned int cpu, u32 msr_no, u32 l, u32 h);
int rdmsrl_on_cpu(unsigned int cpu, u32 msr_no, u64 *q);
int wrmsrl_on_cpu(unsigned int cpu, u32 msr_no, u64 q);
void rdmsr_on_cpus(const struct cpumask *mask, u32 msr_no, struct msr *msrs);
void wrmsr_on_cpus(const struct cpumask *mask, u32 msr_no, struct msr *msrs);
int rdmsr_safe_on_cpu(unsigned int cpu, u32 msr_no, u32 *l, u32 *h);
int wrmsr_safe_on_cpu(unsigned int cpu, u32 msr_no, u32 l, u32 h);
int rdmsrl_safe_on_cpu(unsigned int cpu, u32 msr_no, u64 *q);
int wrmsrl_safe_on_cpu(unsigned int cpu, u32 msr_no, u64 q);
int rdmsr_safe_regs_on_cpu(unsigned int cpu, u32 regs[8]);
int wrmsr_safe_regs_on_cpu(unsigned int cpu, u32 regs[8]);
# 21 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h" 2


# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/special_insns.h" 1






static inline __attribute__((no_instrument_function)) void native_clts(void)
{
 asm volatile("clts");
}
# 19 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/special_insns.h"
extern unsigned long __force_order;

static inline __attribute__((no_instrument_function)) unsigned long native_read_cr0(void)
{
 unsigned long val;
 asm volatile("mov %%cr0,%0\n\t" : "=r" (val), "=m" (__force_order));
 return val;
}

static inline __attribute__((no_instrument_function)) void native_write_cr0(unsigned long val)
{
 asm volatile("mov %0,%%cr0": : "r" (val), "m" (__force_order));
}

static inline __attribute__((no_instrument_function)) unsigned long native_read_cr2(void)
{
 unsigned long val;
 asm volatile("mov %%cr2,%0\n\t" : "=r" (val), "=m" (__force_order));
 return val;
}

static inline __attribute__((no_instrument_function)) void native_write_cr2(unsigned long val)
{
 asm volatile("mov %0,%%cr2": : "r" (val), "m" (__force_order));
}

static inline __attribute__((no_instrument_function)) unsigned long native_read_cr3(void)
{
 unsigned long val;
 asm volatile("mov %%cr3,%0\n\t" : "=r" (val), "=m" (__force_order));
 return val;
}

static inline __attribute__((no_instrument_function)) void native_write_cr3(unsigned long val)
{
 asm volatile("mov %0,%%cr3": : "r" (val), "m" (__force_order));
}

static inline __attribute__((no_instrument_function)) unsigned long native_read_cr4(void)
{
 unsigned long val;
 asm volatile("mov %%cr4,%0\n\t" : "=r" (val), "=m" (__force_order));
 return val;
}

static inline __attribute__((no_instrument_function)) unsigned long native_read_cr4_safe(void)
{
 unsigned long val;
# 75 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/special_insns.h"
 val = native_read_cr4();

 return val;
}

static inline __attribute__((no_instrument_function)) void native_write_cr4(unsigned long val)
{
 asm volatile("mov %0,%%cr4": : "r" (val), "m" (__force_order));
}


static inline __attribute__((no_instrument_function)) unsigned long native_read_cr8(void)
{
 unsigned long cr8;
 asm volatile("movq %%cr8,%0" : "=r" (cr8));
 return cr8;
}

static inline __attribute__((no_instrument_function)) void native_write_cr8(unsigned long val)
{
 asm volatile("movq %0,%%cr8" :: "r" (val) : "memory");
}


static inline __attribute__((no_instrument_function)) void native_wbinvd(void)
{
 asm volatile("wbinvd": : :"memory");
}

extern void native_load_gs_index(unsigned);
# 189 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/special_insns.h"
static inline __attribute__((no_instrument_function)) void clflush(volatile void *__p)
{
 asm volatile("clflush %0" : "+m" (*(volatile char *)__p));
}

static inline __attribute__((no_instrument_function)) void clflushopt(volatile void *__p)
{
 asm volatile ("661:\n\t" ".byte " "0x3e" "; clflush %P0" "\n662:\n" ".pushsection .altinstructions,\"a\"\n" " .long 661b - .\n" " .long " "663""1""f - .\n" " .word " "( 9*32+23)" "\n" " .byte " "662b-661b" "\n" " .byte " "664""1""f-""663""1""f" "\n" ".popsection\n" ".pushsection .discard,\"aw\",@progbits\n" " .byte 0xff + (" "664""1""f-""663""1""f" ") - (" "662b-661b" ")\n" ".popsection\n" ".pushsection .altinstr_replacement, \"ax\"\n" "663""1"":\n\t" ".byte 0x66; clflush %P0" "\n" "664""1" ":\n\t" ".popsection" : "+m" (*(volatile char *)__p) : "i" (0))


                                              ;
}
# 24 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h" 2

# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/personality.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/personality.h" 1
# 10 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/personality.h"
enum {
 UNAME26 = 0x0020000,
 ADDR_NO_RANDOMIZE = 0x0040000,
 FDPIC_FUNCPTRS = 0x0080000,


 MMAP_PAGE_ZERO = 0x0100000,
 ADDR_COMPAT_LAYOUT = 0x0200000,
 READ_IMPLIES_EXEC = 0x0400000,
 ADDR_LIMIT_32BIT = 0x0800000,
 SHORT_INODE = 0x1000000,
 WHOLE_SECONDS = 0x2000000,
 STICKY_TIMEOUTS = 0x4000000,
 ADDR_LIMIT_3GB = 0x8000000,
};
# 41 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/personality.h"
enum {
 PER_LINUX = 0x0000,
 PER_LINUX_32BIT = 0x0000 | ADDR_LIMIT_32BIT,
 PER_LINUX_FDPIC = 0x0000 | FDPIC_FUNCPTRS,
 PER_SVR4 = 0x0001 | STICKY_TIMEOUTS | MMAP_PAGE_ZERO,
 PER_SVR3 = 0x0002 | STICKY_TIMEOUTS | SHORT_INODE,
 PER_SCOSVR3 = 0x0003 | STICKY_TIMEOUTS |
      WHOLE_SECONDS | SHORT_INODE,
 PER_OSR5 = 0x0003 | STICKY_TIMEOUTS | WHOLE_SECONDS,
 PER_WYSEV386 = 0x0004 | STICKY_TIMEOUTS | SHORT_INODE,
 PER_ISCR4 = 0x0005 | STICKY_TIMEOUTS,
 PER_BSD = 0x0006,
 PER_SUNOS = 0x0006 | STICKY_TIMEOUTS,
 PER_XENIX = 0x0007 | STICKY_TIMEOUTS | SHORT_INODE,
 PER_LINUX32 = 0x0008,
 PER_LINUX32_3GB = 0x0008 | ADDR_LIMIT_3GB,
 PER_IRIX32 = 0x0009 | STICKY_TIMEOUTS,
 PER_IRIXN32 = 0x000a | STICKY_TIMEOUTS,
 PER_IRIX64 = 0x000b | STICKY_TIMEOUTS,
 PER_RISCOS = 0x000c,
 PER_SOLARIS = 0x000d | STICKY_TIMEOUTS,
 PER_UW7 = 0x000e | STICKY_TIMEOUTS | MMAP_PAGE_ZERO,
 PER_OSF4 = 0x000f,
 PER_HPUX = 0x0010,
 PER_MASK = 0x00ff,
};
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/personality.h" 2






struct exec_domain;
struct pt_regs;

extern int register_exec_domain(struct exec_domain *);
extern int unregister_exec_domain(struct exec_domain *);
extern int __set_personality(unsigned int);
# 25 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/personality.h"
typedef void (*handler_t)(int, struct pt_regs *);

struct exec_domain {
 const char *name;
 handler_t handler;
 unsigned char pers_low;
 unsigned char pers_high;
 unsigned long *signal_map;
 unsigned long *signal_invmap;
 struct map_segment *err_map;
 struct map_segment *socktype_map;
 struct map_segment *sockopt_map;
 struct map_segment *af_map;
 struct module *module;
 struct exec_domain *next;
};
# 26 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h" 2



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/math64.h" 1




# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/div64.h" 1
# 63 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/div64.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/div64.h" 1
# 64 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/div64.h" 2
# 6 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/math64.h" 2
# 18 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/math64.h"
static inline __attribute__((no_instrument_function)) u64 div_u64_rem(u64 dividend, u32 divisor, u32 *remainder)
{
 *remainder = dividend % divisor;
 return dividend / divisor;
}




static inline __attribute__((no_instrument_function)) s64 div_s64_rem(s64 dividend, s32 divisor, s32 *remainder)
{
 *remainder = dividend % divisor;
 return dividend / divisor;
}




static inline __attribute__((no_instrument_function)) u64 div64_u64_rem(u64 dividend, u64 divisor, u64 *remainder)
{
 *remainder = dividend % divisor;
 return dividend / divisor;
}




static inline __attribute__((no_instrument_function)) u64 div64_u64(u64 dividend, u64 divisor)
{
 return dividend / divisor;
}




static inline __attribute__((no_instrument_function)) s64 div64_s64(s64 dividend, s64 divisor)
{
 return dividend / divisor;
}
# 97 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/math64.h"
static inline __attribute__((no_instrument_function)) u64 div_u64(u64 dividend, u32 divisor)
{
 u32 remainder;
 return div_u64_rem(dividend, divisor, &remainder);
}






static inline __attribute__((no_instrument_function)) s64 div_s64(s64 dividend, s32 divisor)
{
 s32 remainder;
 return div_s64_rem(dividend, divisor, &remainder);
}


u32 iter_div_u64_rem(u64 dividend, u32 divisor, u64 *remainder);

static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) u32
__iter_div_u64_rem(u64 dividend, u32 divisor, u64 *remainder)
{
 u32 ret = 0;

 while (dividend >= divisor) {


  asm("" : "+rm"(dividend));

  dividend -= divisor;
  ret++;
 }

 *remainder = dividend;

 return ret;
}
# 148 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/math64.h"
static inline __attribute__((no_instrument_function)) u64 mul_u64_u32_shr(u64 a, u32 mul, unsigned int shift)
{
 u32 ah, al;
 u64 ret;

 al = a;
 ah = a >> 32;

 ret = ((u64)al * mul) >> shift;
 if (ah)
  ret += ((u64)ah * mul) << (32 - shift);

 return ret;
}
# 30 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/err.h" 1






# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/errno.h" 1
# 8 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/err.h" 2
# 23 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/err.h"
static inline __attribute__((no_instrument_function)) void * ERR_PTR(long error)
{
 return (void *) error;
}

static inline __attribute__((no_instrument_function)) long PTR_ERR( const void *ptr)
{
 return (long) ptr;
}

static inline __attribute__((no_instrument_function)) bool IS_ERR( const void *ptr)
{
 return __builtin_expect(!!(((unsigned long)ptr) >= (unsigned long)-4095), 0);
}

static inline __attribute__((no_instrument_function)) bool IS_ERR_OR_NULL( const void *ptr)
{
 return !ptr || __builtin_expect(!!(((unsigned long)ptr) >= (unsigned long)-4095), 0);
}
# 50 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/err.h"
static inline __attribute__((no_instrument_function)) void * ERR_CAST( const void *ptr)
{

 return (void *) ptr;
}

static inline __attribute__((no_instrument_function)) int PTR_ERR_OR_ZERO( const void *ptr)
{
 if (IS_ERR(ptr))
  return PTR_ERR(ptr);
 else
  return 0;
}
# 31 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/irqflags.h" 1
# 15 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/irqflags.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/irqflags.h" 1
# 11 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/irqflags.h"
static inline __attribute__((no_instrument_function)) unsigned long native_save_fl(void)
{
 unsigned long flags;






 asm volatile("# __raw_save_flags\n\t"
       "pushf ; pop %0"
       : "=rm" (flags)
       :
       : "memory");

 return flags;
}

static inline __attribute__((no_instrument_function)) void native_restore_fl(unsigned long flags)
{
 asm volatile("push %0 ; popf"
       :
       :"g" (flags)
       :"memory", "cc");
}

static inline __attribute__((no_instrument_function)) void native_irq_disable(void)
{
 asm volatile("cli": : :"memory");
}

static inline __attribute__((no_instrument_function)) void native_irq_enable(void)
{
 asm volatile("sti": : :"memory");
}

static inline __attribute__((no_instrument_function)) void native_safe_halt(void)
{
 asm volatile("sti; hlt": : :"memory");
}

static inline __attribute__((no_instrument_function)) void native_halt(void)
{
 asm volatile("hlt": : :"memory");
}
# 155 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/irqflags.h"
static inline __attribute__((no_instrument_function)) int arch_irqs_disabled_flags(unsigned long flags)
{
 return !(flags & ((1UL) << (9)));
}

static inline __attribute__((no_instrument_function)) int arch_irqs_disabled(void)
{
 unsigned long flags = arch_local_save_flags();

 return arch_irqs_disabled_flags(flags);
}
# 16 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/irqflags.h" 2
# 32 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h" 2
# 46 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h"
static inline __attribute__((no_instrument_function)) void *current_text_addr(void)
{
 void *pc;

 asm volatile("mov $1f, %0; 1:":"=r" (pc));

 return pc;
}
# 63 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h"
enum tlb_infos {
 ENTRIES,
 NR_INFO
};

extern u16 __attribute__((__section__(".data..read_mostly"))) tlb_lli_4k[NR_INFO];
extern u16 __attribute__((__section__(".data..read_mostly"))) tlb_lli_2m[NR_INFO];
extern u16 __attribute__((__section__(".data..read_mostly"))) tlb_lli_4m[NR_INFO];
extern u16 __attribute__((__section__(".data..read_mostly"))) tlb_lld_4k[NR_INFO];
extern u16 __attribute__((__section__(".data..read_mostly"))) tlb_lld_2m[NR_INFO];
extern u16 __attribute__((__section__(".data..read_mostly"))) tlb_lld_4m[NR_INFO];
extern u16 __attribute__((__section__(".data..read_mostly"))) tlb_lld_1g[NR_INFO];







struct cpuinfo_x86 {
 __u8 x86;
 __u8 x86_vendor;
 __u8 x86_model;
 __u8 x86_mask;
# 96 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h"
 int x86_tlbsize;

 __u8 x86_virt_bits;
 __u8 x86_phys_bits;

 __u8 x86_coreid_bits;

 __u32 extended_cpuid_level;

 int cpuid_level;
 __u32 x86_capability[11 + 1];
 char x86_vendor_id[16];
 char x86_model_id[64];

 int x86_cache_size;
 int x86_cache_alignment;
 int x86_power;
 unsigned long loops_per_jiffy;

 u16 x86_max_cores;
 u16 apicid;
 u16 initial_apicid;
 u16 x86_clflush_size;

 u16 booted_cores;

 u16 phys_proc_id;

 u16 cpu_core_id;

 u8 compute_unit_id;

 u16 cpu_index;
 u32 microcode;
};
# 146 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h"
extern struct cpuinfo_x86 boot_cpu_data;
extern struct cpuinfo_x86 new_cpu_data;

extern struct tss_struct doublefault_tss;
extern __u32 cpu_caps_cleared[11];
extern __u32 cpu_caps_set[11];


extern __attribute__((section(".data..percpu" "..read_mostly"))) __typeof__(struct cpuinfo_x86) cpu_info;






extern const struct seq_operations cpuinfo_op;



extern void cpu_detect(struct cpuinfo_x86 *c);
extern void fpu_detect(struct cpuinfo_x86 *c);

extern void early_cpu_init(void);
extern void identify_boot_cpu(void);
extern void identify_secondary_cpu(struct cpuinfo_x86 *);
extern void print_cpu_info(struct cpuinfo_x86 *);
void print_cpu_msr(struct cpuinfo_x86 *);
extern void init_scattered_cpuid_features(struct cpuinfo_x86 *c);
extern unsigned int init_intel_cacheinfo(struct cpuinfo_x86 *c);
extern void init_amd_cacheinfo(struct cpuinfo_x86 *c);

extern void detect_extended_topology(struct cpuinfo_x86 *c);
extern void detect_ht(struct cpuinfo_x86 *c);




static inline __attribute__((no_instrument_function)) int have_cpuid_p(void)
{
 return 1;
}

static inline __attribute__((no_instrument_function)) void native_cpuid(unsigned int *eax, unsigned int *ebx,
    unsigned int *ecx, unsigned int *edx)
{

 asm volatile("cpuid"
     : "=a" (*eax),
       "=b" (*ebx),
       "=c" (*ecx),
       "=d" (*edx)
     : "0" (*eax), "2" (*ecx)
     : "memory");
}

static inline __attribute__((no_instrument_function)) void load_cr3(pgd_t *pgdir)
{
 write_cr3(__phys_addr_nodebug((unsigned long)(pgdir)));
}
# 240 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h"
struct x86_hw_tss {
 u32 reserved1;
 u64 sp0;
 u64 sp1;
 u64 sp2;
 u64 reserved2;
 u64 ist[7];
 u32 reserved3;
 u32 reserved4;
 u16 reserved5;
 u16 io_bitmap_base;

} __attribute__((packed)) __attribute__((__aligned__((1 << (6)))));
# 264 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h"
struct tss_struct {



 struct x86_hw_tss x86_tss;







 unsigned long io_bitmap[((65536/8)/sizeof(long)) + 1];




 unsigned long stack[64];

} __attribute__((__aligned__((1 << (6)))));

extern __attribute__((section(".data..percpu" "..shared_aligned"))) __typeof__(struct tss_struct) init_tss __attribute__((__aligned__((1 << (6)))));




struct orig_ist {
 unsigned long ist[7];
};



struct i387_fsave_struct {
 u32 cwd;
 u32 swd;
 u32 twd;
 u32 fip;
 u32 fcs;
 u32 foo;
 u32 fos;


 u32 st_space[20];


 u32 status;
};

struct i387_fxsave_struct {
 u16 cwd;
 u16 swd;
 u16 twd;
 u16 fop;
 union {
  struct {
   u64 rip;
   u64 rdp;
  };
  struct {
   u32 fip;
   u32 fcs;
   u32 foo;
   u32 fos;
  };
 };
 u32 mxcsr;
 u32 mxcsr_mask;


 u32 st_space[32];


 u32 xmm_space[64];

 u32 padding[12];

 union {
  u32 padding1[12];
  u32 sw_reserved[12];
 };

} __attribute__((aligned(16)));

struct i387_soft_struct {
 u32 cwd;
 u32 swd;
 u32 twd;
 u32 fip;
 u32 fcs;
 u32 foo;
 u32 fos;

 u32 st_space[20];
 u8 ftop;
 u8 changed;
 u8 lookahead;
 u8 no_update;
 u8 rm;
 u8 alimit;
 struct math_emu_info *info;
 u32 entry_eip;
};

struct ymmh_struct {

 u32 ymmh_space[64];
};


struct lwp_struct {
 u8 reserved[128];
};

struct bndreg {
 u64 lower_bound;
 u64 upper_bound;
} __attribute__((packed));

struct bndcsr {
 u64 bndcfgu;
 u64 bndstatus;
} __attribute__((packed));

struct xsave_hdr_struct {
 u64 xstate_bv;
 u64 xcomp_bv;
 u64 reserved[6];
} __attribute__((packed));

struct xsave_struct {
 struct i387_fxsave_struct i387;
 struct xsave_hdr_struct xsave_hdr;
 struct ymmh_struct ymmh;
 struct lwp_struct lwp;
 struct bndreg bndreg[4];
 struct bndcsr bndcsr;

} __attribute__ ((packed, aligned (64)));

union thread_xstate {
 struct i387_fsave_struct fsave;
 struct i387_fxsave_struct fxsave;
 struct i387_soft_struct soft;
 struct xsave_struct xsave;
};

struct fpu {
 unsigned int last_cpu;
 unsigned int has_fpu;
 union thread_xstate *state;
};


extern __attribute__((section(".data..percpu" ""))) __typeof__(struct orig_ist) orig_ist;

union irq_stack_union {
 char irq_stack[(((1UL) << 12) << 2)];





 struct {
  char gs_base[40];
  unsigned long stack_canary;
 };
};

extern __attribute__((section(".data..percpu" "..first"))) __typeof__(union irq_stack_union) irq_stack_union __attribute__((externally_visible));
extern typeof(irq_stack_union) init_per_cpu__irq_stack_union;

extern __attribute__((section(".data..percpu" ""))) __typeof__(char *) irq_stack_ptr;
extern __attribute__((section(".data..percpu" ""))) __typeof__(unsigned int) irq_count;
extern void ignore_sysret(void);
# 463 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h"
extern unsigned int xstate_size;
extern void free_thread_xstate(struct task_struct *);
extern struct kmem_cache *task_xstate_cachep;

struct perf_event;

struct thread_struct {

 struct desc_struct tls_array[3];
 unsigned long sp0;
 unsigned long sp;



 unsigned long usersp;
 unsigned short es;
 unsigned short ds;
 unsigned short fsindex;
 unsigned short gsindex;





 unsigned long fs;

 unsigned long gs;

 struct perf_event *ptrace_bps[4];

 unsigned long debugreg6;

 unsigned long ptrace_dr7;

 unsigned long cr2;
 unsigned long trap_nr;
 unsigned long error_code;

 struct fpu fpu;
# 513 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h"
 unsigned long *io_bitmap_ptr;
 unsigned long iopl;

 unsigned io_bitmap_max;
# 525 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h"
 unsigned char fpu_counter;
};




static inline __attribute__((no_instrument_function)) void native_set_iopl_mask(unsigned mask)
{
# 545 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h"
}

static inline __attribute__((no_instrument_function)) void
native_load_sp0(struct tss_struct *tss, struct thread_struct *thread)
{
 tss->x86_tss.sp0 = thread->sp0;







}

static inline __attribute__((no_instrument_function)) void native_swapgs(void)
{

 asm volatile("swapgs" ::: "memory");

}
# 588 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h"
extern unsigned long mmu_cr4_features;
extern u32 *trampoline_cr4_features;

static inline __attribute__((no_instrument_function)) void set_in_cr4(unsigned long mask)
{
 unsigned long cr4;

 mmu_cr4_features |= mask;
 if (trampoline_cr4_features)
  *trampoline_cr4_features = mmu_cr4_features;
 cr4 = read_cr4();
 cr4 |= mask;
 write_cr4(cr4);
}

static inline __attribute__((no_instrument_function)) void clear_in_cr4(unsigned long mask)
{
 unsigned long cr4;

 mmu_cr4_features &= ~mask;
 if (trampoline_cr4_features)
  *trampoline_cr4_features = mmu_cr4_features;
 cr4 = read_cr4();
 cr4 &= ~mask;
 write_cr4(cr4);
}

typedef struct {
 unsigned long seg;
} mm_segment_t;



extern void release_thread(struct task_struct *);

unsigned long get_wchan(struct task_struct *p);






static inline __attribute__((no_instrument_function)) void cpuid(unsigned int op,
    unsigned int *eax, unsigned int *ebx,
    unsigned int *ecx, unsigned int *edx)
{
 *eax = op;
 *ecx = 0;
 __cpuid(eax, ebx, ecx, edx);
}


static inline __attribute__((no_instrument_function)) void cpuid_count(unsigned int op, int count,
          unsigned int *eax, unsigned int *ebx,
          unsigned int *ecx, unsigned int *edx)
{
 *eax = op;
 *ecx = count;
 __cpuid(eax, ebx, ecx, edx);
}




static inline __attribute__((no_instrument_function)) unsigned int cpuid_eax(unsigned int op)
{
 unsigned int eax, ebx, ecx, edx;

 cpuid(op, &eax, &ebx, &ecx, &edx);

 return eax;
}

static inline __attribute__((no_instrument_function)) unsigned int cpuid_ebx(unsigned int op)
{
 unsigned int eax, ebx, ecx, edx;

 cpuid(op, &eax, &ebx, &ecx, &edx);

 return ebx;
}

static inline __attribute__((no_instrument_function)) unsigned int cpuid_ecx(unsigned int op)
{
 unsigned int eax, ebx, ecx, edx;

 cpuid(op, &eax, &ebx, &ecx, &edx);

 return ecx;
}

static inline __attribute__((no_instrument_function)) unsigned int cpuid_edx(unsigned int op)
{
 unsigned int eax, ebx, ecx, edx;

 cpuid(op, &eax, &ebx, &ecx, &edx);

 return edx;
}


static inline __attribute__((no_instrument_function)) void rep_nop(void)
{
 asm volatile("rep; nop" ::: "memory");
}

static inline __attribute__((no_instrument_function)) void cpu_relax(void)
{
 rep_nop();
}




static inline __attribute__((no_instrument_function)) void sync_core(void)
{
 int tmp;
# 724 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h"
 asm volatile("cpuid"
       : "=a" (tmp)
       : "0" (1)
       : "ebx", "ecx", "edx", "memory");

}

extern void select_idle_routine(const struct cpuinfo_x86 *c);
extern void init_amd_e400_c1e_mask(void);

extern unsigned long boot_option_idle_override;
extern bool amd_e400_c1e_detected;

enum idle_boot_override {IDLE_NO_OVERRIDE=0, IDLE_HALT, IDLE_NOMWAIT,
    IDLE_POLL};

extern void enable_sep_cpu(void);
extern int sysenter_setup(void);

extern void early_trap_init(void);
void early_trap_pf_init(void);


extern struct desc_ptr early_gdt_descr;

extern void cpu_set_gdt(int);
extern void switch_to_new_gdt(int);
extern void load_percpu_segment(int);
extern void cpu_init(void);

static inline __attribute__((no_instrument_function)) unsigned long get_debugctlmsr(void)
{
 unsigned long debugctlmsr = 0;





 do { int _err; debugctlmsr = paravirt_read_msr(0x000001d9, &_err); } while (0);

 return debugctlmsr;
}

static inline __attribute__((no_instrument_function)) void update_debugctlmsr(unsigned long debugctlmsr)
{




 do { paravirt_write_msr(0x000001d9, (u32)((u64)(debugctlmsr)), ((u64)(debugctlmsr))>>32); } while (0);
}

extern void set_task_blockstep(struct task_struct *task, bool on);





extern unsigned int machine_id;
extern unsigned int machine_submodel_id;
extern unsigned int BIOS_revision;


extern int bootloader_type;
extern int bootloader_version;

extern char ignore_fpu_irq;
# 809 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h"
static inline __attribute__((no_instrument_function)) void prefetch(const void *x)
{
 asm volatile ("661:\n\t" "prefetcht0 (%1)" "\n662:\n" ".pushsection .altinstructions,\"a\"\n" " .long 661b - .\n" " .long " "663""1""f - .\n" " .word " "( 0*32+25)" "\n" " .byte " "662b-661b" "\n" " .byte " "664""1""f-""663""1""f" "\n" ".popsection\n" ".pushsection .discard,\"aw\",@progbits\n" " .byte 0xff + (" "664""1""f-""663""1""f" ") - (" "662b-661b" ")\n" ".popsection\n" ".pushsection .altinstr_replacement, \"ax\"\n" "663""1"":\n\t" "prefetchnta (%1)" "\n" "664""1" ":\n\t" ".popsection" : : "i" (0), "r" (x))


             ;
}






static inline __attribute__((no_instrument_function)) void prefetchw(const void *x)
{
 asm volatile ("661:\n\t" "prefetcht0 (%1)" "\n662:\n" ".pushsection .altinstructions,\"a\"\n" " .long 661b - .\n" " .long " "663""1""f - .\n" " .word " "( 1*32+31)" "\n" " .byte " "662b-661b" "\n" " .byte " "664""1""f-""663""1""f" "\n" ".popsection\n" ".pushsection .discard,\"aw\",@progbits\n" " .byte 0xff + (" "664""1""f-""663""1""f" ") - (" "662b-661b" ")\n" ".popsection\n" ".pushsection .altinstr_replacement, \"ax\"\n" "663""1"":\n\t" "prefetchw (%1)" "\n" "664""1" ":\n\t" ".popsection" : : "i" (0), "r" (x))


             ;
}

static inline __attribute__((no_instrument_function)) void spin_lock_prefetch(const void *x)
{
 prefetchw(x);
}
# 937 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h"
extern unsigned long KSTK_ESP(struct task_struct *task);




extern __attribute__((section(".data..percpu" ""))) __typeof__(unsigned long) old_rsp;



extern void start_thread(struct pt_regs *regs, unsigned long new_ip,
            unsigned long new_sp);
# 961 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h"
extern int get_tsc_mode(unsigned long adr);
extern int set_tsc_mode(unsigned int val);
# 972 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/processor.h"
static inline __attribute__((no_instrument_function)) int mpx_enable_management(struct task_struct *tsk)
{
 return -22;
}
static inline __attribute__((no_instrument_function)) int mpx_disable_management(struct task_struct *tsk)
{
 return -22;
}


extern u16 amd_get_nb_id(int cpu);

static inline __attribute__((no_instrument_function)) uint32_t hypervisor_cpuid_base(const char *sig, uint32_t leaves)
{
 uint32_t base, eax, signature[3];

 for (base = 0x40000000; base < 0x40010000; base += 0x100) {
  cpuid(base, &eax, &signature[0], &signature[1], &signature[2]);

  if (!memcmp(sig, signature, 12) &&
      (leaves == 0 || ((eax - base) >= leaves)))
   return base;
 }

 return 0;
}

extern unsigned long arch_align_stack(unsigned long sp);
extern void free_init_pages(char *what, unsigned long begin, unsigned long end);

void default_idle(void);

bool xen_set_default_idle(void);




void stop_this_cpu(void *dummy);
void df_debug(struct pt_regs *regs, long error_code);
# 51 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/thread_info.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/atomic.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic.h" 1







# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/cmpxchg.h" 1
# 13 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/cmpxchg.h"
extern void __xchg_wrong_size(void)
 __attribute__((error("Bad argument size for xchg")));
extern void __cmpxchg_wrong_size(void)
 __attribute__((error("Bad argument size for cmpxchg")));
extern void __xadd_wrong_size(void)
 __attribute__((error("Bad argument size for xadd")));
extern void __add_wrong_size(void)
 __attribute__((error("Bad argument size for add")));
# 145 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/cmpxchg.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/cmpxchg_64.h" 1



static inline __attribute__((no_instrument_function)) void set_64bit(volatile u64 *ptr, u64 val)
{
 *ptr = val;
}
# 146 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/cmpxchg.h" 2
# 9 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic.h" 2
# 25 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic.h"
static inline __attribute__((no_instrument_function)) int atomic_read(const atomic_t *v)
{
 return (*({ __attribute__((unused)) typeof((v)->counter) __var = ( typeof((v)->counter)) 0; (volatile typeof((v)->counter) *)&((v)->counter); }));
}
# 37 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic.h"
static inline __attribute__((no_instrument_function)) void atomic_set(atomic_t *v, int i)
{
 v->counter = i;
}
# 49 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic.h"
static inline __attribute__((no_instrument_function)) void atomic_add(int i, atomic_t *v)
{
 asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "addl %1,%0"
       : "+m" (v->counter)
       : "ir" (i));
}
# 63 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic.h"
static inline __attribute__((no_instrument_function)) void atomic_sub(int i, atomic_t *v)
{
 asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "subl %1,%0"
       : "+m" (v->counter)
       : "ir" (i));
}
# 79 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic.h"
static inline __attribute__((no_instrument_function)) int atomic_sub_and_test(int i, atomic_t *v)
{
 do { char c; asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "subl" " %2, " "%0" "; set" "e" " %1" : "+m" (v->counter), "=qm" (c) : "er" (i) : "memory"); return c != 0; } while (0);
}







static inline __attribute__((no_instrument_function)) void atomic_inc(atomic_t *v)
{
 asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "incl %0"
       : "+m" (v->counter));
}







static inline __attribute__((no_instrument_function)) void atomic_dec(atomic_t *v)
{
 asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "decl %0"
       : "+m" (v->counter));
}
# 116 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic.h"
static inline __attribute__((no_instrument_function)) int atomic_dec_and_test(atomic_t *v)
{
 do { char c; asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "decl" " " "%0" "; set" "e" " %1" : "+m" (v->counter), "=qm" (c) : : "memory"); return c != 0; } while (0);
}
# 129 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic.h"
static inline __attribute__((no_instrument_function)) int atomic_inc_and_test(atomic_t *v)
{
 do { char c; asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "incl" " " "%0" "; set" "e" " %1" : "+m" (v->counter), "=qm" (c) : : "memory"); return c != 0; } while (0);
}
# 143 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic.h"
static inline __attribute__((no_instrument_function)) int atomic_add_negative(int i, atomic_t *v)
{
 do { char c; asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "addl" " %2, " "%0" "; set" "s" " %1" : "+m" (v->counter), "=qm" (c) : "er" (i) : "memory"); return c != 0; } while (0);
}
# 155 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic.h"
static inline __attribute__((no_instrument_function)) int atomic_add_return(int i, atomic_t *v)
{
 return i + ({ __typeof__ (*(((&v->counter)))) __ret = (((i))); switch (sizeof(*(((&v->counter))))) { case 1: asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "xadd" "b %b0, %1\n" : "+q" (__ret), "+m" (*(((&v->counter)))) : : "memory", "cc"); break; case 2: asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "xadd" "w %w0, %1\n" : "+r" (__ret), "+m" (*(((&v->counter)))) : : "memory", "cc"); break; case 4: asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "xadd" "l %0, %1\n" : "+r" (__ret), "+m" (*(((&v->counter)))) : : "memory", "cc"); break; case 8: asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "xadd" "q %q0, %1\n" : "+r" (__ret), "+m" (*(((&v->counter)))) : : "memory", "cc"); break; default: __xadd_wrong_size(); } __ret; });
}
# 167 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic.h"
static inline __attribute__((no_instrument_function)) int atomic_sub_return(int i, atomic_t *v)
{
 return atomic_add_return(-i, v);
}




static inline __attribute__((no_instrument_function)) int atomic_cmpxchg(atomic_t *v, int old, int new)
{
 return ({ __typeof__(*((&v->counter))) __ret; __typeof__(*((&v->counter))) __old = ((old)); __typeof__(*((&v->counter))) __new = ((new)); switch ((sizeof(*(&v->counter)))) { case 1: { volatile u8 *__ptr = (volatile u8 *)((&v->counter)); asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "cmpxchgb %2,%1" : "=a" (__ret), "+m" (*__ptr) : "q" (__new), "0" (__old) : "memory"); break; } case 2: { volatile u16 *__ptr = (volatile u16 *)((&v->counter)); asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "cmpxchgw %2,%1" : "=a" (__ret), "+m" (*__ptr) : "r" (__new), "0" (__old) : "memory"); break; } case 4: { volatile u32 *__ptr = (volatile u32 *)((&v->counter)); asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "cmpxchgl %2,%1" : "=a" (__ret), "+m" (*__ptr) : "r" (__new), "0" (__old) : "memory"); break; } case 8: { volatile u64 *__ptr = (volatile u64 *)((&v->counter)); asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "cmpxchgq %2,%1" : "=a" (__ret), "+m" (*__ptr) : "r" (__new), "0" (__old) : "memory"); break; } default: __cmpxchg_wrong_size(); } __ret; });
}

static inline __attribute__((no_instrument_function)) int atomic_xchg(atomic_t *v, int new)
{
 return ({ __typeof__ (*((&v->counter))) __ret = ((new)); switch (sizeof(*((&v->counter)))) { case 1: asm volatile ("" "xchg" "b %b0, %1\n" : "+q" (__ret), "+m" (*((&v->counter))) : : "memory", "cc"); break; case 2: asm volatile ("" "xchg" "w %w0, %1\n" : "+r" (__ret), "+m" (*((&v->counter))) : : "memory", "cc"); break; case 4: asm volatile ("" "xchg" "l %0, %1\n" : "+r" (__ret), "+m" (*((&v->counter))) : : "memory", "cc"); break; case 8: asm volatile ("" "xchg" "q %q0, %1\n" : "+r" (__ret), "+m" (*((&v->counter))) : : "memory", "cc"); break; default: __xchg_wrong_size(); } __ret; });
}
# 194 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic.h"
static inline __attribute__((no_instrument_function)) int __atomic_add_unless(atomic_t *v, int a, int u)
{
 int c, old;
 c = atomic_read(v);
 for (;;) {
  if (__builtin_expect(!!(c == (u)), 0))
   break;
  old = atomic_cmpxchg((v), c, c + (a));
  if (__builtin_expect(!!(old == c), 1))
   break;
  c = old;
 }
 return c;
}
# 216 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic.h"
static inline __attribute__((no_instrument_function)) short int atomic_inc_short(short int *v)
{
 asm(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "addw $1, %0" : "+m" (*v));
 return *v;
}
# 235 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic64_64.h" 1
# 19 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic64_64.h"
static inline __attribute__((no_instrument_function)) long atomic64_read(const atomic64_t *v)
{
 return (*({ __attribute__((unused)) typeof((v)->counter) __var = ( typeof((v)->counter)) 0; (volatile typeof((v)->counter) *)&((v)->counter); }));
}
# 31 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic64_64.h"
static inline __attribute__((no_instrument_function)) void atomic64_set(atomic64_t *v, long i)
{
 v->counter = i;
}
# 43 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic64_64.h"
static inline __attribute__((no_instrument_function)) void atomic64_add(long i, atomic64_t *v)
{
 asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "addq %1,%0"
       : "=m" (v->counter)
       : "er" (i), "m" (v->counter));
}
# 57 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic64_64.h"
static inline __attribute__((no_instrument_function)) void atomic64_sub(long i, atomic64_t *v)
{
 asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "subq %1,%0"
       : "=m" (v->counter)
       : "er" (i), "m" (v->counter));
}
# 73 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic64_64.h"
static inline __attribute__((no_instrument_function)) int atomic64_sub_and_test(long i, atomic64_t *v)
{
 do { char c; asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "subq" " %2, " "%0" "; set" "e" " %1" : "+m" (v->counter), "=qm" (c) : "er" (i) : "memory"); return c != 0; } while (0);
}







static inline __attribute__((no_instrument_function)) void atomic64_inc(atomic64_t *v)
{
 asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "incq %0"
       : "=m" (v->counter)
       : "m" (v->counter));
}







static inline __attribute__((no_instrument_function)) void atomic64_dec(atomic64_t *v)
{
 asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "decq %0"
       : "=m" (v->counter)
       : "m" (v->counter));
}
# 112 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic64_64.h"
static inline __attribute__((no_instrument_function)) int atomic64_dec_and_test(atomic64_t *v)
{
 do { char c; asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "decq" " " "%0" "; set" "e" " %1" : "+m" (v->counter), "=qm" (c) : : "memory"); return c != 0; } while (0);
}
# 125 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic64_64.h"
static inline __attribute__((no_instrument_function)) int atomic64_inc_and_test(atomic64_t *v)
{
 do { char c; asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "incq" " " "%0" "; set" "e" " %1" : "+m" (v->counter), "=qm" (c) : : "memory"); return c != 0; } while (0);
}
# 139 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic64_64.h"
static inline __attribute__((no_instrument_function)) int atomic64_add_negative(long i, atomic64_t *v)
{
 do { char c; asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "addq" " %2, " "%0" "; set" "s" " %1" : "+m" (v->counter), "=qm" (c) : "er" (i) : "memory"); return c != 0; } while (0);
}
# 151 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic64_64.h"
static inline __attribute__((no_instrument_function)) long atomic64_add_return(long i, atomic64_t *v)
{
 return i + ({ __typeof__ (*(((&v->counter)))) __ret = (((i))); switch (sizeof(*(((&v->counter))))) { case 1: asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "xadd" "b %b0, %1\n" : "+q" (__ret), "+m" (*(((&v->counter)))) : : "memory", "cc"); break; case 2: asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "xadd" "w %w0, %1\n" : "+r" (__ret), "+m" (*(((&v->counter)))) : : "memory", "cc"); break; case 4: asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "xadd" "l %0, %1\n" : "+r" (__ret), "+m" (*(((&v->counter)))) : : "memory", "cc"); break; case 8: asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "xadd" "q %q0, %1\n" : "+r" (__ret), "+m" (*(((&v->counter)))) : : "memory", "cc"); break; default: __xadd_wrong_size(); } __ret; });
}

static inline __attribute__((no_instrument_function)) long atomic64_sub_return(long i, atomic64_t *v)
{
 return atomic64_add_return(-i, v);
}




static inline __attribute__((no_instrument_function)) long atomic64_cmpxchg(atomic64_t *v, long old, long new)
{
 return ({ __typeof__(*((&v->counter))) __ret; __typeof__(*((&v->counter))) __old = ((old)); __typeof__(*((&v->counter))) __new = ((new)); switch ((sizeof(*(&v->counter)))) { case 1: { volatile u8 *__ptr = (volatile u8 *)((&v->counter)); asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "cmpxchgb %2,%1" : "=a" (__ret), "+m" (*__ptr) : "q" (__new), "0" (__old) : "memory"); break; } case 2: { volatile u16 *__ptr = (volatile u16 *)((&v->counter)); asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "cmpxchgw %2,%1" : "=a" (__ret), "+m" (*__ptr) : "r" (__new), "0" (__old) : "memory"); break; } case 4: { volatile u32 *__ptr = (volatile u32 *)((&v->counter)); asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "cmpxchgl %2,%1" : "=a" (__ret), "+m" (*__ptr) : "r" (__new), "0" (__old) : "memory"); break; } case 8: { volatile u64 *__ptr = (volatile u64 *)((&v->counter)); asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "cmpxchgq %2,%1" : "=a" (__ret), "+m" (*__ptr) : "r" (__new), "0" (__old) : "memory"); break; } default: __cmpxchg_wrong_size(); } __ret; });
}

static inline __attribute__((no_instrument_function)) long atomic64_xchg(atomic64_t *v, long new)
{
 return ({ __typeof__ (*((&v->counter))) __ret = ((new)); switch (sizeof(*((&v->counter)))) { case 1: asm volatile ("" "xchg" "b %b0, %1\n" : "+q" (__ret), "+m" (*((&v->counter))) : : "memory", "cc"); break; case 2: asm volatile ("" "xchg" "w %w0, %1\n" : "+r" (__ret), "+m" (*((&v->counter))) : : "memory", "cc"); break; case 4: asm volatile ("" "xchg" "l %0, %1\n" : "+r" (__ret), "+m" (*((&v->counter))) : : "memory", "cc"); break; case 8: asm volatile ("" "xchg" "q %q0, %1\n" : "+r" (__ret), "+m" (*((&v->counter))) : : "memory", "cc"); break; default: __xchg_wrong_size(); } __ret; });
}
# 183 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic64_64.h"
static inline __attribute__((no_instrument_function)) int atomic64_add_unless(atomic64_t *v, long a, long u)
{
 long c, old;
 c = atomic64_read(v);
 for (;;) {
  if (__builtin_expect(!!(c == (u)), 0))
   break;
  old = atomic64_cmpxchg((v), c, c + (a));
  if (__builtin_expect(!!(old == c), 1))
   break;
  c = old;
 }
 return c != (u);
}
# 207 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic64_64.h"
static inline __attribute__((no_instrument_function)) long atomic64_dec_if_positive(atomic64_t *v)
{
 long c, old, dec;
 c = atomic64_read(v);
 for (;;) {
  dec = c - 1;
  if (__builtin_expect(!!(dec < 0), 0))
   break;
  old = atomic64_cmpxchg((v), c, dec);
  if (__builtin_expect(!!(old == c), 1))
   break;
  c = old;
 }
 return dec;
}
# 236 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/atomic.h" 2
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/atomic.h" 2
# 15 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/atomic.h"
static inline __attribute__((no_instrument_function)) int atomic_add_unless(atomic_t *v, int a, int u)
{
 return __atomic_add_unless(v, a, u) != u;
}
# 44 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/atomic.h"
static inline __attribute__((no_instrument_function)) int atomic_inc_not_zero_hint(atomic_t *v, int hint)
{
 int val, c = hint;


 if (!hint)
  return atomic_add_unless((v), 1, 0);

 do {
  val = atomic_cmpxchg(v, c, c + 1);
  if (val == c)
   return 1;
  c = val;
 } while (c);

 return 0;
}



static inline __attribute__((no_instrument_function)) int atomic_inc_unless_negative(atomic_t *p)
{
 int v, v1;
 for (v = 0; v >= 0; v = v1) {
  v1 = atomic_cmpxchg(p, v, v + 1);
  if (__builtin_expect(!!(v1 == v), 1))
   return 1;
 }
 return 0;
}



static inline __attribute__((no_instrument_function)) int atomic_dec_unless_positive(atomic_t *p)
{
 int v, v1;
 for (v = 0; v <= 0; v = v1) {
  v1 = atomic_cmpxchg(p, v, v - 1);
  if (__builtin_expect(!!(v1 == v), 1))
   return 1;
 }
 return 0;
}
# 97 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/atomic.h"
static inline __attribute__((no_instrument_function)) int atomic_dec_if_positive(atomic_t *v)
{
 int c, old, dec;
 c = atomic_read(v);
 for (;;) {
  dec = c - 1;
  if (__builtin_expect(!!(dec < 0), 0))
   break;
  old = atomic_cmpxchg((v), c, dec);
  if (__builtin_expect(!!(old == c), 1))
   break;
  c = old;
 }
 return dec;
}



static inline __attribute__((no_instrument_function)) void atomic_or(int i, atomic_t *v)
{
 int old;
 int new;

 do {
  old = atomic_read(v);
  new = old | i;
 } while (atomic_cmpxchg(v, old, new) != old);
}


# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/atomic-long.h" 1
# 23 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/atomic-long.h"
typedef atomic64_t atomic_long_t;



static inline __attribute__((no_instrument_function)) long atomic_long_read(atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 return (long)atomic64_read(v);
}

static inline __attribute__((no_instrument_function)) void atomic_long_set(atomic_long_t *l, long i)
{
 atomic64_t *v = (atomic64_t *)l;

 atomic64_set(v, i);
}

static inline __attribute__((no_instrument_function)) void atomic_long_inc(atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 atomic64_inc(v);
}

static inline __attribute__((no_instrument_function)) void atomic_long_dec(atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 atomic64_dec(v);
}

static inline __attribute__((no_instrument_function)) void atomic_long_add(long i, atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 atomic64_add(i, v);
}

static inline __attribute__((no_instrument_function)) void atomic_long_sub(long i, atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 atomic64_sub(i, v);
}

static inline __attribute__((no_instrument_function)) int atomic_long_sub_and_test(long i, atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 return atomic64_sub_and_test(i, v);
}

static inline __attribute__((no_instrument_function)) int atomic_long_dec_and_test(atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 return atomic64_dec_and_test(v);
}

static inline __attribute__((no_instrument_function)) int atomic_long_inc_and_test(atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 return atomic64_inc_and_test(v);
}

static inline __attribute__((no_instrument_function)) int atomic_long_add_negative(long i, atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 return atomic64_add_negative(i, v);
}

static inline __attribute__((no_instrument_function)) long atomic_long_add_return(long i, atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 return (long)atomic64_add_return(i, v);
}

static inline __attribute__((no_instrument_function)) long atomic_long_sub_return(long i, atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 return (long)atomic64_sub_return(i, v);
}

static inline __attribute__((no_instrument_function)) long atomic_long_inc_return(atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 return (long)(atomic64_add_return(1, (v)));
}

static inline __attribute__((no_instrument_function)) long atomic_long_dec_return(atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 return (long)(atomic64_sub_return(1, (v)));
}

static inline __attribute__((no_instrument_function)) long atomic_long_add_unless(atomic_long_t *l, long a, long u)
{
 atomic64_t *v = (atomic64_t *)l;

 return (long)atomic64_add_unless(v, a, u);
}
# 128 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/atomic.h" 2
# 52 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/thread_info.h" 2

struct thread_info {
 struct task_struct *task;
 struct exec_domain *exec_domain;
 __u32 flags;
 __u32 status;
 __u32 cpu;
 int saved_preempt_count;
 mm_segment_t addr_limit;
 struct restart_block restart_block;
 void *sysenter_return;
 unsigned int sig_on_uaccess_error:1;
 unsigned int uaccess_err:1;
};
# 190 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/thread_info.h"
extern __attribute__((section(".data..percpu" ""))) __typeof__(unsigned long) kernel_stack;

static inline __attribute__((no_instrument_function)) struct thread_info *current_thread_info(void)
{
 struct thread_info *ti;
 ti = (void *)(({ typeof(kernel_stack) pfo_ret__; switch (sizeof(kernel_stack)) { case 1: asm("mov" "b ""%%""gs"":" "%" "P1"",%0" : "=q" (pfo_ret__) : "p" (&(kernel_stack))); break; case 2: asm("mov" "w ""%%""gs"":" "%" "P1"",%0" : "=r" (pfo_ret__) : "p" (&(kernel_stack))); break; case 4: asm("mov" "l ""%%""gs"":" "%" "P1"",%0" : "=r" (pfo_ret__) : "p" (&(kernel_stack))); break; case 8: asm("mov" "q ""%%""gs"":" "%" "P1"",%0" : "=r" (pfo_ret__) : "p" (&(kernel_stack))); break; default: __bad_percpu_size(); } pfo_ret__; }) +
        (5*(64/8)) - (((1UL) << 12) << 2));
 return ti;
}
# 227 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/thread_info.h"
static inline __attribute__((no_instrument_function)) void set_restore_sigmask(void)
{
 struct thread_info *ti = current_thread_info();
 ti->status |= 0x0008;
 ({ int __ret_warn_on = !!(!(__builtin_constant_p((2)) ? constant_test_bit((2), ((unsigned long *)&ti->flags)) : variable_test_bit((2), ((unsigned long *)&ti->flags)))); if (__builtin_expect(!!(__ret_warn_on), 0)) warn_slowpath_null("/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/thread_info.h", 231); __builtin_expect(!!(__ret_warn_on), 0); });
}
static inline __attribute__((no_instrument_function)) void clear_restore_sigmask(void)
{
 current_thread_info()->status &= ~0x0008;
}
static inline __attribute__((no_instrument_function)) bool test_restore_sigmask(void)
{
 return current_thread_info()->status & 0x0008;
}
static inline __attribute__((no_instrument_function)) bool test_and_clear_restore_sigmask(void)
{
 struct thread_info *ti = current_thread_info();
 if (!(ti->status & 0x0008))
  return false;
 ti->status &= ~0x0008;
 return true;
}

static inline __attribute__((no_instrument_function)) bool is_ia32_task(void)
{




 if (current_thread_info()->status & 0x0002)
  return true;

 return false;
}



extern void arch_task_cache_init(void);
extern int arch_dup_task_struct(struct task_struct *dst, struct task_struct *src);
extern void arch_release_task_struct(struct task_struct *tsk);
# 55 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/thread_info.h" 2
# 69 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/thread_info.h"
static inline __attribute__((no_instrument_function)) void set_ti_thread_flag(struct thread_info *ti, int flag)
{
 set_bit(flag, (unsigned long *)&ti->flags);
}

static inline __attribute__((no_instrument_function)) void clear_ti_thread_flag(struct thread_info *ti, int flag)
{
 clear_bit(flag, (unsigned long *)&ti->flags);
}

static inline __attribute__((no_instrument_function)) int test_and_set_ti_thread_flag(struct thread_info *ti, int flag)
{
 return test_and_set_bit(flag, (unsigned long *)&ti->flags);
}

static inline __attribute__((no_instrument_function)) int test_and_clear_ti_thread_flag(struct thread_info *ti, int flag)
{
 return test_and_clear_bit(flag, (unsigned long *)&ti->flags);
}

static inline __attribute__((no_instrument_function)) int test_ti_thread_flag(struct thread_info *ti, int flag)
{
 return (__builtin_constant_p((flag)) ? constant_test_bit((flag), ((unsigned long *)&ti->flags)) : variable_test_bit((flag), ((unsigned long *)&ti->flags)));
}
# 7 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/preempt.h" 2

extern __attribute__((section(".data..percpu" ""))) __typeof__(int) __preempt_count;
# 20 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/preempt.h"
static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) int preempt_count(void)
{
 return ({ typeof(__preempt_count) pfo_ret__; switch (sizeof(__preempt_count)) { case 1: asm("mov" "b ""%%""gs"":" "%" "1"",%0" : "=q" (pfo_ret__) : "m" (__preempt_count)); break; case 2: asm("mov" "w ""%%""gs"":" "%" "1"",%0" : "=r" (pfo_ret__) : "m" (__preempt_count)); break; case 4: asm("mov" "l ""%%""gs"":" "%" "1"",%0" : "=r" (pfo_ret__) : "m" (__preempt_count)); break; case 8: asm("mov" "q ""%%""gs"":" "%" "1"",%0" : "=r" (pfo_ret__) : "m" (__preempt_count)); break; default: __bad_percpu_size(); } pfo_ret__; }) & ~0x80000000;
}

static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) void preempt_count_set(int pc)
{
 do { typedef typeof((__preempt_count)) pto_T__; if (0) { pto_T__ pto_tmp__; pto_tmp__ = (pc); (void)pto_tmp__; } switch (sizeof((__preempt_count))) { case 1: asm("mov" "b %1,""%%""gs"":" "%" "0" : "+m" ((__preempt_count)) : "qi" ((pto_T__)(pc))); break; case 2: asm("mov" "w %1,""%%""gs"":" "%" "0" : "+m" ((__preempt_count)) : "ri" ((pto_T__)(pc))); break; case 4: asm("mov" "l %1,""%%""gs"":" "%" "0" : "+m" ((__preempt_count)) : "ri" ((pto_T__)(pc))); break; case 8: asm("mov" "q %1,""%%""gs"":" "%" "0" : "+m" ((__preempt_count)) : "re" ((pto_T__)(pc))); break; default: __bad_percpu_size(); } } while (0);
}
# 51 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/preempt.h"
static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) void set_preempt_need_resched(void)
{
 do { typedef typeof((__preempt_count)) pto_T__; if (0) { pto_T__ pto_tmp__; pto_tmp__ = (~0x80000000); (void)pto_tmp__; } switch (sizeof((__preempt_count))) { case 1: asm("and" "b %1,""%%""gs"":" "%" "0" : "+m" ((__preempt_count)) : "qi" ((pto_T__)(~0x80000000))); break; case 2: asm("and" "w %1,""%%""gs"":" "%" "0" : "+m" ((__preempt_count)) : "ri" ((pto_T__)(~0x80000000))); break; case 4: asm("and" "l %1,""%%""gs"":" "%" "0" : "+m" ((__preempt_count)) : "ri" ((pto_T__)(~0x80000000))); break; case 8: asm("and" "q %1,""%%""gs"":" "%" "0" : "+m" ((__preempt_count)) : "re" ((pto_T__)(~0x80000000))); break; default: __bad_percpu_size(); } } while (0);
}

static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) void clear_preempt_need_resched(void)
{
 do { typedef typeof((__preempt_count)) pto_T__; if (0) { pto_T__ pto_tmp__; pto_tmp__ = (0x80000000); (void)pto_tmp__; } switch (sizeof((__preempt_count))) { case 1: asm("or" "b %1,""%%""gs"":" "%" "0" : "+m" ((__preempt_count)) : "qi" ((pto_T__)(0x80000000))); break; case 2: asm("or" "w %1,""%%""gs"":" "%" "0" : "+m" ((__preempt_count)) : "ri" ((pto_T__)(0x80000000))); break; case 4: asm("or" "l %1,""%%""gs"":" "%" "0" : "+m" ((__preempt_count)) : "ri" ((pto_T__)(0x80000000))); break; case 8: asm("or" "q %1,""%%""gs"":" "%" "0" : "+m" ((__preempt_count)) : "re" ((pto_T__)(0x80000000))); break; default: __bad_percpu_size(); } } while (0);
}

static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) bool test_preempt_need_resched(void)
{
 return !(({ typeof(__preempt_count) pfo_ret__; switch (sizeof(__preempt_count)) { case 1: asm("mov" "b ""%%""gs"":" "%" "1"",%0" : "=q" (pfo_ret__) : "m" (__preempt_count)); break; case 2: asm("mov" "w ""%%""gs"":" "%" "1"",%0" : "=r" (pfo_ret__) : "m" (__preempt_count)); break; case 4: asm("mov" "l ""%%""gs"":" "%" "1"",%0" : "=r" (pfo_ret__) : "m" (__preempt_count)); break; case 8: asm("mov" "q ""%%""gs"":" "%" "1"",%0" : "=r" (pfo_ret__) : "m" (__preempt_count)); break; default: __bad_percpu_size(); } pfo_ret__; }) & 0x80000000);
}





static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) void __preempt_count_add(int val)
{
 do { typedef typeof((__preempt_count)) pao_T__; const int pao_ID__ = (__builtin_constant_p(val) && ((val) == 1 || (val) == -1)) ? (int)(val) : 0; if (0) { pao_T__ pao_tmp__; pao_tmp__ = (val); (void)pao_tmp__; } switch (sizeof((__preempt_count))) { case 1: if (pao_ID__ == 1) asm("incb ""%%""gs"":" "%" "0" : "+m" ((__preempt_count))); else if (pao_ID__ == -1) asm("decb ""%%""gs"":" "%" "0" : "+m" ((__preempt_count))); else asm("addb %1, ""%%""gs"":" "%" "0" : "+m" ((__preempt_count)) : "qi" ((pao_T__)(val))); break; case 2: if (pao_ID__ == 1) asm("incw ""%%""gs"":" "%" "0" : "+m" ((__preempt_count))); else if (pao_ID__ == -1) asm("decw ""%%""gs"":" "%" "0" : "+m" ((__preempt_count))); else asm("addw %1, ""%%""gs"":" "%" "0" : "+m" ((__preempt_count)) : "ri" ((pao_T__)(val))); break; case 4: if (pao_ID__ == 1) asm("incl ""%%""gs"":" "%" "0" : "+m" ((__preempt_count))); else if (pao_ID__ == -1) asm("decl ""%%""gs"":" "%" "0" : "+m" ((__preempt_count))); else asm("addl %1, ""%%""gs"":" "%" "0" : "+m" ((__preempt_count)) : "ri" ((pao_T__)(val))); break; case 8: if (pao_ID__ == 1) asm("incq ""%%""gs"":" "%" "0" : "+m" ((__preempt_count))); else if (pao_ID__ == -1) asm("decq ""%%""gs"":" "%" "0" : "+m" ((__preempt_count))); else asm("addq %1, ""%%""gs"":" "%" "0" : "+m" ((__preempt_count)) : "re" ((pao_T__)(val))); break; default: __bad_percpu_size(); } } while (0);
}

static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) void __preempt_count_sub(int val)
{
 do { typedef typeof((__preempt_count)) pao_T__; const int pao_ID__ = (__builtin_constant_p(-val) && ((-val) == 1 || (-val) == -1)) ? (int)(-val) : 0; if (0) { pao_T__ pao_tmp__; pao_tmp__ = (-val); (void)pao_tmp__; } switch (sizeof((__preempt_count))) { case 1: if (pao_ID__ == 1) asm("incb ""%%""gs"":" "%" "0" : "+m" ((__preempt_count))); else if (pao_ID__ == -1) asm("decb ""%%""gs"":" "%" "0" : "+m" ((__preempt_count))); else asm("addb %1, ""%%""gs"":" "%" "0" : "+m" ((__preempt_count)) : "qi" ((pao_T__)(-val))); break; case 2: if (pao_ID__ == 1) asm("incw ""%%""gs"":" "%" "0" : "+m" ((__preempt_count))); else if (pao_ID__ == -1) asm("decw ""%%""gs"":" "%" "0" : "+m" ((__preempt_count))); else asm("addw %1, ""%%""gs"":" "%" "0" : "+m" ((__preempt_count)) : "ri" ((pao_T__)(-val))); break; case 4: if (pao_ID__ == 1) asm("incl ""%%""gs"":" "%" "0" : "+m" ((__preempt_count))); else if (pao_ID__ == -1) asm("decl ""%%""gs"":" "%" "0" : "+m" ((__preempt_count))); else asm("addl %1, ""%%""gs"":" "%" "0" : "+m" ((__preempt_count)) : "ri" ((pao_T__)(-val))); break; case 8: if (pao_ID__ == 1) asm("incq ""%%""gs"":" "%" "0" : "+m" ((__preempt_count))); else if (pao_ID__ == -1) asm("decq ""%%""gs"":" "%" "0" : "+m" ((__preempt_count))); else asm("addq %1, ""%%""gs"":" "%" "0" : "+m" ((__preempt_count)) : "re" ((pao_T__)(-val))); break; default: __bad_percpu_size(); } } while (0);
}






static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) bool __preempt_count_dec_and_test(void)
{
 do { char c; asm volatile ("decl" " " "%%""gs"":" "%" "0" "; set" "e" " %1" : "+m" (__preempt_count), "=qm" (c) : : "memory"); return c != 0; } while (0);
}




static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) bool should_resched(void)
{
 return __builtin_expect(!!(!({ typeof(__preempt_count) pfo_ret__; switch (sizeof(__preempt_count)) { case 1: asm("mov" "b ""%%""gs"":" "%" "1"",%0" : "=q" (pfo_ret__) : "m" (__preempt_count)); break; case 2: asm("mov" "w ""%%""gs"":" "%" "1"",%0" : "=r" (pfo_ret__) : "m" (__preempt_count)); break; case 4: asm("mov" "l ""%%""gs"":" "%" "1"",%0" : "=r" (pfo_ret__) : "m" (__preempt_count)); break; case 8: asm("mov" "q ""%%""gs"":" "%" "1"",%0" : "=r" (pfo_ret__) : "m" (__preempt_count)); break; default: __bad_percpu_size(); } pfo_ret__; })), 0);
}
# 19 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/preempt.h" 2
# 149 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/preempt.h"
struct preempt_notifier;
# 165 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/preempt.h"
struct preempt_ops {
 void (*sched_in)(struct preempt_notifier *notifier, int cpu);
 void (*sched_out)(struct preempt_notifier *notifier,
     struct task_struct *next);
};
# 178 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/preempt.h"
struct preempt_notifier {
 struct hlist_node link;
 struct preempt_ops *ops;
};

void preempt_notifier_register(struct preempt_notifier *notifier);
void preempt_notifier_unregister(struct preempt_notifier *notifier);

static inline __attribute__((no_instrument_function)) void preempt_notifier_init(struct preempt_notifier *notifier,
         struct preempt_ops *ops)
{
 INIT_HLIST_NODE(&notifier->link);
 notifier->ops = ops;
}
# 51 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock.h" 2






# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/bottom_half.h" 1




# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/preempt_mask.h" 1
# 6 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/bottom_half.h" 2




static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) void __local_bh_disable_ip(unsigned long ip, unsigned int cnt)
{
 __preempt_count_add(cnt);
 __asm__ __volatile__("": : :"memory");
}


static inline __attribute__((no_instrument_function)) void local_bh_disable(void)
{
 __local_bh_disable_ip(({ __label__ __here; __here: (unsigned long)&&__here; }), (2 * (1UL << (0 + 8))));
}

extern void _local_bh_enable(void);
extern void __local_bh_enable_ip(unsigned long ip, unsigned int cnt);

static inline __attribute__((no_instrument_function)) void local_bh_enable_ip(unsigned long ip)
{
 __local_bh_enable_ip(ip, (2 * (1UL << (0 + 8))));
}

static inline __attribute__((no_instrument_function)) void local_bh_enable(void)
{
 __local_bh_enable_ip(({ __label__ __here; __here: (unsigned long)&&__here; }), (2 * (1UL << (0 + 8))));
}
# 58 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock.h" 2
# 81 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock_types.h" 1
# 18 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock_types.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/lockdep.h" 1
# 12 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/lockdep.h"
struct task_struct;
struct lockdep_map;


extern int prove_locking;
extern int lock_stat;
# 373 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/lockdep.h"
static inline __attribute__((no_instrument_function)) void lockdep_off(void)
{
}

static inline __attribute__((no_instrument_function)) void lockdep_on(void)
{
}
# 414 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/lockdep.h"
struct lock_class_key { };
# 469 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/lockdep.h"
static inline __attribute__((no_instrument_function)) void print_irqtrace_events(struct task_struct *curr)
{
}
# 19 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock_types.h" 2

typedef struct raw_spinlock {
 arch_spinlock_t raw_lock;
# 32 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock_types.h"
} raw_spinlock_t;
# 64 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock_types.h"
typedef struct spinlock {
 union {
  struct raw_spinlock rlock;
# 75 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock_types.h"
 };
} spinlock_t;
# 86 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock_types.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/rwlock_types.h" 1
# 11 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/rwlock_types.h"
typedef struct {
 arch_rwlock_t raw_lock;
# 23 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/rwlock_types.h"
} rwlock_t;
# 87 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock_types.h" 2
# 82 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock.h" 2





# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/spinlock.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/jump_label.h" 1
# 52 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/jump_label.h"
extern bool static_key_initialized;
# 72 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/jump_label.h"
struct static_key {
 atomic_t enabled;
};


enum jump_label_type {
 JUMP_LABEL_DISABLE = 0,
 JUMP_LABEL_ENABLE,
};

struct module;



static inline __attribute__((no_instrument_function)) int static_key_count(struct static_key *key)
{
 return atomic_read(&key->enabled);
}
# 146 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/jump_label.h"
static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) void jump_label_init(void)
{
 static_key_initialized = true;
}

static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) bool static_key_false(struct static_key *key)
{
 if (__builtin_expect(!!(static_key_count(key) > 0), 0))
  return true;
 return false;
}

static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) bool static_key_true(struct static_key *key)
{
 if (__builtin_expect(!!(static_key_count(key) > 0), 1))
  return true;
 return false;
}

static inline __attribute__((no_instrument_function)) void static_key_slow_inc(struct static_key *key)
{
 ({ int __ret_warn_on = !!(!static_key_initialized); if (__builtin_expect(!!(__ret_warn_on), 0)) warn_slowpath_fmt("/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/jump_label.h", 167, "%s used before call to jump_label_init", __func__); __builtin_expect(!!(__ret_warn_on), 0); });
 atomic_inc(&key->enabled);
}

static inline __attribute__((no_instrument_function)) void static_key_slow_dec(struct static_key *key)
{
 ({ int __ret_warn_on = !!(!static_key_initialized); if (__builtin_expect(!!(__ret_warn_on), 0)) warn_slowpath_fmt("/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/jump_label.h", 173, "%s used before call to jump_label_init", __func__); __builtin_expect(!!(__ret_warn_on), 0); });
 atomic_dec(&key->enabled);
}

static inline __attribute__((no_instrument_function)) int jump_label_text_reserved(void *start, void *end)
{
 return 0;
}

static inline __attribute__((no_instrument_function)) void jump_label_lock(void) {}
static inline __attribute__((no_instrument_function)) void jump_label_unlock(void) {}

static inline __attribute__((no_instrument_function)) int jump_label_apply_nops(struct module *mod)
{
 return 0;
}
# 200 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/jump_label.h"
static inline __attribute__((no_instrument_function)) bool static_key_enabled(struct static_key *key)
{
 return static_key_count(key) > 0;
}
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/spinlock.h" 2
# 42 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/spinlock.h"
extern struct static_key paravirt_ticketlocks_enabled;
static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) bool static_key_false(struct static_key *key);



static inline __attribute__((no_instrument_function)) void __ticket_enter_slowpath(arch_spinlock_t *lock)
{
 set_bit(0, (volatile unsigned long *)&lock->tickets.head);
}
# 63 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/spinlock.h"
static inline __attribute__((no_instrument_function)) int __tickets_equal(__ticket_t one, __ticket_t two)
{
 return !((one ^ two) & ~((__ticket_t)1));
}

static inline __attribute__((no_instrument_function)) void __ticket_check_and_clear_slowpath(arch_spinlock_t *lock,
       __ticket_t head)
{
 if (head & ((__ticket_t)1)) {
  arch_spinlock_t old, new;

  old.tickets.head = head;
  new.tickets.head = head & ~((__ticket_t)1);
  old.tickets.tail = new.tickets.head + ((__ticket_t)2);
  new.tickets.tail = old.tickets.tail;


  ({ __typeof__(*((&lock->head_tail))) __ret; __typeof__(*((&lock->head_tail))) __old = ((old.head_tail)); __typeof__(*((&lock->head_tail))) __new = ((new.head_tail)); switch ((sizeof(*(&lock->head_tail)))) { case 1: { volatile u8 *__ptr = (volatile u8 *)((&lock->head_tail)); asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "cmpxchgb %2,%1" : "=a" (__ret), "+m" (*__ptr) : "q" (__new), "0" (__old) : "memory"); break; } case 2: { volatile u16 *__ptr = (volatile u16 *)((&lock->head_tail)); asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "cmpxchgw %2,%1" : "=a" (__ret), "+m" (*__ptr) : "r" (__new), "0" (__old) : "memory"); break; } case 4: { volatile u32 *__ptr = (volatile u32 *)((&lock->head_tail)); asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "cmpxchgl %2,%1" : "=a" (__ret), "+m" (*__ptr) : "r" (__new), "0" (__old) : "memory"); break; } case 8: { volatile u64 *__ptr = (volatile u64 *)((&lock->head_tail)); asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "cmpxchgq %2,%1" : "=a" (__ret), "+m" (*__ptr) : "r" (__new), "0" (__old) : "memory"); break; } default: __cmpxchg_wrong_size(); } __ret; });
 }
}

static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) int arch_spin_value_unlocked(arch_spinlock_t lock)
{
 return __tickets_equal(lock.tickets.head, lock.tickets.tail);
}
# 102 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/spinlock.h"
static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) void arch_spin_lock(arch_spinlock_t *lock)
{
 register struct __raw_tickets inc = { .tail = ((__ticket_t)2) };

 inc = ({ __typeof__ (*(((&lock->tickets)))) __ret = (((inc))); switch (sizeof(*(((&lock->tickets))))) { case 1: asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "xadd" "b %b0, %1\n" : "+q" (__ret), "+m" (*(((&lock->tickets)))) : : "memory", "cc"); break; case 2: asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "xadd" "w %w0, %1\n" : "+r" (__ret), "+m" (*(((&lock->tickets)))) : : "memory", "cc"); break; case 4: asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "xadd" "l %0, %1\n" : "+r" (__ret), "+m" (*(((&lock->tickets)))) : : "memory", "cc"); break; case 8: asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "xadd" "q %q0, %1\n" : "+r" (__ret), "+m" (*(((&lock->tickets)))) : : "memory", "cc"); break; default: __xadd_wrong_size(); } __ret; });
 if (__builtin_expect(!!(inc.head == inc.tail), 1))
  goto out;

 for (;;) {
  unsigned count = (1 << 15);

  do {
   inc.head = ({ union { typeof(lock->tickets.head) __val; char __c[1]; } __u; __read_once_size(&(lock->tickets.head), __u.__c, sizeof(lock->tickets.head)); __u.__val; });
   if (__tickets_equal(inc.head, inc.tail))
    goto clear_slowpath;
   cpu_relax();
  } while (--count);
  __ticket_lock_spinning(lock, inc.tail);
 }
clear_slowpath:
 __ticket_check_and_clear_slowpath(lock, inc.head);
out:
 __asm__ __volatile__("": : :"memory");
}

static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) int arch_spin_trylock(arch_spinlock_t *lock)
{
 arch_spinlock_t old, new;

 old.tickets = ({ union { typeof(lock->tickets) __val; char __c[1]; } __u; __read_once_size(&(lock->tickets), __u.__c, sizeof(lock->tickets)); __u.__val; });
 if (!__tickets_equal(old.tickets.head, old.tickets.tail))
  return 0;

 new.head_tail = old.head_tail + (((__ticket_t)2) << (sizeof(__ticket_t) * 8));
 new.head_tail &= ~((__ticket_t)1);


 return ({ __typeof__(*((&lock->head_tail))) __ret; __typeof__(*((&lock->head_tail))) __old = ((old.head_tail)); __typeof__(*((&lock->head_tail))) __new = ((new.head_tail)); switch ((sizeof(*(&lock->head_tail)))) { case 1: { volatile u8 *__ptr = (volatile u8 *)((&lock->head_tail)); asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "cmpxchgb %2,%1" : "=a" (__ret), "+m" (*__ptr) : "q" (__new), "0" (__old) : "memory"); break; } case 2: { volatile u16 *__ptr = (volatile u16 *)((&lock->head_tail)); asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "cmpxchgw %2,%1" : "=a" (__ret), "+m" (*__ptr) : "r" (__new), "0" (__old) : "memory"); break; } case 4: { volatile u32 *__ptr = (volatile u32 *)((&lock->head_tail)); asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "cmpxchgl %2,%1" : "=a" (__ret), "+m" (*__ptr) : "r" (__new), "0" (__old) : "memory"); break; } case 8: { volatile u64 *__ptr = (volatile u64 *)((&lock->head_tail)); asm volatile(".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "cmpxchgq %2,%1" : "=a" (__ret), "+m" (*__ptr) : "r" (__new), "0" (__old) : "memory"); break; } default: __cmpxchg_wrong_size(); } __ret; }) == old.head_tail;
}

static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) void arch_spin_unlock(arch_spinlock_t *lock)
{
 if (((__ticket_t)1) &&
  static_key_false(&paravirt_ticketlocks_enabled)) {
  __ticket_t head;

  do { bool __cond = !(!(((__ticket_t)256) != 256)); extern void __compiletime_assert_148(void) __attribute__((error("BUILD_BUG_ON failed: " "((__ticket_t)NR_CPUS) != NR_CPUS"))); if (__cond) __compiletime_assert_148(); do { } while (0); } while (0);

  head = ({ __typeof__ (*(((&lock->tickets.head)))) __ret = (((((__ticket_t)2)))); switch (sizeof(*(((&lock->tickets.head))))) { case 1: asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "xadd" "b %b0, %1\n" : "+q" (__ret), "+m" (*(((&lock->tickets.head)))) : : "memory", "cc"); break; case 2: asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "xadd" "w %w0, %1\n" : "+r" (__ret), "+m" (*(((&lock->tickets.head)))) : : "memory", "cc"); break; case 4: asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "xadd" "l %0, %1\n" : "+r" (__ret), "+m" (*(((&lock->tickets.head)))) : : "memory", "cc"); break; case 8: asm volatile (".pushsection .smp_locks,\"a\"\n" ".balign 4\n" ".long 671f - .\n" ".popsection\n" "671:" "\n\tlock; " "xadd" "q %q0, %1\n" : "+r" (__ret), "+m" (*(((&lock->tickets.head)))) : : "memory", "cc"); break; default: __xadd_wrong_size(); } __ret; });

  if (__builtin_expect(!!(head & ((__ticket_t)1)), 0)) {
   head &= ~((__ticket_t)1);
   __ticket_unlock_kick(lock, (head + ((__ticket_t)2)));
  }
 } else
  ({ __typeof__ (*(&lock->tickets.head)) __ret = (((__ticket_t)2)); switch (sizeof(*(&lock->tickets.head))) { case 1: asm volatile ( "addb %b1, %0\n" : "+m" (*(&lock->tickets.head)) : "qi" (((__ticket_t)2)) : "memory", "cc"); break; case 2: asm volatile ( "addw %w1, %0\n" : "+m" (*(&lock->tickets.head)) : "ri" (((__ticket_t)2)) : "memory", "cc"); break; case 4: asm volatile ( "addl %1, %0\n" : "+m" (*(&lock->tickets.head)) : "ri" (((__ticket_t)2)) : "memory", "cc"); break; case 8: asm volatile ( "addq %1, %0\n" : "+m" (*(&lock->tickets.head)) : "ri" (((__ticket_t)2)) : "memory", "cc"); break; default: __add_wrong_size(); } __ret; });
}

static inline __attribute__((no_instrument_function)) int arch_spin_is_locked(arch_spinlock_t *lock)
{
 struct __raw_tickets tmp = ({ union { typeof(lock->tickets) __val; char __c[1]; } __u; __read_once_size(&(lock->tickets), __u.__c, sizeof(lock->tickets)); __u.__val; });

 return !__tickets_equal(tmp.tail, tmp.head);
}

static inline __attribute__((no_instrument_function)) int arch_spin_is_contended(arch_spinlock_t *lock)
{
 struct __raw_tickets tmp = ({ union { typeof(lock->tickets) __val; char __c[1]; } __u; __read_once_size(&(lock->tickets), __u.__c, sizeof(lock->tickets)); __u.__val; });

 tmp.head &= ~((__ticket_t)1);
 return (__ticket_t)(tmp.tail - tmp.head) > ((__ticket_t)2);
}


static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) void arch_spin_lock_flags(arch_spinlock_t *lock,
        unsigned long flags)
{
 arch_spin_lock(lock);
}

static inline __attribute__((no_instrument_function)) void arch_spin_unlock_wait(arch_spinlock_t *lock)
{
 __ticket_t head = ({ union { typeof(lock->tickets.head) __val; char __c[1]; } __u; __read_once_size(&(lock->tickets.head), __u.__c, sizeof(lock->tickets.head)); __u.__val; });

 for (;;) {
  struct __raw_tickets tmp = ({ union { typeof(lock->tickets) __val; char __c[1]; } __u; __read_once_size(&(lock->tickets), __u.__c, sizeof(lock->tickets)); __u.__val; });




  if (__tickets_equal(tmp.head, tmp.tail) ||
    !__tickets_equal(tmp.head, head))
   break;

  cpu_relax();
 }
}
# 214 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/spinlock.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/qrwlock.h" 1







static inline __attribute__((no_instrument_function)) void queue_write_unlock(struct qrwlock *lock)
{
        __asm__ __volatile__("": : :"memory");
        (*({ __attribute__((unused)) typeof(*(u8 *)&lock->cnts) __var = ( typeof(*(u8 *)&lock->cnts)) 0; (volatile typeof(*(u8 *)&lock->cnts) *)&(*(u8 *)&lock->cnts); })) = 0;
}


# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/qrwlock.h" 1
# 39 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/qrwlock.h"
extern void queue_read_lock_slowpath(struct qrwlock *lock);
extern void queue_write_lock_slowpath(struct qrwlock *lock);





static inline __attribute__((no_instrument_function)) int queue_read_can_lock(struct qrwlock *lock)
{
 return !(atomic_read(&lock->cnts) & 0xff);
}





static inline __attribute__((no_instrument_function)) int queue_write_can_lock(struct qrwlock *lock)
{
 return !atomic_read(&lock->cnts);
}






static inline __attribute__((no_instrument_function)) int queue_read_trylock(struct qrwlock *lock)
{
 u32 cnts;

 cnts = atomic_read(&lock->cnts);
 if (__builtin_expect(!!(!(cnts & 0xff)), 1)) {
  cnts = (u32)atomic_add_return((1U << 8), &lock->cnts);
  if (__builtin_expect(!!(!(cnts & 0xff)), 1))
   return 1;
  atomic_sub((1U << 8), &lock->cnts);
 }
 return 0;
}






static inline __attribute__((no_instrument_function)) int queue_write_trylock(struct qrwlock *lock)
{
 u32 cnts;

 cnts = atomic_read(&lock->cnts);
 if (__builtin_expect(!!(cnts), 0))
  return 0;

 return __builtin_expect(!!(atomic_cmpxchg(&lock->cnts, cnts, cnts | 0xff) == cnts), 1)
                                          ;
}




static inline __attribute__((no_instrument_function)) void queue_read_lock(struct qrwlock *lock)
{
 u32 cnts;

 cnts = atomic_add_return((1U << 8), &lock->cnts);
 if (__builtin_expect(!!(!(cnts & 0xff)), 1))
  return;


 queue_read_lock_slowpath(lock);
}





static inline __attribute__((no_instrument_function)) void queue_write_lock(struct qrwlock *lock)
{

 if (atomic_cmpxchg(&lock->cnts, 0, 0xff) == 0)
  return;

 queue_write_lock_slowpath(lock);
}





static inline __attribute__((no_instrument_function)) void queue_read_unlock(struct qrwlock *lock)
{



 __asm__ __volatile__("": : :"memory");
 atomic_sub((1U << 8), &lock->cnts);
}
# 16 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/qrwlock.h" 2
# 215 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/spinlock.h" 2
# 88 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock.h" 2
# 155 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock.h"
static inline __attribute__((no_instrument_function)) void do_raw_spin_lock(raw_spinlock_t *lock)
{
 (void)0;
 arch_spin_lock(&lock->raw_lock);
}

static inline __attribute__((no_instrument_function)) void
do_raw_spin_lock_flags(raw_spinlock_t *lock, unsigned long *flags)
{
 (void)0;
 arch_spin_lock_flags(&lock->raw_lock, *flags);
}

static inline __attribute__((no_instrument_function)) int do_raw_spin_trylock(raw_spinlock_t *lock)
{
 return arch_spin_trylock(&(lock)->raw_lock);
}

static inline __attribute__((no_instrument_function)) void do_raw_spin_unlock(raw_spinlock_t *lock)
{
 arch_spin_unlock(&lock->raw_lock);
 (void)0;
}
# 281 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/rwlock.h" 1
# 282 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock.h" 2





# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock_api_smp.h" 1
# 18 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock_api_smp.h"
int in_lock_functions(unsigned long addr);



void __attribute__((section(".spinlock.text"))) _raw_spin_lock(raw_spinlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _raw_spin_lock_nested(raw_spinlock_t *lock, int subclass)
        ;
void __attribute__((section(".spinlock.text")))
_raw_spin_lock_nest_lock(raw_spinlock_t *lock, struct lockdep_map *map)
        ;
void __attribute__((section(".spinlock.text"))) _raw_spin_lock_bh(raw_spinlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _raw_spin_lock_irq(raw_spinlock_t *lock)
        ;

unsigned long __attribute__((section(".spinlock.text"))) _raw_spin_lock_irqsave(raw_spinlock_t *lock)
        ;
unsigned long __attribute__((section(".spinlock.text")))
_raw_spin_lock_irqsave_nested(raw_spinlock_t *lock, int subclass)
        ;
int __attribute__((section(".spinlock.text"))) _raw_spin_trylock(raw_spinlock_t *lock);
int __attribute__((section(".spinlock.text"))) _raw_spin_trylock_bh(raw_spinlock_t *lock);
void __attribute__((section(".spinlock.text"))) _raw_spin_unlock(raw_spinlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _raw_spin_unlock_bh(raw_spinlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _raw_spin_unlock_irq(raw_spinlock_t *lock) ;
void __attribute__((section(".spinlock.text")))
_raw_spin_unlock_irqrestore(raw_spinlock_t *lock, unsigned long flags)
        ;
# 86 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock_api_smp.h"
static inline __attribute__((no_instrument_function)) int __raw_spin_trylock(raw_spinlock_t *lock)
{
 __asm__ __volatile__("": : :"memory");
 if (do_raw_spin_trylock(lock)) {
  do { } while (0);
  return 1;
 }
 __asm__ __volatile__("": : :"memory");
 return 0;
}
# 104 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock_api_smp.h"
static inline __attribute__((no_instrument_function)) unsigned long __raw_spin_lock_irqsave(raw_spinlock_t *lock)
{
 unsigned long flags;

 do { do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); flags = arch_local_irq_save(); } while (0); do { } while (0); } while (0);
 __asm__ __volatile__("": : :"memory");
 do { } while (0);
# 119 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock_api_smp.h"
 do_raw_spin_lock_flags(lock, &flags);

 return flags;
}

static inline __attribute__((no_instrument_function)) void __raw_spin_lock_irq(raw_spinlock_t *lock)
{
 do { arch_local_irq_disable(); do { } while (0); } while (0);
 __asm__ __volatile__("": : :"memory");
 do { } while (0);
 do_raw_spin_lock(lock);
}

static inline __attribute__((no_instrument_function)) void __raw_spin_lock_bh(raw_spinlock_t *lock)
{
 __local_bh_disable_ip((unsigned long)__builtin_return_address(0), ((2 * (1UL << (0 + 8))) + 0));
 do { } while (0);
 do_raw_spin_lock(lock);
}

static inline __attribute__((no_instrument_function)) void __raw_spin_lock(raw_spinlock_t *lock)
{
 __asm__ __volatile__("": : :"memory");
 do { } while (0);
 do_raw_spin_lock(lock);
}



static inline __attribute__((no_instrument_function)) void __raw_spin_unlock(raw_spinlock_t *lock)
{
 do { } while (0);
 do_raw_spin_unlock(lock);
 __asm__ __volatile__("": : :"memory");
}

static inline __attribute__((no_instrument_function)) void __raw_spin_unlock_irqrestore(raw_spinlock_t *lock,
         unsigned long flags)
{
 do { } while (0);
 do_raw_spin_unlock(lock);
 do { if (({ ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); arch_irqs_disabled_flags(flags); })) { do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); arch_local_irq_restore(flags); } while (0); do { } while (0); } else { do { } while (0); do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); arch_local_irq_restore(flags); } while (0); } } while (0);
 __asm__ __volatile__("": : :"memory");
}

static inline __attribute__((no_instrument_function)) void __raw_spin_unlock_irq(raw_spinlock_t *lock)
{
 do { } while (0);
 do_raw_spin_unlock(lock);
 do { do { } while (0); arch_local_irq_enable(); } while (0);
 __asm__ __volatile__("": : :"memory");
}

static inline __attribute__((no_instrument_function)) void __raw_spin_unlock_bh(raw_spinlock_t *lock)
{
 do { } while (0);
 do_raw_spin_unlock(lock);
 __local_bh_enable_ip((unsigned long)__builtin_return_address(0), ((2 * (1UL << (0 + 8))) + 0));
}

static inline __attribute__((no_instrument_function)) int __raw_spin_trylock_bh(raw_spinlock_t *lock)
{
 __local_bh_disable_ip((unsigned long)__builtin_return_address(0), ((2 * (1UL << (0 + 8))) + 0));
 if (do_raw_spin_trylock(lock)) {
  do { } while (0);
  return 1;
 }
 __local_bh_enable_ip((unsigned long)__builtin_return_address(0), ((2 * (1UL << (0 + 8))) + 0));
 return 0;
}

# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/rwlock_api_smp.h" 1
# 18 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/rwlock_api_smp.h"
void __attribute__((section(".spinlock.text"))) _raw_read_lock(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _raw_write_lock(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _raw_read_lock_bh(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _raw_write_lock_bh(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _raw_read_lock_irq(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _raw_write_lock_irq(rwlock_t *lock) ;
unsigned long __attribute__((section(".spinlock.text"))) _raw_read_lock_irqsave(rwlock_t *lock)
       ;
unsigned long __attribute__((section(".spinlock.text"))) _raw_write_lock_irqsave(rwlock_t *lock)
       ;
int __attribute__((section(".spinlock.text"))) _raw_read_trylock(rwlock_t *lock);
int __attribute__((section(".spinlock.text"))) _raw_write_trylock(rwlock_t *lock);
void __attribute__((section(".spinlock.text"))) _raw_read_unlock(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _raw_write_unlock(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _raw_read_unlock_bh(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _raw_write_unlock_bh(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _raw_read_unlock_irq(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _raw_write_unlock_irq(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text")))
_raw_read_unlock_irqrestore(rwlock_t *lock, unsigned long flags)
       ;
void __attribute__((section(".spinlock.text")))
_raw_write_unlock_irqrestore(rwlock_t *lock, unsigned long flags)
       ;
# 117 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/rwlock_api_smp.h"
static inline __attribute__((no_instrument_function)) int __raw_read_trylock(rwlock_t *lock)
{
 __asm__ __volatile__("": : :"memory");
 if (queue_read_trylock(&(lock)->raw_lock)) {
  do { } while (0);
  return 1;
 }
 __asm__ __volatile__("": : :"memory");
 return 0;
}

static inline __attribute__((no_instrument_function)) int __raw_write_trylock(rwlock_t *lock)
{
 __asm__ __volatile__("": : :"memory");
 if (queue_write_trylock(&(lock)->raw_lock)) {
  do { } while (0);
  return 1;
 }
 __asm__ __volatile__("": : :"memory");
 return 0;
}
# 146 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/rwlock_api_smp.h"
static inline __attribute__((no_instrument_function)) void __raw_read_lock(rwlock_t *lock)
{
 __asm__ __volatile__("": : :"memory");
 do { } while (0);
 do {(void)0; queue_read_lock(&(lock)->raw_lock); } while (0);
}

static inline __attribute__((no_instrument_function)) unsigned long __raw_read_lock_irqsave(rwlock_t *lock)
{
 unsigned long flags;

 do { do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); flags = arch_local_irq_save(); } while (0); do { } while (0); } while (0);
 __asm__ __volatile__("": : :"memory");
 do { } while (0);
 do {(void)0; queue_read_lock(&((lock))->raw_lock); } while (0)
                                       ;
 return flags;
}

static inline __attribute__((no_instrument_function)) void __raw_read_lock_irq(rwlock_t *lock)
{
 do { arch_local_irq_disable(); do { } while (0); } while (0);
 __asm__ __volatile__("": : :"memory");
 do { } while (0);
 do {(void)0; queue_read_lock(&(lock)->raw_lock); } while (0);
}

static inline __attribute__((no_instrument_function)) void __raw_read_lock_bh(rwlock_t *lock)
{
 __local_bh_disable_ip((unsigned long)__builtin_return_address(0), ((2 * (1UL << (0 + 8))) + 0));
 do { } while (0);
 do {(void)0; queue_read_lock(&(lock)->raw_lock); } while (0);
}

static inline __attribute__((no_instrument_function)) unsigned long __raw_write_lock_irqsave(rwlock_t *lock)
{
 unsigned long flags;

 do { do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); flags = arch_local_irq_save(); } while (0); do { } while (0); } while (0);
 __asm__ __volatile__("": : :"memory");
 do { } while (0);
 do {(void)0; queue_write_lock(&((lock))->raw_lock); } while (0)
                                        ;
 return flags;
}

static inline __attribute__((no_instrument_function)) void __raw_write_lock_irq(rwlock_t *lock)
{
 do { arch_local_irq_disable(); do { } while (0); } while (0);
 __asm__ __volatile__("": : :"memory");
 do { } while (0);
 do {(void)0; queue_write_lock(&(lock)->raw_lock); } while (0);
}

static inline __attribute__((no_instrument_function)) void __raw_write_lock_bh(rwlock_t *lock)
{
 __local_bh_disable_ip((unsigned long)__builtin_return_address(0), ((2 * (1UL << (0 + 8))) + 0));
 do { } while (0);
 do {(void)0; queue_write_lock(&(lock)->raw_lock); } while (0);
}

static inline __attribute__((no_instrument_function)) void __raw_write_lock(rwlock_t *lock)
{
 __asm__ __volatile__("": : :"memory");
 do { } while (0);
 do {(void)0; queue_write_lock(&(lock)->raw_lock); } while (0);
}



static inline __attribute__((no_instrument_function)) void __raw_write_unlock(rwlock_t *lock)
{
 do { } while (0);
 do {queue_write_unlock(&(lock)->raw_lock); (void)0; } while (0);
 __asm__ __volatile__("": : :"memory");
}

static inline __attribute__((no_instrument_function)) void __raw_read_unlock(rwlock_t *lock)
{
 do { } while (0);
 do {queue_read_unlock(&(lock)->raw_lock); (void)0; } while (0);
 __asm__ __volatile__("": : :"memory");
}

static inline __attribute__((no_instrument_function)) void
__raw_read_unlock_irqrestore(rwlock_t *lock, unsigned long flags)
{
 do { } while (0);
 do {queue_read_unlock(&(lock)->raw_lock); (void)0; } while (0);
 do { if (({ ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); arch_irqs_disabled_flags(flags); })) { do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); arch_local_irq_restore(flags); } while (0); do { } while (0); } else { do { } while (0); do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); arch_local_irq_restore(flags); } while (0); } } while (0);
 __asm__ __volatile__("": : :"memory");
}

static inline __attribute__((no_instrument_function)) void __raw_read_unlock_irq(rwlock_t *lock)
{
 do { } while (0);
 do {queue_read_unlock(&(lock)->raw_lock); (void)0; } while (0);
 do { do { } while (0); arch_local_irq_enable(); } while (0);
 __asm__ __volatile__("": : :"memory");
}

static inline __attribute__((no_instrument_function)) void __raw_read_unlock_bh(rwlock_t *lock)
{
 do { } while (0);
 do {queue_read_unlock(&(lock)->raw_lock); (void)0; } while (0);
 __local_bh_enable_ip((unsigned long)__builtin_return_address(0), ((2 * (1UL << (0 + 8))) + 0));
}

static inline __attribute__((no_instrument_function)) void __raw_write_unlock_irqrestore(rwlock_t *lock,
          unsigned long flags)
{
 do { } while (0);
 do {queue_write_unlock(&(lock)->raw_lock); (void)0; } while (0);
 do { if (({ ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); arch_irqs_disabled_flags(flags); })) { do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); arch_local_irq_restore(flags); } while (0); do { } while (0); } else { do { } while (0); do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); arch_local_irq_restore(flags); } while (0); } } while (0);
 __asm__ __volatile__("": : :"memory");
}

static inline __attribute__((no_instrument_function)) void __raw_write_unlock_irq(rwlock_t *lock)
{
 do { } while (0);
 do {queue_write_unlock(&(lock)->raw_lock); (void)0; } while (0);
 do { do { } while (0); arch_local_irq_enable(); } while (0);
 __asm__ __volatile__("": : :"memory");
}

static inline __attribute__((no_instrument_function)) void __raw_write_unlock_bh(rwlock_t *lock)
{
 do { } while (0);
 do {queue_write_unlock(&(lock)->raw_lock); (void)0; } while (0);
 __local_bh_enable_ip((unsigned long)__builtin_return_address(0), ((2 * (1UL << (0 + 8))) + 0));
}
# 191 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock_api_smp.h" 2
# 288 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock.h" 2
# 296 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock.h"
static inline __attribute__((no_instrument_function)) raw_spinlock_t *spinlock_check(spinlock_t *lock)
{
 return &lock->rlock;
}







static inline __attribute__((no_instrument_function)) void spin_lock(spinlock_t *lock)
{
 _raw_spin_lock(&lock->rlock);
}

static inline __attribute__((no_instrument_function)) void spin_lock_bh(spinlock_t *lock)
{
 _raw_spin_lock_bh(&lock->rlock);
}

static inline __attribute__((no_instrument_function)) int spin_trylock(spinlock_t *lock)
{
 return (_raw_spin_trylock(&lock->rlock));
}
# 332 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock.h"
static inline __attribute__((no_instrument_function)) void spin_lock_irq(spinlock_t *lock)
{
 _raw_spin_lock_irq(&lock->rlock);
}
# 347 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock.h"
static inline __attribute__((no_instrument_function)) void spin_unlock(spinlock_t *lock)
{
 _raw_spin_unlock(&lock->rlock);
}

static inline __attribute__((no_instrument_function)) void spin_unlock_bh(spinlock_t *lock)
{
 _raw_spin_unlock_bh(&lock->rlock);
}

static inline __attribute__((no_instrument_function)) void spin_unlock_irq(spinlock_t *lock)
{
 __raw_spin_unlock_irq(&lock->rlock);
}

static inline __attribute__((no_instrument_function)) void spin_unlock_irqrestore(spinlock_t *lock, unsigned long flags)
{
 do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); _raw_spin_unlock_irqrestore(&lock->rlock, flags); } while (0);
}

static inline __attribute__((no_instrument_function)) int spin_trylock_bh(spinlock_t *lock)
{
 return (_raw_spin_trylock_bh(&lock->rlock));
}

static inline __attribute__((no_instrument_function)) int spin_trylock_irq(spinlock_t *lock)
{
 return ({ do { arch_local_irq_disable(); do { } while (0); } while (0); (_raw_spin_trylock(&lock->rlock)) ? 1 : ({ do { do { } while (0); arch_local_irq_enable(); } while (0); 0; }); });
}






static inline __attribute__((no_instrument_function)) void spin_unlock_wait(spinlock_t *lock)
{
 arch_spin_unlock_wait(&(&lock->rlock)->raw_lock);
}

static inline __attribute__((no_instrument_function)) int spin_is_locked(spinlock_t *lock)
{
 return arch_spin_is_locked(&(&lock->rlock)->raw_lock);
}

static inline __attribute__((no_instrument_function)) int spin_is_contended(spinlock_t *lock)
{
 return arch_spin_is_contended(&(&lock->rlock)->raw_lock);
}

static inline __attribute__((no_instrument_function)) int spin_can_lock(spinlock_t *lock)
{
 return (!arch_spin_is_locked(&(&lock->rlock)->raw_lock));
}
# 417 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/spinlock.h"
extern int _atomic_dec_and_lock(atomic_t *atomic, spinlock_t *lock);
# 36 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/seqlock.h" 2
# 46 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/seqlock.h"
typedef struct seqcount {
 unsigned sequence;



} seqcount_t;

static inline __attribute__((no_instrument_function)) void __seqcount_init(seqcount_t *s, const char *name,
       struct lock_class_key *key)
{



 do { (void)(name); (void)(key); } while (0);
 s->sequence = 0;
}
# 106 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/seqlock.h"
static inline __attribute__((no_instrument_function)) unsigned __read_seqcount_begin(const seqcount_t *s)
{
 unsigned ret;

repeat:
 ret = (*({ __attribute__((unused)) typeof(s->sequence) __var = ( typeof(s->sequence)) 0; (volatile typeof(s->sequence) *)&(s->sequence); }));
 if (__builtin_expect(!!(ret & 1), 0)) {
  cpu_relax();
  goto repeat;
 }
 return ret;
}
# 128 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/seqlock.h"
static inline __attribute__((no_instrument_function)) unsigned raw_read_seqcount(const seqcount_t *s)
{
 unsigned ret = (*({ __attribute__((unused)) typeof(s->sequence) __var = ( typeof(s->sequence)) 0; (volatile typeof(s->sequence) *)&(s->sequence); }));
 __asm__ __volatile__("": : :"memory");
 return ret;
}
# 144 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/seqlock.h"
static inline __attribute__((no_instrument_function)) unsigned raw_read_seqcount_begin(const seqcount_t *s)
{
 unsigned ret = __read_seqcount_begin(s);
 __asm__ __volatile__("": : :"memory");
 return ret;
}
# 160 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/seqlock.h"
static inline __attribute__((no_instrument_function)) unsigned read_seqcount_begin(const seqcount_t *s)
{
 ;
 return raw_read_seqcount_begin(s);
}
# 180 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/seqlock.h"
static inline __attribute__((no_instrument_function)) unsigned raw_seqcount_begin(const seqcount_t *s)
{
 unsigned ret = (*({ __attribute__((unused)) typeof(s->sequence) __var = ( typeof(s->sequence)) 0; (volatile typeof(s->sequence) *)&(s->sequence); }));
 __asm__ __volatile__("": : :"memory");
 return ret & ~1;
}
# 201 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/seqlock.h"
static inline __attribute__((no_instrument_function)) int __read_seqcount_retry(const seqcount_t *s, unsigned start)
{
 return __builtin_expect(!!(s->sequence != start), 0);
}
# 216 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/seqlock.h"
static inline __attribute__((no_instrument_function)) int read_seqcount_retry(const seqcount_t *s, unsigned start)
{
 __asm__ __volatile__("": : :"memory");
 return __read_seqcount_retry(s, start);
}



static inline __attribute__((no_instrument_function)) void raw_write_seqcount_begin(seqcount_t *s)
{
 s->sequence++;
 __asm__ __volatile__("": : :"memory");
}

static inline __attribute__((no_instrument_function)) void raw_write_seqcount_end(seqcount_t *s)
{
 __asm__ __volatile__("": : :"memory");
 s->sequence++;
}





static inline __attribute__((no_instrument_function)) void raw_write_seqcount_latch(seqcount_t *s)
{
       __asm__ __volatile__("": : :"memory");
       s->sequence++;
       __asm__ __volatile__("": : :"memory");
}





static inline __attribute__((no_instrument_function)) void write_seqcount_begin_nested(seqcount_t *s, int subclass)
{
 raw_write_seqcount_begin(s);
 do { } while (0);
}

static inline __attribute__((no_instrument_function)) void write_seqcount_begin(seqcount_t *s)
{
 write_seqcount_begin_nested(s, 0);
}

static inline __attribute__((no_instrument_function)) void write_seqcount_end(seqcount_t *s)
{
 do { } while (0);
 raw_write_seqcount_end(s);
}
# 275 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/seqlock.h"
static inline __attribute__((no_instrument_function)) void write_seqcount_barrier(seqcount_t *s)
{
 __asm__ __volatile__("": : :"memory");
 s->sequence+=2;
}

typedef struct {
 struct seqcount seqcount;
 spinlock_t lock;
} seqlock_t;
# 308 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/seqlock.h"
static inline __attribute__((no_instrument_function)) unsigned read_seqbegin(const seqlock_t *sl)
{
 return read_seqcount_begin(&sl->seqcount);
}

static inline __attribute__((no_instrument_function)) unsigned read_seqretry(const seqlock_t *sl, unsigned start)
{
 return read_seqcount_retry(&sl->seqcount, start);
}






static inline __attribute__((no_instrument_function)) void write_seqlock(seqlock_t *sl)
{
 spin_lock(&sl->lock);
 write_seqcount_begin(&sl->seqcount);
}

static inline __attribute__((no_instrument_function)) void write_sequnlock(seqlock_t *sl)
{
 write_seqcount_end(&sl->seqcount);
 spin_unlock(&sl->lock);
}

static inline __attribute__((no_instrument_function)) void write_seqlock_bh(seqlock_t *sl)
{
 spin_lock_bh(&sl->lock);
 write_seqcount_begin(&sl->seqcount);
}

static inline __attribute__((no_instrument_function)) void write_sequnlock_bh(seqlock_t *sl)
{
 write_seqcount_end(&sl->seqcount);
 spin_unlock_bh(&sl->lock);
}

static inline __attribute__((no_instrument_function)) void write_seqlock_irq(seqlock_t *sl)
{
 spin_lock_irq(&sl->lock);
 write_seqcount_begin(&sl->seqcount);
}

static inline __attribute__((no_instrument_function)) void write_sequnlock_irq(seqlock_t *sl)
{
 write_seqcount_end(&sl->seqcount);
 spin_unlock_irq(&sl->lock);
}

static inline __attribute__((no_instrument_function)) unsigned long __write_seqlock_irqsave(seqlock_t *sl)
{
 unsigned long flags;

 do { do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); flags = _raw_spin_lock_irqsave(spinlock_check(&sl->lock)); } while (0); } while (0);
 write_seqcount_begin(&sl->seqcount);
 return flags;
}




static inline __attribute__((no_instrument_function)) void
write_sequnlock_irqrestore(seqlock_t *sl, unsigned long flags)
{
 write_seqcount_end(&sl->seqcount);
 spin_unlock_irqrestore(&sl->lock, flags);
}






static inline __attribute__((no_instrument_function)) void read_seqlock_excl(seqlock_t *sl)
{
 spin_lock(&sl->lock);
}

static inline __attribute__((no_instrument_function)) void read_sequnlock_excl(seqlock_t *sl)
{
 spin_unlock(&sl->lock);
}
# 403 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/seqlock.h"
static inline __attribute__((no_instrument_function)) void read_seqbegin_or_lock(seqlock_t *lock, int *seq)
{
 if (!(*seq & 1))
  *seq = read_seqbegin(lock);
 else
  read_seqlock_excl(lock);
}

static inline __attribute__((no_instrument_function)) int need_seqretry(seqlock_t *lock, int seq)
{
 return !(seq & 1) && read_seqretry(lock, seq);
}

static inline __attribute__((no_instrument_function)) void done_seqretry(seqlock_t *lock, int seq)
{
 if (seq & 1)
  read_sequnlock_excl(lock);
}

static inline __attribute__((no_instrument_function)) void read_seqlock_excl_bh(seqlock_t *sl)
{
 spin_lock_bh(&sl->lock);
}

static inline __attribute__((no_instrument_function)) void read_sequnlock_excl_bh(seqlock_t *sl)
{
 spin_unlock_bh(&sl->lock);
}

static inline __attribute__((no_instrument_function)) void read_seqlock_excl_irq(seqlock_t *sl)
{
 spin_lock_irq(&sl->lock);
}

static inline __attribute__((no_instrument_function)) void read_sequnlock_excl_irq(seqlock_t *sl)
{
 spin_unlock_irq(&sl->lock);
}

static inline __attribute__((no_instrument_function)) unsigned long __read_seqlock_excl_irqsave(seqlock_t *sl)
{
 unsigned long flags;

 do { do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); flags = _raw_spin_lock_irqsave(spinlock_check(&sl->lock)); } while (0); } while (0);
 return flags;
}




static inline __attribute__((no_instrument_function)) void
read_sequnlock_excl_irqrestore(seqlock_t *sl, unsigned long flags)
{
 spin_unlock_irqrestore(&sl->lock, flags);
}

static inline __attribute__((no_instrument_function)) unsigned long
read_seqbegin_or_lock_irqsave(seqlock_t *lock, int *seq)
{
 unsigned long flags = 0;

 if (!(*seq & 1))
  *seq = read_seqbegin(lock);
 else
  do { flags = __read_seqlock_excl_irqsave(lock); } while (0);

 return flags;
}

static inline __attribute__((no_instrument_function)) void
done_seqretry_irqrestore(seqlock_t *lock, int seq, unsigned long flags)
{
 if (seq & 1)
  read_sequnlock_excl_irqrestore(lock, flags);
}
# 6 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/time.h" 2

# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/time64.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/time.h" 1
# 9 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/time.h"
struct timespec {
 __kernel_time_t tv_sec;
 long tv_nsec;
};


struct timeval {
 __kernel_time_t tv_sec;
 __kernel_suseconds_t tv_usec;
};

struct timezone {
 int tz_minuteswest;
 int tz_dsttime;
};
# 34 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/time.h"
struct itimerspec {
 struct timespec it_interval;
 struct timespec it_value;
};

struct itimerval {
 struct timeval it_interval;
 struct timeval it_value;
};
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/time64.h" 2

typedef __s64 time64_t;
# 36 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/time64.h"
static inline __attribute__((no_instrument_function)) struct timespec timespec64_to_timespec(const struct timespec ts64)
{
 return ts64;
}

static inline __attribute__((no_instrument_function)) struct timespec timespec_to_timespec64(const struct timespec ts)
{
 return ts;
}
# 8 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/time.h" 2

extern struct timezone sys_tz;



static inline __attribute__((no_instrument_function)) int timespec_equal(const struct timespec *a,
                                 const struct timespec *b)
{
 return (a->tv_sec == b->tv_sec) && (a->tv_nsec == b->tv_nsec);
}






static inline __attribute__((no_instrument_function)) int timespec_compare(const struct timespec *lhs, const struct timespec *rhs)
{
 if (lhs->tv_sec < rhs->tv_sec)
  return -1;
 if (lhs->tv_sec > rhs->tv_sec)
  return 1;
 return lhs->tv_nsec - rhs->tv_nsec;
}

static inline __attribute__((no_instrument_function)) int timeval_compare(const struct timeval *lhs, const struct timeval *rhs)
{
 if (lhs->tv_sec < rhs->tv_sec)
  return -1;
 if (lhs->tv_sec > rhs->tv_sec)
  return 1;
 return lhs->tv_usec - rhs->tv_usec;
}

extern time64_t mktime64(const unsigned int year, const unsigned int mon,
   const unsigned int day, const unsigned int hour,
   const unsigned int min, const unsigned int sec);




static inline __attribute__((no_instrument_function)) unsigned long mktime(const unsigned int year,
   const unsigned int mon, const unsigned int day,
   const unsigned int hour, const unsigned int min,
   const unsigned int sec)
{
 return mktime64(year, mon, day, hour, min, sec);
}

extern void set_normalized_timespec(struct timespec *ts, time_t sec, s64 nsec);






extern struct timespec timespec_add_safe(const struct timespec lhs,
      const struct timespec rhs);


static inline __attribute__((no_instrument_function)) struct timespec timespec_add(struct timespec lhs,
      struct timespec rhs)
{
 struct timespec ts_delta;
 set_normalized_timespec(&ts_delta, lhs.tv_sec + rhs.tv_sec,
    lhs.tv_nsec + rhs.tv_nsec);
 return ts_delta;
}




static inline __attribute__((no_instrument_function)) struct timespec timespec_sub(struct timespec lhs,
      struct timespec rhs)
{
 struct timespec ts_delta;
 set_normalized_timespec(&ts_delta, lhs.tv_sec - rhs.tv_sec,
    lhs.tv_nsec - rhs.tv_nsec);
 return ts_delta;
}




static inline __attribute__((no_instrument_function)) bool timespec_valid(const struct timespec *ts)
{

 if (ts->tv_sec < 0)
  return false;

 if ((unsigned long)ts->tv_nsec >= 1000000000L)
  return false;
 return true;
}

static inline __attribute__((no_instrument_function)) bool timespec_valid_strict(const struct timespec *ts)
{
 if (!timespec_valid(ts))
  return false;

 if ((unsigned long long)ts->tv_sec >= (((s64)~((u64)1 << 63)) / 1000000000L))
  return false;
 return true;
}

static inline __attribute__((no_instrument_function)) bool timeval_valid(const struct timeval *tv)
{

 if (tv->tv_sec < 0)
  return false;


 if (tv->tv_usec < 0 || tv->tv_usec >= 1000000L)
  return false;

 return true;
}

extern struct timespec timespec_trunc(struct timespec t, unsigned gran);
# 144 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/time.h"
struct itimerval;
extern int do_setitimer(int which, struct itimerval *value,
   struct itimerval *ovalue);
extern int do_getitimer(int which, struct itimerval *value);

extern unsigned int alarm_setitimer(unsigned int seconds);

extern long do_utimes(int dfd, const char *filename, struct timespec *times, int flags);

struct tms;
extern void do_sys_times(struct tms *);





struct tm {




 int tm_sec;

 int tm_min;

 int tm_hour;

 int tm_mday;

 int tm_mon;

 long tm_year;

 int tm_wday;

 int tm_yday;
};

void time_to_tm(time_t totalsecs, int offset, struct tm *result);
# 191 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/time.h"
static inline __attribute__((no_instrument_function)) s64 timespec_to_ns(const struct timespec *ts)
{
 return ((s64) ts->tv_sec * 1000000000L) + ts->tv_nsec;
}
# 203 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/time.h"
static inline __attribute__((no_instrument_function)) s64 timeval_to_ns(const struct timeval *tv)
{
 return ((s64) tv->tv_sec * 1000000000L) +
  tv->tv_usec * 1000L;
}







extern struct timespec ns_to_timespec(const s64 nsec);







extern struct timeval ns_to_timeval(const s64 nsec);
# 233 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/time.h"
static inline __attribute__((no_instrument_function)) __attribute__((always_inline)) void timespec_add_ns(struct timespec *a, u64 ns)
{
 a->tv_sec += __iter_div_u64_rem(a->tv_nsec + ns, 1000000000L, &ns);
 a->tv_nsec = ns;
}
# 7 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/signal.h" 2



struct siginfo;
# 93 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/signal.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/asm-generic/signal-defs.h" 1
# 17 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/asm-generic/signal-defs.h"
typedef void __signalfn_t(int);
typedef __signalfn_t *__sighandler_t;

typedef void __restorefn_t(void);
typedef __restorefn_t *__sigrestore_t;
# 94 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/signal.h" 2
# 127 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/signal.h"
typedef struct sigaltstack {
 void *ss_sp;
 int ss_flags;
 size_t ss_size;
} stack_t;
# 32 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/asm/signal.h" 2

extern void do_notify_resume(struct pt_regs *, void *, __u32);
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/signal.h" 2
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/siginfo.h" 1
# 14 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/siginfo.h"
# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/siginfo.h" 1



# 1 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/asm-generic/siginfo.h" 1






typedef union sigval {
 int sival_int;
 void *sival_ptr;
} sigval_t;
# 48 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/asm-generic/siginfo.h"
typedef struct siginfo {
 int si_signo;
 int si_errno;
 int si_code;

 union {
  int _pad[((128 - (4 * sizeof(int))) / sizeof(int))];


  struct {
   __kernel_pid_t _pid;
   __kernel_uid32_t _uid;
  } _kill;


  struct {
   __kernel_timer_t _tid;
   int _overrun;
   char _pad[sizeof( __kernel_uid32_t) - sizeof(int)];
   sigval_t _sigval;
   int _sys_private;
  } _timer;


  struct {
   __kernel_pid_t _pid;
   __kernel_uid32_t _uid;
   sigval_t _sigval;
  } _rt;


  struct {
   __kernel_pid_t _pid;
   __kernel_uid32_t _uid;
   int _status;
   __kernel_clock_t _utime;
   __kernel_clock_t _stime;
  } _sigchld;


  struct {
   void *_addr;



   short _addr_lsb;
   struct {
    void *_lower;
    void *_upper;
   } _addr_bnd;
  } _sigfault;


  struct {
   long _band;
   int _fd;
  } _sigpoll;


  struct {
   void *_call_addr;
   int _syscall;
   unsigned int _arch;
  } _sigsys;
 } _sifields;
} siginfo_t;
# 285 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/asm-generic/siginfo.h"
typedef struct sigevent {
 sigval_t sigev_value;
 int sigev_signo;
 int sigev_notify;
 union {
  int _pad[((64 - (sizeof(int) * 2 + sizeof(sigval_t))) / sizeof(int))];
   int _tid;

  struct {
   void (*_function)(sigval_t);
   void *_attribute;
  } _sigev_thread;
 } _sigev_un;
} sigevent_t;
# 5 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/siginfo.h" 2
# 17 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/asm-generic/siginfo.h"
struct siginfo;
void do_schedule_next_timer(struct siginfo *info);





static inline __attribute__((no_instrument_function)) void copy_siginfo(struct siginfo *to, struct siginfo *from)
{
 if (from->si_code < 0)
  memcpy(to, from, sizeof(*to));
 else

  memcpy(to, from, (4 * sizeof(int)) + sizeof(from->_sifields._sigchld));
}



extern int copy_siginfo_to_user(struct siginfo *to, const struct siginfo *from);
# 15 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/arch/x86/include/uapi/asm/siginfo.h" 2
# 6 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/uapi/linux/signal.h" 2
# 7 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/signal.h" 2

struct task_struct;


extern int print_fatal_signals;




struct sigqueue {
 struct list_head list;
 int flags;
 siginfo_t info;
 struct user_struct *user;
};




struct sigpending {
 struct list_head list;
 sigset_t signal;
};
# 40 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/signal.h"
static inline __attribute__((no_instrument_function)) void sigaddset(sigset_t *set, int _sig)
{
 unsigned long sig = _sig - 1;
 if ((64 / 64) == 1)
  set->sig[0] |= 1UL << sig;
 else
  set->sig[sig / 64] |= 1UL << (sig % 64);
}

static inline __attribute__((no_instrument_function)) void sigdelset(sigset_t *set, int _sig)
{
 unsigned long sig = _sig - 1;
 if ((64 / 64) == 1)
  set->sig[0] &= ~(1UL << sig);
 else
  set->sig[sig / 64] &= ~(1UL << (sig % 64));
}

static inline __attribute__((no_instrument_function)) int sigismember(sigset_t *set, int _sig)
{
 unsigned long sig = _sig - 1;
 if ((64 / 64) == 1)
  return 1 & (set->sig[0] >> sig);
 else
  return 1 & (set->sig[sig / 64] >> (sig % 64));
}



static inline __attribute__((no_instrument_function)) int sigisemptyset(sigset_t *set)
{
 switch ((64 / 64)) {
 case 4:
  return (set->sig[3] | set->sig[2] |
   set->sig[1] | set->sig[0]) == 0;
 case 2:
  return (set->sig[1] | set->sig[0]) == 0;
 case 1:
  return set->sig[0] == 0;
 default:
  do { bool __cond = !(!(1)); extern void __compiletime_assert_80(void) __attribute__((error("BUILD_BUG failed"))); if (__cond) __compiletime_assert_80(); do { } while (0); } while (0);
  return 0;
 }
}
# 114 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/signal.h"
static inline __attribute__((no_instrument_function)) void sigorsets(sigset_t *r, const sigset_t *a, const sigset_t *b) { unsigned long a0, a1, a2, a3, b0, b1, b2, b3; switch ((64 / 64)) { case 4: a3 = a->sig[3]; a2 = a->sig[2]; b3 = b->sig[3]; b2 = b->sig[2]; r->sig[3] = ((a3) | (b3)); r->sig[2] = ((a2) | (b2)); case 2: a1 = a->sig[1]; b1 = b->sig[1]; r->sig[1] = ((a1) | (b1)); case 1: a0 = a->sig[0]; b0 = b->sig[0]; r->sig[0] = ((a0) | (b0)); break; default: do { bool __cond = !(!(1)); extern void __compiletime_assert_114(void) __attribute__((error("BUILD_BUG failed"))); if (__cond) __compiletime_assert_114(); do { } while (0); } while (0); } }


static inline __attribute__((no_instrument_function)) void sigandsets(sigset_t *r, const sigset_t *a, const sigset_t *b) { unsigned long a0, a1, a2, a3, b0, b1, b2, b3; switch ((64 / 64)) { case 4: a3 = a->sig[3]; a2 = a->sig[2]; b3 = b->sig[3]; b2 = b->sig[2]; r->sig[3] = ((a3) & (b3)); r->sig[2] = ((a2) & (b2)); case 2: a1 = a->sig[1]; b1 = b->sig[1]; r->sig[1] = ((a1) & (b1)); case 1: a0 = a->sig[0]; b0 = b->sig[0]; r->sig[0] = ((a0) & (b0)); break; default: do { bool __cond = !(!(1)); extern void __compiletime_assert_117(void) __attribute__((error("BUILD_BUG failed"))); if (__cond) __compiletime_assert_117(); do { } while (0); } while (0); } }


static inline __attribute__((no_instrument_function)) void sigandnsets(sigset_t *r, const sigset_t *a, const sigset_t *b) { unsigned long a0, a1, a2, a3, b0, b1, b2, b3; switch ((64 / 64)) { case 4: a3 = a->sig[3]; a2 = a->sig[2]; b3 = b->sig[3]; b2 = b->sig[2]; r->sig[3] = ((a3) & ~(b3)); r->sig[2] = ((a2) & ~(b2)); case 2: a1 = a->sig[1]; b1 = b->sig[1]; r->sig[1] = ((a1) & ~(b1)); case 1: a0 = a->sig[0]; b0 = b->sig[0]; r->sig[0] = ((a0) & ~(b0)); break; default: do { bool __cond = !(!(1)); extern void __compiletime_assert_120(void) __attribute__((error("BUILD_BUG failed"))); if (__cond) __compiletime_assert_120(); do { } while (0); } while (0); } }
# 142 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/signal.h"
static inline __attribute__((no_instrument_function)) void signotset(sigset_t *set) { switch ((64 / 64)) { case 4: set->sig[3] = (~(set->sig[3])); set->sig[2] = (~(set->sig[2])); case 2: set->sig[1] = (~(set->sig[1])); case 1: set->sig[0] = (~(set->sig[0])); break; default: do { bool __cond = !(!(1)); extern void __compiletime_assert_142(void) __attribute__((error("BUILD_BUG failed"))); if (__cond) __compiletime_assert_142(); do { } while (0); } while (0); } }




static inline __attribute__((no_instrument_function)) void sigemptyset(sigset_t *set)
{
 switch ((64 / 64)) {
 default:
  memset(set, 0, sizeof(sigset_t));
  break;
 case 2: set->sig[1] = 0;
 case 1: set->sig[0] = 0;
  break;
 }
}

static inline __attribute__((no_instrument_function)) void sigfillset(sigset_t *set)
{
 switch ((64 / 64)) {
 default:
  memset(set, -1, sizeof(sigset_t));
  break;
 case 2: set->sig[1] = -1;
 case 1: set->sig[0] = -1;
  break;
 }
}



static inline __attribute__((no_instrument_function)) void sigaddsetmask(sigset_t *set, unsigned long mask)
{
 set->sig[0] |= mask;
}

static inline __attribute__((no_instrument_function)) void sigdelsetmask(sigset_t *set, unsigned long mask)
{
 set->sig[0] &= ~mask;
}

static inline __attribute__((no_instrument_function)) int sigtestsetmask(sigset_t *set, unsigned long mask)
{
 return (set->sig[0] & mask) != 0;
}

static inline __attribute__((no_instrument_function)) void siginitset(sigset_t *set, unsigned long mask)
{
 set->sig[0] = mask;
 switch ((64 / 64)) {
 default:
  memset(&set->sig[1], 0, sizeof(long)*((64 / 64)-1));
  break;
 case 2: set->sig[1] = 0;
 case 1: ;
 }
}

static inline __attribute__((no_instrument_function)) void siginitsetinv(sigset_t *set, unsigned long mask)
{
 set->sig[0] = ~mask;
 switch ((64 / 64)) {
 default:
  memset(&set->sig[1], -1, sizeof(long)*((64 / 64)-1));
  break;
 case 2: set->sig[1] = -1;
 case 1: ;
 }
}



static inline __attribute__((no_instrument_function)) void init_sigpending(struct sigpending *sig)
{
 sigemptyset(&sig->signal);
 INIT_LIST_HEAD(&sig->list);
}

extern void flush_sigqueue(struct sigpending *queue);


static inline __attribute__((no_instrument_function)) int valid_signal(unsigned long sig)
{
 return sig <= 64 ? 1 : 0;
}

struct timespec;
struct pt_regs;

extern int next_signal(struct sigpending *pending, sigset_t *mask);
extern int do_send_sig_info(int sig, struct siginfo *info,
    struct task_struct *p, bool group);
extern int group_send_sig_info(int sig, struct siginfo *info, struct task_struct *p);
extern int __group_send_sig_info(int, struct siginfo *, struct task_struct *);
extern int do_sigtimedwait(const sigset_t *, siginfo_t *,
    const struct timespec *);
extern int sigprocmask(int, sigset_t *, sigset_t *);
extern void set_current_blocked(sigset_t *);
extern void __set_current_blocked(const sigset_t *);
extern int show_unhandled_signals;

struct sigaction {

 __sighandler_t sa_handler;
 unsigned long sa_flags;





 __sigrestore_t sa_restorer;

 sigset_t sa_mask;
};

struct k_sigaction {
 struct sigaction sa;



};
# 273 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/signal.h"
struct ksignal {
 struct k_sigaction ka;
 siginfo_t info;
 int sig;
};

extern int get_signal(struct ksignal *ksig);
extern void signal_setup_done(int failed, struct ksignal *ksig, int stepping);
extern void exit_signals(struct task_struct *tsk);
extern void kernel_sigaction(int, __sighandler_t);

static inline __attribute__((no_instrument_function)) void allow_signal(int sig)
{





 kernel_sigaction(sig, ( __sighandler_t)2);
}

static inline __attribute__((no_instrument_function)) void disallow_signal(int sig)
{
 kernel_sigaction(sig, (( __sighandler_t)1));
}

extern struct kmem_cache *sighand_cachep;

int unhandled_signal(struct task_struct *tsk, int sig);
# 426 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/signal.h"
void signals_init(void);

int restore_altstack(const stack_t *);
int __save_altstack(stack_t *, unsigned long);
# 440 "/usr/src/linux-source-3.19.0/linux-source-3.19.0/include/linux/signal.h"
struct seq_file;
extern void render_sigset_t(struct seq_file *, const char *, sigset_t *);
# 16 "source/arch/x86/include/asm/syscalls.h" 2




 long sys_ioperm(unsigned long, unsigned long, int);
 long sys_iopl(unsigned int);


 int sys_modify_ldt(int, void *, unsigned long);


 long sys_rt_sigreturn(void);


 long sys_set_thread_area(struct user_desc *);
 long sys_get_thread_area(struct user_desc *);
# 47 "source/arch/x86/include/asm/syscalls.h"
 long sys_arch_prctl(int, unsigned long);


 long sys_mmap(unsigned long, unsigned long, unsigned long,
    unsigned long, unsigned long, unsigned long);
