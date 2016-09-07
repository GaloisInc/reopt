-- DO NOT EDIT.  Generated from make_bsd_syscalls/Main.hs
module Reopt.Machine.SysDeps.FreeBSDGenerated where
import           Data.Macaw.Architecture.Syscall
import           Data.Map (Map, fromList)
import           Data.Word

syscallInfo :: Map Word64 SyscallTypeInfo
syscallInfo = fromList
                [ ( 0 , ( "nosys" , WordArgType , [] ) )
                , ( 1 , ( "sys_exit" , VoidArgType , [ WordArgType ] ) )
                , ( 2 , ( "fork" , WordArgType , [] ) )
                , ( 3
                  , ( "read"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 4
                  , ( "write"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 5
                  , ( "open"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 6 , ( "close" , WordArgType , [ WordArgType ] ) )
                , ( 7
                  , ( "wait4"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 8 , ( "creat" , WordArgType , [ WordArgType , WordArgType ] ) )
                , ( 9 , ( "link" , WordArgType , [ WordArgType , WordArgType ] ) )
                , ( 10 , ( "unlink" , WordArgType , [ WordArgType ] ) )
                , ( 12 , ( "chdir" , WordArgType , [ WordArgType ] ) )
                , ( 13 , ( "fchdir" , WordArgType , [ WordArgType ] ) )
                , ( 14
                  , ( "mknod"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 15
                  , ( "chmod" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 16
                  , ( "chown"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 17 , ( "obreak" , WordArgType , [ WordArgType ] ) )
                , ( 18
                  , ( "getfsstat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 19
                  , ( "lseek"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 20 , ( "getpid" , WordArgType , [] ) )
                , ( 21
                  , ( "mount"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 22
                  , ( "unmount" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 23 , ( "setuid" , WordArgType , [ WordArgType ] ) )
                , ( 24 , ( "getuid" , WordArgType , [] ) )
                , ( 25 , ( "geteuid" , WordArgType , [] ) )
                , ( 26
                  , ( "ptrace"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 27
                  , ( "recvmsg"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 28
                  , ( "sendmsg"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 29
                  , ( "recvfrom"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 30
                  , ( "accept"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 31
                  , ( "getpeername"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 32
                  , ( "getsockname"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 33
                  , ( "access" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 34
                  , ( "chflags" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 35
                  , ( "fchflags" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 36 , ( "sync" , WordArgType , [] ) )
                , ( 37 , ( "kill" , WordArgType , [ WordArgType , WordArgType ] ) )
                , ( 38 , ( "stat" , WordArgType , [ WordArgType , WordArgType ] ) )
                , ( 39 , ( "getppid" , WordArgType , [] ) )
                , ( 40
                  , ( "lstat" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 41 , ( "dup" , WordArgType , [ WordArgType ] ) )
                , ( 42 , ( "pipe" , WordArgType , [] ) )
                , ( 43 , ( "getegid" , WordArgType , [] ) )
                , ( 44
                  , ( "profil"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 45
                  , ( "ktrace"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 46
                  , ( "sigaction"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 47 , ( "getgid" , WordArgType , [] ) )
                , ( 49
                  , ( "getlogin" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 50 , ( "setlogin" , WordArgType , [ WordArgType ] ) )
                , ( 51 , ( "acct" , WordArgType , [ WordArgType ] ) )
                , ( 52 , ( "sigpending" , WordArgType , [] ) )
                , ( 53
                  , ( "sigaltstack" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 54
                  , ( "ioctl"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 55 , ( "reboot" , WordArgType , [ WordArgType ] ) )
                , ( 56 , ( "revoke" , WordArgType , [ WordArgType ] ) )
                , ( 57
                  , ( "symlink" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 58
                  , ( "readlink"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 59
                  , ( "execve"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 60 , ( "umask" , WordArgType , [ WordArgType ] ) )
                , ( 61 , ( "chroot" , WordArgType , [ WordArgType ] ) )
                , ( 62
                  , ( "fstat" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 63
                  , ( "getkerninfo"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 64 , ( "getpagesize" , WordArgType , [] ) )
                , ( 65
                  , ( "msync"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 66 , ( "vfork" , WordArgType , [] ) )
                , ( 69 , ( "sbrk" , WordArgType , [ WordArgType ] ) )
                , ( 70 , ( "sstk" , WordArgType , [ WordArgType ] ) )
                , ( 71
                  , ( "mmap"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 72 , ( "ovadvise" , WordArgType , [ WordArgType ] ) )
                , ( 73
                  , ( "munmap" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 74
                  , ( "mprotect"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 75
                  , ( "madvise"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 78
                  , ( "mincore"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 79
                  , ( "getgroups" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 80
                  , ( "setgroups" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 81 , ( "getpgrp" , WordArgType , [] ) )
                , ( 82
                  , ( "setpgid" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 83
                  , ( "setitimer"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 84 , ( "wait" , WordArgType , [] ) )
                , ( 85 , ( "swapon" , WordArgType , [ WordArgType ] ) )
                , ( 86
                  , ( "getitimer" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 87
                  , ( "gethostname" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 88
                  , ( "sethostname" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 89 , ( "getdtablesize" , WordArgType , [] ) )
                , ( 90 , ( "dup2" , WordArgType , [ WordArgType , WordArgType ] ) )
                , ( 92
                  , ( "fcntl"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 93
                  , ( "select"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 95 , ( "fsync" , WordArgType , [ WordArgType ] ) )
                , ( 96
                  , ( "setpriority"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 97
                  , ( "socket"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 98
                  , ( "connect"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 99
                  , ( "accept"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 100
                  , ( "getpriority" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 101
                  , ( "send"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 102
                  , ( "recv"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 103 , ( "sigreturn" , WordArgType , [ WordArgType ] ) )
                , ( 104
                  , ( "bind"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 105
                  , ( "setsockopt"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 106
                  , ( "listen" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 108
                  , ( "sigvec"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 109 , ( "sigblock" , WordArgType , [ WordArgType ] ) )
                , ( 110 , ( "sigsetmask" , WordArgType , [ WordArgType ] ) )
                , ( 112
                  , ( "sigstack" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 113
                  , ( "recvmsg"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 114
                  , ( "sendmsg"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 116
                  , ( "gettimeofday" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 117
                  , ( "getrusage" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 118
                  , ( "getsockopt"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 120
                  , ( "readv"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 121
                  , ( "writev"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 122
                  , ( "settimeofday" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 123
                  , ( "fchown"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 124
                  , ( "fchmod" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 125
                  , ( "recvfrom"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 126
                  , ( "setreuid" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 127
                  , ( "setregid" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 128
                  , ( "rename" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 129
                  , ( "truncate" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 130
                  , ( "ftruncate" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 131
                  , ( "flock" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 132
                  , ( "mkfifo" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 133
                  , ( "sendto"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 134
                  , ( "shutdown" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 135
                  , ( "socketpair"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 136
                  , ( "mkdir" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 137 , ( "rmdir" , WordArgType , [ WordArgType ] ) )
                , ( 138
                  , ( "utimes" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 140
                  , ( "adjtime" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 141
                  , ( "getpeername"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 142 , ( "gethostid" , WordArgType , [] ) )
                , ( 143 , ( "sethostid" , WordArgType , [ WordArgType ] ) )
                , ( 144
                  , ( "getrlimit" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 145
                  , ( "setrlimit" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 146
                  , ( "killpg" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 147 , ( "setsid" , WordArgType , [] ) )
                , ( 148
                  , ( "quotactl"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 149 , ( "quota" , WordArgType , [] ) )
                , ( 150
                  , ( "getsockname"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 154
                  , ( "nlm_syscall"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 155
                  , ( "nfssvc" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 156
                  , ( "getdirentries"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 157
                  , ( "statfs" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 158
                  , ( "fstatfs" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 160
                  , ( "lgetfh" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 161
                  , ( "getfh" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 162
                  , ( "getdomainname" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 163
                  , ( "setdomainname" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 164 , ( "uname" , WordArgType , [ WordArgType ] ) )
                , ( 165
                  , ( "sysarch" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 166
                  , ( "rtprio"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 169
                  , ( "semsys"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 170
                  , ( "msgsys"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 171
                  , ( "shmsys"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 173
                  , ( "freebsd6_pread"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 174
                  , ( "freebsd6_pwrite"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 175 , ( "setfib" , WordArgType , [ WordArgType ] ) )
                , ( 176 , ( "ntp_adjtime" , WordArgType , [ WordArgType ] ) )
                , ( 181 , ( "setgid" , WordArgType , [ WordArgType ] ) )
                , ( 182 , ( "setegid" , WordArgType , [ WordArgType ] ) )
                , ( 183 , ( "seteuid" , WordArgType , [ WordArgType ] ) )
                , ( 188
                  , ( "stat" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 189
                  , ( "fstat" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 190
                  , ( "lstat" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 191
                  , ( "pathconf" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 192
                  , ( "fpathconf" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 194
                  , ( "getrlimit" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 195
                  , ( "setrlimit" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 196
                  , ( "getdirentries"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 197
                  , ( "freebsd6_mmap"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 198 , ( "nosys" , WordArgType , [] ) )
                , ( 199
                  , ( "freebsd6_lseek"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 200
                  , ( "freebsd6_truncate"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 201
                  , ( "freebsd6_ftruncate"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 202
                  , ( "__sysctl"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 203
                  , ( "mlock" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 204
                  , ( "munlock" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 205 , ( "undelete" , WordArgType , [ WordArgType ] ) )
                , ( 206
                  , ( "futimes" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 207 , ( "getpgid" , WordArgType , [ WordArgType ] ) )
                , ( 209
                  , ( "poll"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 220
                  , ( "__semctl"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 221
                  , ( "semget"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 222
                  , ( "semop"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 224
                  , ( "msgctl"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 225
                  , ( "msgget" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 226
                  , ( "msgsnd"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 227
                  , ( "msgrcv"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 228
                  , ( "shmat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 229
                  , ( "shmctl"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 230 , ( "shmdt" , WordArgType , [ WordArgType ] ) )
                , ( 231
                  , ( "shmget"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 232
                  , ( "clock_gettime" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 233
                  , ( "clock_settime" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 234
                  , ( "clock_getres" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 235
                  , ( "ktimer_create"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 236 , ( "ktimer_delete" , WordArgType , [ WordArgType ] ) )
                , ( 237
                  , ( "ktimer_settime"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 238
                  , ( "ktimer_gettime"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 239 , ( "ktimer_getoverrun" , WordArgType , [ WordArgType ] ) )
                , ( 240
                  , ( "nanosleep" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 241
                  , ( "ffclock_getcounter" , WordArgType , [ WordArgType ] )
                  )
                , ( 242
                  , ( "ffclock_setestimate" , WordArgType , [ WordArgType ] )
                  )
                , ( 243
                  , ( "ffclock_getestimate" , WordArgType , [ WordArgType ] )
                  )
                , ( 247
                  , ( "clock_getcpuclockid2"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 248 , ( "ntp_gettime" , WordArgType , [ WordArgType ] ) )
                , ( 250
                  , ( "minherit"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 251 , ( "rfork" , WordArgType , [ WordArgType ] ) )
                , ( 252
                  , ( "openbsd_poll"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 253 , ( "issetugid" , WordArgType , [] ) )
                , ( 254
                  , ( "lchown"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 255 , ( "aio_read" , WordArgType , [ WordArgType ] ) )
                , ( 256 , ( "aio_write" , WordArgType , [ WordArgType ] ) )
                , ( 257
                  , ( "lio_listio"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 272
                  , ( "getdents"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 274
                  , ( "lchmod" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 275
                  , ( "lchown"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 276
                  , ( "lutimes" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 277
                  , ( "msync"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 278
                  , ( "nstat" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 279
                  , ( "nfstat" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 280
                  , ( "nlstat" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 289
                  , ( "preadv"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 290
                  , ( "pwritev"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 297
                  , ( "fhstatfs" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 298
                  , ( "fhopen" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 299
                  , ( "fhstat" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 300 , ( "modnext" , WordArgType , [ WordArgType ] ) )
                , ( 301
                  , ( "modstat" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 302 , ( "modfnext" , WordArgType , [ WordArgType ] ) )
                , ( 303 , ( "modfind" , WordArgType , [ WordArgType ] ) )
                , ( 304 , ( "kldload" , WordArgType , [ WordArgType ] ) )
                , ( 305 , ( "kldunload" , WordArgType , [ WordArgType ] ) )
                , ( 306 , ( "kldfind" , WordArgType , [ WordArgType ] ) )
                , ( 307 , ( "kldnext" , WordArgType , [ WordArgType ] ) )
                , ( 308
                  , ( "kldstat" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 309 , ( "kldfirstmod" , WordArgType , [ WordArgType ] ) )
                , ( 310 , ( "getsid" , WordArgType , [ WordArgType ] ) )
                , ( 311
                  , ( "setresuid"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 312
                  , ( "setresgid"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 314 , ( "aio_return" , WordArgType , [ WordArgType ] ) )
                , ( 315
                  , ( "aio_suspend"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 316
                  , ( "aio_cancel" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 317 , ( "aio_error" , WordArgType , [ WordArgType ] ) )
                , ( 318 , ( "oaio_read" , WordArgType , [ WordArgType ] ) )
                , ( 319 , ( "oaio_write" , WordArgType , [ WordArgType ] ) )
                , ( 320
                  , ( "olio_listio"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 321 , ( "yield" , WordArgType , [] ) )
                , ( 324 , ( "mlockall" , WordArgType , [ WordArgType ] ) )
                , ( 325 , ( "munlockall" , WordArgType , [] ) )
                , ( 326
                  , ( "__getcwd" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 327
                  , ( "sched_setparam"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 328
                  , ( "sched_getparam"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 329
                  , ( "sched_setscheduler"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 330
                  , ( "sched_getscheduler" , WordArgType , [ WordArgType ] )
                  )
                , ( 331 , ( "sched_yield" , WordArgType , [] ) )
                , ( 332
                  , ( "sched_get_priority_max" , WordArgType , [ WordArgType ] )
                  )
                , ( 333
                  , ( "sched_get_priority_min" , WordArgType , [ WordArgType ] )
                  )
                , ( 334
                  , ( "sched_rr_get_interval"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 335
                  , ( "utrace" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 336
                  , ( "sendfile"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 337
                  , ( "kldsym"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 338 , ( "jail" , WordArgType , [ WordArgType ] ) )
                , ( 339
                  , ( "nnpfs_syscall"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 340
                  , ( "sigprocmask"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 341 , ( "sigsuspend" , WordArgType , [ WordArgType ] ) )
                , ( 342
                  , ( "sigaction"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 343 , ( "sigpending" , WordArgType , [ WordArgType ] ) )
                , ( 344 , ( "sigreturn" , WordArgType , [ WordArgType ] ) )
                , ( 345
                  , ( "sigtimedwait"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 346
                  , ( "sigwaitinfo" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 347
                  , ( "__acl_get_file"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 348
                  , ( "__acl_set_file"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 349
                  , ( "__acl_get_fd"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 350
                  , ( "__acl_set_fd"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 351
                  , ( "__acl_delete_file"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 352
                  , ( "__acl_delete_fd"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 353
                  , ( "__acl_aclcheck_file"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 354
                  , ( "__acl_aclcheck_fd"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 355
                  , ( "extattrctl"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 356
                  , ( "extattr_set_file"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 357
                  , ( "extattr_get_file"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 358
                  , ( "extattr_delete_file"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 359
                  , ( "aio_waitcomplete"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 360
                  , ( "getresuid"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 361
                  , ( "getresgid"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 362 , ( "kqueue" , WordArgType , [] ) )
                , ( 363
                  , ( "kevent"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 371
                  , ( "extattr_set_fd"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 372
                  , ( "extattr_get_fd"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 373
                  , ( "extattr_delete_fd"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 374 , ( "__setugid" , WordArgType , [ WordArgType ] ) )
                , ( 376
                  , ( "eaccess" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 377
                  , ( "afs3_syscall"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 378
                  , ( "nmount"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 384 , ( "__mac_get_proc" , WordArgType , [ WordArgType ] ) )
                , ( 385 , ( "__mac_set_proc" , WordArgType , [ WordArgType ] ) )
                , ( 386
                  , ( "__mac_get_fd" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 387
                  , ( "__mac_get_file"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 388
                  , ( "__mac_set_fd" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 389
                  , ( "__mac_set_file"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 390
                  , ( "kenv"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 391
                  , ( "lchflags" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 392
                  , ( "uuidgen" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 393
                  , ( "sendfile"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 394
                  , ( "mac_syscall"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 395
                  , ( "getfsstat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 396
                  , ( "statfs" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 397
                  , ( "fstatfs" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 398
                  , ( "fhstatfs" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 400 , ( "ksem_close" , WordArgType , [ WordArgType ] ) )
                , ( 401 , ( "ksem_post" , WordArgType , [ WordArgType ] ) )
                , ( 402 , ( "ksem_wait" , WordArgType , [ WordArgType ] ) )
                , ( 403 , ( "ksem_trywait" , WordArgType , [ WordArgType ] ) )
                , ( 404
                  , ( "ksem_init" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 405
                  , ( "ksem_open"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 406 , ( "ksem_unlink" , WordArgType , [ WordArgType ] ) )
                , ( 407
                  , ( "ksem_getvalue" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 408 , ( "ksem_destroy" , WordArgType , [ WordArgType ] ) )
                , ( 409
                  , ( "__mac_get_pid" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 410
                  , ( "__mac_get_link"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 411
                  , ( "__mac_set_link"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 412
                  , ( "extattr_set_link"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 413
                  , ( "extattr_get_link"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 414
                  , ( "extattr_delete_link"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 415
                  , ( "__mac_execve"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 416
                  , ( "sigaction"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 417 , ( "sigreturn" , WordArgType , [ WordArgType ] ) )
                , ( 421 , ( "getcontext" , WordArgType , [ WordArgType ] ) )
                , ( 422 , ( "setcontext" , WordArgType , [ WordArgType ] ) )
                , ( 423
                  , ( "swapcontext" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 424 , ( "swapoff" , WordArgType , [ WordArgType ] ) )
                , ( 425
                  , ( "__acl_get_link"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 426
                  , ( "__acl_set_link"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 427
                  , ( "__acl_delete_link"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 428
                  , ( "__acl_aclcheck_link"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 429
                  , ( "sigwait" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 430
                  , ( "thr_create"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 431 , ( "thr_exit" , VoidArgType , [ WordArgType ] ) )
                , ( 432 , ( "thr_self" , WordArgType , [ WordArgType ] ) )
                , ( 433
                  , ( "thr_kill" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 434 , ( "_umtx_lock" , WordArgType , [ WordArgType ] ) )
                , ( 435 , ( "_umtx_unlock" , WordArgType , [ WordArgType ] ) )
                , ( 436 , ( "jail_attach" , WordArgType , [ WordArgType ] ) )
                , ( 437
                  , ( "extattr_list_fd"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 438
                  , ( "extattr_list_file"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 439
                  , ( "extattr_list_link"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 441
                  , ( "ksem_timedwait"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 442 , ( "thr_suspend" , WordArgType , [ WordArgType ] ) )
                , ( 443 , ( "thr_wake" , WordArgType , [ WordArgType ] ) )
                , ( 444
                  , ( "kldunloadf" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 445
                  , ( "audit" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 446
                  , ( "auditon"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 447 , ( "getauid" , WordArgType , [ WordArgType ] ) )
                , ( 448 , ( "setauid" , WordArgType , [ WordArgType ] ) )
                , ( 449 , ( "getaudit" , WordArgType , [ WordArgType ] ) )
                , ( 450 , ( "setaudit" , WordArgType , [ WordArgType ] ) )
                , ( 451
                  , ( "getaudit_addr" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 452
                  , ( "setaudit_addr" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 453 , ( "auditctl" , WordArgType , [ WordArgType ] ) )
                , ( 454
                  , ( "_umtx_op"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 455
                  , ( "thr_new" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 456
                  , ( "sigqueue"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 457
                  , ( "kmq_open"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 458
                  , ( "kmq_setattr"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 459
                  , ( "kmq_timedreceive"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 460
                  , ( "kmq_timedsend"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 461
                  , ( "kmq_notify" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 462 , ( "kmq_unlink" , WordArgType , [ WordArgType ] ) )
                , ( 463
                  , ( "abort2"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 464
                  , ( "thr_set_name" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 465
                  , ( "aio_fsync" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 466
                  , ( "rtprio_thread"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 471
                  , ( "sctp_peeloff" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 472
                  , ( "sctp_generic_sendmsg"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 473
                  , ( "sctp_generic_sendmsg_iov"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 474
                  , ( "sctp_generic_recvmsg"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 475
                  , ( "pread"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 476
                  , ( "pwrite"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 477
                  , ( "mmap"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 478
                  , ( "lseek"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 479
                  , ( "truncate" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 480
                  , ( "ftruncate" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 481
                  , ( "thr_kill2"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 482
                  , ( "shm_open"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 483 , ( "shm_unlink" , WordArgType , [ WordArgType ] ) )
                , ( 484 , ( "cpuset" , WordArgType , [ WordArgType ] ) )
                , ( 485
                  , ( "cpuset_setid"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 486
                  , ( "cpuset_getid"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 487
                  , ( "cpuset_getaffinity"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 488
                  , ( "cpuset_setaffinity"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 489
                  , ( "faccessat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 490
                  , ( "fchmodat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 491
                  , ( "fchownat"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 492
                  , ( "fexecve"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 493
                  , ( "fstatat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 494
                  , ( "futimesat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 495
                  , ( "linkat"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 496
                  , ( "mkdirat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 497
                  , ( "mkfifoat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 498
                  , ( "mknodat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 499
                  , ( "openat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 500
                  , ( "readlinkat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 501
                  , ( "renameat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 502
                  , ( "symlinkat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 503
                  , ( "unlinkat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 504 , ( "posix_openpt" , WordArgType , [ WordArgType ] ) )
                , ( 505 , ( "gssd_syscall" , WordArgType , [ WordArgType ] ) )
                , ( 506
                  , ( "jail_get"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 507
                  , ( "jail_set"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 508 , ( "jail_remove" , WordArgType , [ WordArgType ] ) )
                , ( 509 , ( "closefrom" , WordArgType , [ WordArgType ] ) )
                , ( 510
                  , ( "__semctl"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 511
                  , ( "msgctl"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 512
                  , ( "shmctl"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 513
                  , ( "lpathconf" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 515
                  , ( "__cap_rights_get"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 516 , ( "cap_enter" , WordArgType , [] ) )
                , ( 517 , ( "cap_getmode" , WordArgType , [ WordArgType ] ) )
                , ( 518
                  , ( "pdfork" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 519
                  , ( "pdkill" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 520
                  , ( "pdgetpid" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 522
                  , ( "pselect"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 523
                  , ( "getloginclass" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 524 , ( "setloginclass" , WordArgType , [ WordArgType ] ) )
                , ( 525
                  , ( "rctl_get_racct"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 526
                  , ( "rctl_get_rules"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 527
                  , ( "rctl_get_limits"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 528
                  , ( "rctl_add_rule"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 529
                  , ( "rctl_remove_rule"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 530
                  , ( "posix_fallocate"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 531
                  , ( "posix_fadvise"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 532
                  , ( "wait6"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 533
                  , ( "cap_rights_limit"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 534
                  , ( "cap_ioctls_limit"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 535
                  , ( "cap_ioctls_get"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 536
                  , ( "cap_fcntls_limit"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 537
                  , ( "cap_fcntls_get"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 538
                  , ( "bindat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 539
                  , ( "connectat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 540
                  , ( "chflagsat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 541
                  , ( "accept4"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 542
                  , ( "pipe2" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 543 , ( "aio_mlock" , WordArgType , [ WordArgType ] ) )
                , ( 544
                  , ( "procctl"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                ]
