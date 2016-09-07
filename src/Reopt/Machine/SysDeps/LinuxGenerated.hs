-- DO NOT EDIT.  Generated from make_linux_syscalls/Main.hs
module Reopt.Machine.SysDeps.LinuxGenerated where
import           Data.Macaw.Architecture.Syscall
import           Data.Map (Map, fromList)
import           Data.Word

syscallInfo :: Map Word64 SyscallTypeInfo
syscallInfo = fromList
                [ ( 0
                  , ( "sys_read"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 1
                  , ( "sys_write"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 2
                  , ( "sys_open"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 3 , ( "sys_close" , WordArgType , [ WordArgType ] ) )
                , ( 4
                  , ( "sys_newstat" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 5
                  , ( "sys_newfstat" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 6
                  , ( "sys_newlstat" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 7
                  , ( "sys_poll"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 8
                  , ( "sys_lseek"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 9
                  , ( "sys_mmap"
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
                , ( 10
                  , ( "sys_mprotect"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 11
                  , ( "sys_munmap" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 12 , ( "sys_brk" , WordArgType , [ WordArgType ] ) )
                , ( 13
                  , ( "sys_rt_sigaction"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 14
                  , ( "sys_rt_sigprocmask"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 15 , ( "sys_rt_sigreturn" , WordArgType , [] ) )
                , ( 16
                  , ( "sys_ioctl"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 17
                  , ( "sys_pread64"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 18
                  , ( "sys_pwrite64"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 19
                  , ( "sys_readv"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 20
                  , ( "sys_writev"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 21
                  , ( "sys_access" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 22 , ( "sys_pipe" , WordArgType , [ WordArgType ] ) )
                , ( 23
                  , ( "sys_select"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 24 , ( "sys_sched_yield" , WordArgType , [] ) )
                , ( 25
                  , ( "sys_mremap"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 26
                  , ( "sys_msync"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 27
                  , ( "sys_mincore"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 28
                  , ( "sys_madvise"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 29
                  , ( "sys_shmget"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 30
                  , ( "sys_shmat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 31
                  , ( "sys_shmctl"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 32 , ( "sys_dup" , WordArgType , [ WordArgType ] ) )
                , ( 33
                  , ( "sys_dup2" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 34 , ( "sys_pause" , WordArgType , [] ) )
                , ( 35
                  , ( "sys_nanosleep" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 36
                  , ( "sys_getitimer" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 37 , ( "sys_alarm" , WordArgType , [ WordArgType ] ) )
                , ( 38
                  , ( "sys_setitimer"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 39 , ( "sys_getpid" , WordArgType , [] ) )
                , ( 40
                  , ( "sys_sendfile64"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 41
                  , ( "sys_socket"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 42
                  , ( "sys_connect"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 43
                  , ( "sys_accept"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 44
                  , ( "sys_sendto"
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
                , ( 45
                  , ( "sys_recvfrom"
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
                , ( 46
                  , ( "sys_sendmsg"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 47
                  , ( "sys_recvmsg"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 48
                  , ( "sys_shutdown" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 49
                  , ( "sys_bind"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 50
                  , ( "sys_listen" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 51
                  , ( "sys_getsockname"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 52
                  , ( "sys_getpeername"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 53
                  , ( "sys_socketpair"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 54
                  , ( "sys_setsockopt"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 55
                  , ( "sys_getsockopt"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 56
                  , ( "sys_clone"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 57 , ( "sys_fork" , WordArgType , [] ) )
                , ( 58 , ( "sys_vfork" , WordArgType , [] ) )
                , ( 59
                  , ( "sys_execve"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 60 , ( "sys_exit" , WordArgType , [ WordArgType ] ) )
                , ( 61
                  , ( "sys_wait4"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 62
                  , ( "sys_kill" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 63 , ( "sys_newuname" , WordArgType , [ WordArgType ] ) )
                , ( 64
                  , ( "sys_semget"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 65
                  , ( "sys_semop"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 66
                  , ( "sys_semctl"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 67 , ( "sys_shmdt" , WordArgType , [ WordArgType ] ) )
                , ( 68
                  , ( "sys_msgget" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 69
                  , ( "sys_msgsnd"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 70
                  , ( "sys_msgrcv"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 71
                  , ( "sys_msgctl"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 72
                  , ( "sys_fcntl"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 73
                  , ( "sys_flock" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 74 , ( "sys_fsync" , WordArgType , [ WordArgType ] ) )
                , ( 75 , ( "sys_fdatasync" , WordArgType , [ WordArgType ] ) )
                , ( 76
                  , ( "sys_truncate" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 77
                  , ( "sys_ftruncate" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 78
                  , ( "sys_getdents"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 79
                  , ( "sys_getcwd" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 80 , ( "sys_chdir" , WordArgType , [ WordArgType ] ) )
                , ( 81 , ( "sys_fchdir" , WordArgType , [ WordArgType ] ) )
                , ( 82
                  , ( "sys_rename" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 83
                  , ( "sys_mkdir" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 84 , ( "sys_rmdir" , WordArgType , [ WordArgType ] ) )
                , ( 85
                  , ( "sys_creat" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 86
                  , ( "sys_link" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 87 , ( "sys_unlink" , WordArgType , [ WordArgType ] ) )
                , ( 88
                  , ( "sys_symlink" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 89
                  , ( "sys_readlink"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 90
                  , ( "sys_chmod" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 91
                  , ( "sys_fchmod" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 92
                  , ( "sys_chown"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 93
                  , ( "sys_fchown"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 94
                  , ( "sys_lchown"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 95 , ( "sys_umask" , WordArgType , [ WordArgType ] ) )
                , ( 96
                  , ( "sys_gettimeofday"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 97
                  , ( "sys_getrlimit" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 98
                  , ( "sys_getrusage" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 99 , ( "sys_sysinfo" , WordArgType , [ WordArgType ] ) )
                , ( 100 , ( "sys_times" , WordArgType , [ WordArgType ] ) )
                , ( 101
                  , ( "sys_ptrace"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 102 , ( "sys_getuid" , WordArgType , [] ) )
                , ( 103
                  , ( "sys_syslog"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 104 , ( "sys_getgid" , WordArgType , [] ) )
                , ( 105 , ( "sys_setuid" , WordArgType , [ WordArgType ] ) )
                , ( 106 , ( "sys_setgid" , WordArgType , [ WordArgType ] ) )
                , ( 107 , ( "sys_geteuid" , WordArgType , [] ) )
                , ( 108 , ( "sys_getegid" , WordArgType , [] ) )
                , ( 109
                  , ( "sys_setpgid" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 110 , ( "sys_getppid" , WordArgType , [] ) )
                , ( 111 , ( "sys_getpgrp" , WordArgType , [] ) )
                , ( 112 , ( "sys_setsid" , WordArgType , [] ) )
                , ( 113
                  , ( "sys_setreuid" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 114
                  , ( "sys_setregid" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 115
                  , ( "sys_getgroups" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 116
                  , ( "sys_setgroups" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 117
                  , ( "sys_setresuid"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 118
                  , ( "sys_getresuid"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 119
                  , ( "sys_setresgid"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 120
                  , ( "sys_getresgid"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 121 , ( "sys_getpgid" , WordArgType , [ WordArgType ] ) )
                , ( 122 , ( "sys_setfsuid" , WordArgType , [ WordArgType ] ) )
                , ( 123 , ( "sys_setfsgid" , WordArgType , [ WordArgType ] ) )
                , ( 124 , ( "sys_getsid" , WordArgType , [ WordArgType ] ) )
                , ( 125
                  , ( "sys_capget" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 126
                  , ( "sys_capset" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 127
                  , ( "sys_rt_sigpending"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 128
                  , ( "sys_rt_sigtimedwait"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 129
                  , ( "sys_rt_sigqueueinfo"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 130
                  , ( "sys_rt_sigsuspend"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 131
                  , ( "sys_sigaltstack"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 132
                  , ( "sys_utime" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 133
                  , ( "sys_mknod"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 135 , ( "sys_personality" , WordArgType , [ WordArgType ] ) )
                , ( 136
                  , ( "sys_ustat" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 137
                  , ( "sys_statfs" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 138
                  , ( "sys_fstatfs" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 139
                  , ( "sys_sysfs"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 140
                  , ( "sys_getpriority"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 141
                  , ( "sys_setpriority"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 142
                  , ( "sys_sched_setparam"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 143
                  , ( "sys_sched_getparam"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 144
                  , ( "sys_sched_setscheduler"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 145
                  , ( "sys_sched_getscheduler" , WordArgType , [ WordArgType ] )
                  )
                , ( 146
                  , ( "sys_sched_get_priority_max" , WordArgType , [ WordArgType ] )
                  )
                , ( 147
                  , ( "sys_sched_get_priority_min" , WordArgType , [ WordArgType ] )
                  )
                , ( 148
                  , ( "sys_sched_rr_get_interval"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 149
                  , ( "sys_mlock" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 150
                  , ( "sys_munlock" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 151 , ( "sys_mlockall" , WordArgType , [ WordArgType ] ) )
                , ( 152 , ( "sys_munlockall" , WordArgType , [] ) )
                , ( 153 , ( "sys_vhangup" , WordArgType , [] ) )
                , ( 154
                  , ( "sys_modify_ldt"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 155
                  , ( "sys_pivot_root"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 156 , ( "sys_sysctl" , WordArgType , [ WordArgType ] ) )
                , ( 157
                  , ( "sys_prctl"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 158
                  , ( "sys_arch_prctl"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 159 , ( "sys_adjtimex" , WordArgType , [ WordArgType ] ) )
                , ( 160
                  , ( "sys_setrlimit" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 161 , ( "sys_chroot" , WordArgType , [ WordArgType ] ) )
                , ( 162 , ( "sys_sync" , WordArgType , [] ) )
                , ( 163 , ( "sys_acct" , WordArgType , [ WordArgType ] ) )
                , ( 164
                  , ( "sys_settimeofday"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 165
                  , ( "sys_mount"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 166
                  , ( "sys_umount" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 167
                  , ( "sys_swapon" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 168 , ( "sys_swapoff" , WordArgType , [ WordArgType ] ) )
                , ( 169
                  , ( "sys_reboot"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 170
                  , ( "sys_sethostname"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 171
                  , ( "sys_setdomainname"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 172 , ( "sys_iopl" , WordArgType , [ WordArgType ] ) )
                , ( 173
                  , ( "sys_ioperm"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 175
                  , ( "sys_init_module"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 176
                  , ( "sys_delete_module"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 179
                  , ( "sys_quotactl"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 186 , ( "sys_gettid" , WordArgType , [] ) )
                , ( 187
                  , ( "sys_readahead"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 188
                  , ( "sys_setxattr"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 189
                  , ( "sys_lsetxattr"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 190
                  , ( "sys_fsetxattr"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 191
                  , ( "sys_getxattr"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 192
                  , ( "sys_lgetxattr"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 193
                  , ( "sys_fgetxattr"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 194
                  , ( "sys_listxattr"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 195
                  , ( "sys_llistxattr"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 196
                  , ( "sys_flistxattr"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 197
                  , ( "sys_removexattr"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 198
                  , ( "sys_lremovexattr"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 199
                  , ( "sys_fremovexattr"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 200
                  , ( "sys_tkill" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 201 , ( "sys_time" , WordArgType , [ WordArgType ] ) )
                , ( 202
                  , ( "sys_futex"
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
                  , ( "sys_sched_setaffinity"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 204
                  , ( "sys_sched_getaffinity"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 206
                  , ( "sys_io_setup" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 207 , ( "sys_io_destroy" , WordArgType , [ WordArgType ] ) )
                , ( 208
                  , ( "sys_io_getevents"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 209
                  , ( "sys_io_submit"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 210
                  , ( "sys_io_cancel"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 212
                  , ( "sys_lookup_dcookie"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 213 , ( "sys_epoll_create" , WordArgType , [ WordArgType ] ) )
                , ( 216
                  , ( "sys_remap_file_pages"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 217
                  , ( "sys_getdents64"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 218
                  , ( "sys_set_tid_address" , WordArgType , [ WordArgType ] )
                  )
                , ( 219 , ( "sys_restart_syscall" , WordArgType , [] ) )
                , ( 220
                  , ( "sys_semtimedop"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 221
                  , ( "sys_fadvise64"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 222
                  , ( "sys_timer_create"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 223
                  , ( "sys_timer_settime"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 224
                  , ( "sys_timer_gettime"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 225
                  , ( "sys_timer_getoverrun" , WordArgType , [ WordArgType ] )
                  )
                , ( 226 , ( "sys_timer_delete" , WordArgType , [ WordArgType ] ) )
                , ( 227
                  , ( "sys_clock_settime"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 228
                  , ( "sys_clock_gettime"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 229
                  , ( "sys_clock_getres"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 230
                  , ( "sys_clock_nanosleep"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 231 , ( "sys_exit_group" , WordArgType , [ WordArgType ] ) )
                , ( 232
                  , ( "sys_epoll_wait"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 233
                  , ( "sys_epoll_ctl"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 234
                  , ( "sys_tgkill"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 235
                  , ( "sys_utimes" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 237
                  , ( "sys_mbind"
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
                , ( 238
                  , ( "sys_set_mempolicy"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 239
                  , ( "sys_get_mempolicy"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 240
                  , ( "sys_mq_open"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 241 , ( "sys_mq_unlink" , WordArgType , [ WordArgType ] ) )
                , ( 242
                  , ( "sys_mq_timedsend"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 243
                  , ( "sys_mq_timedreceive"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 244
                  , ( "sys_mq_notify" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 245
                  , ( "sys_mq_getsetattr"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 246
                  , ( "sys_kexec_load"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 247
                  , ( "sys_waitid"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 248
                  , ( "sys_add_key"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 249
                  , ( "sys_request_key"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 250
                  , ( "sys_keyctl"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 251
                  , ( "sys_ioprio_set"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 252
                  , ( "sys_ioprio_get"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 253 , ( "sys_inotify_init" , WordArgType , [] ) )
                , ( 254
                  , ( "sys_inotify_add_watch"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 255
                  , ( "sys_inotify_rm_watch"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 256
                  , ( "sys_migrate_pages"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 257
                  , ( "sys_openat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 258
                  , ( "sys_mkdirat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 259
                  , ( "sys_mknodat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 260
                  , ( "sys_fchownat"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 261
                  , ( "sys_futimesat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 262
                  , ( "sys_newfstatat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 263
                  , ( "sys_unlinkat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 264
                  , ( "sys_renameat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 265
                  , ( "sys_linkat"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 266
                  , ( "sys_symlinkat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 267
                  , ( "sys_readlinkat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 268
                  , ( "sys_fchmodat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 269
                  , ( "sys_faccessat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 270
                  , ( "sys_pselect6"
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
                , ( 271
                  , ( "sys_ppoll"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 272 , ( "sys_unshare" , WordArgType , [ WordArgType ] ) )
                , ( 273
                  , ( "sys_set_robust_list"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 274
                  , ( "sys_get_robust_list"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 275
                  , ( "sys_splice"
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
                , ( 276
                  , ( "sys_tee"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 277
                  , ( "sys_sync_file_range"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 278
                  , ( "sys_vmsplice"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 279
                  , ( "sys_move_pages"
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
                , ( 280
                  , ( "sys_utimensat"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 281
                  , ( "sys_epoll_pwait"
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
                , ( 282
                  , ( "sys_signalfd"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 283
                  , ( "sys_timerfd_create"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 284 , ( "sys_eventfd" , WordArgType , [ WordArgType ] ) )
                , ( 285
                  , ( "sys_fallocate"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 286
                  , ( "sys_timerfd_settime"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 287
                  , ( "sys_timerfd_gettime"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 288
                  , ( "sys_accept4"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 289
                  , ( "sys_signalfd4"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 290
                  , ( "sys_eventfd2" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 291 , ( "sys_epoll_create1" , WordArgType , [ WordArgType ] ) )
                , ( 292
                  , ( "sys_dup3"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 293
                  , ( "sys_pipe2" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 294 , ( "sys_inotify_init1" , WordArgType , [ WordArgType ] ) )
                , ( 295
                  , ( "sys_preadv"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 296
                  , ( "sys_pwritev"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 297
                  , ( "sys_rt_tgsigqueueinfo"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 298
                  , ( "sys_perf_event_open"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 299
                  , ( "sys_recvmmsg"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 300
                  , ( "sys_fanotify_init"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 301
                  , ( "sys_fanotify_mark"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 302
                  , ( "sys_prlimit64"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 303
                  , ( "sys_name_to_handle_at"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 304
                  , ( "sys_open_by_handle_at"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 305
                  , ( "sys_clock_adjtime"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 306 , ( "sys_syncfs" , WordArgType , [ WordArgType ] ) )
                , ( 307
                  , ( "sys_sendmmsg"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 308
                  , ( "sys_setns" , WordArgType , [ WordArgType , WordArgType ] )
                  )
                , ( 309
                  , ( "sys_getcpu"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 310
                  , ( "sys_process_vm_readv"
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
                , ( 311
                  , ( "sys_process_vm_writev"
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
                , ( 312
                  , ( "sys_kcmp"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 313
                  , ( "sys_finit_module"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 314
                  , ( "sys_sched_setattr"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 315
                  , ( "sys_sched_getattr"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 316
                  , ( "sys_renameat2"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 317
                  , ( "sys_seccomp"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 318
                  , ( "sys_getrandom"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 319
                  , ( "sys_memfd_create"
                    , WordArgType
                    , [ WordArgType , WordArgType ]
                    )
                  )
                , ( 320
                  , ( "sys_kexec_file_load"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                , ( 321
                  , ( "sys_bpf"
                    , WordArgType
                    , [ WordArgType , WordArgType , WordArgType ]
                    )
                  )
                , ( 322
                  , ( "sys_execveat"
                    , WordArgType
                    , [ WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      , WordArgType
                      ]
                    )
                  )
                ]
-- Unknown system calls:
-- 134 uselib
-- 174 create_module
-- 177 get_kernel_syms
-- 178 query_module
-- 180 nfsservctl
-- 181 getpmsg
-- 182 putpmsg
-- 183 afs_syscall
-- 184 tuxcall
-- 185 security
-- 205 set_thread_area
-- 211 get_thread_area
-- 214 epoll_ctl_old
-- 215 epoll_wait_old
-- 236 vserver
