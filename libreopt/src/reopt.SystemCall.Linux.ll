

; This returns a struct to allow other arches (looking at you,
; FreeBSD) to return multiple values.
define { i64 } @reopt.SystemCall.Linux  (i64 %rdi, i64 %rsi, i64 %rdx, i64 %r10, i64 %r8, i64 %r9, i64 %rax) alwaysinline nounwind
{
        %sysret = tail call i64 asm sideeffect "syscall"
                                               , "={rax},{rdi},{rsi},{rdx},{r10},{r8},{r9},{rax}
                                                 ,~{memory}{rdi}{rsi}{rdx}{r10}{r8}{r9}{rcx}{r11}"
                                               (i64 %rdi, i64 %rsi, i64 %rdx, i64 %r10, i64 %r8, i64 %r9, i64 %rax)
        %ret = insertvalue { i64 } undef, i64 %sysret, 0
        ret { i64 } %ret
}
