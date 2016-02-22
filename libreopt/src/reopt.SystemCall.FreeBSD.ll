

; This returns a struct to allow other arches (looking at you,
; FreeBSD) to return multiple values.
define { i64, i1 } @reopt.SystemCall.FreeBSD (i64 %rdi, i64 %rsi, i64 %rdx, i64 %r10, i64 %r8, i64 %r9, i64 %rax) alwaysinline nounwind
{
      %r = tail call { i64, i1 } asm sideeffect "syscall
                                                 xorq   %rdx, %rdx
                                                 movq   $$1, %rdi
                                                 cmovb  %rdi, %rdx"
                                                 , "={rax},={rdx},{rdi},{rsi},{rdx},{r10},{r8},{r9},{rax}
                                                   ,~{memory}{rdi}{rsi}{rdx}{r10}{r8}{r9}{rcx}{r11}"
                                                (i64 %rdi, i64 %rsi, i64 %rdx, i64 %r10, i64 %r8, i64 %r9, i64 %rax)
      ret { i64, i1 } %r                                          
}
