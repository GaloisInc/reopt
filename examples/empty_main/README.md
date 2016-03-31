This example runs `reopt` on a very simple C program compiled against
Gnu libc, Musl libc, and Diet libc. The input C program has two
variants, `empty_main_args.c` and `empty_main_no_args.c`.

The results of running `make` are included:

- `artifacts/make.out`: stdout and stderr of `make`.
- `artifacts/tmp`: the original binaries from `make exes`.
- `artifacts/reopt_tmp`: the reoptimized binaries from `make reopt`.

Most of the original binaries make `reopt` crash, and those that don't
result in reoptimized versions that segfault. All of the originals
build, and return exit code 0 when run.

Segfaulting reoptimized binaries
================================

The reoptimized binaries for diet libc -- the only libc version that
`reopt` can reoptimize without crashing -- segfault by entering an
infinite loop and blowing the stack. Below is assembly starting at the
infinite loop for each variant, via `gdb`.

In `empty_main_no_args_diet_clang.reopt`, the loop happens with calls
to 0x80067c. This function always loops, and is called many places in
the nearby code:

    0x80067c:    push   rax
    0x80067d:    mov    rdi,QWORD PTR [rsp+0x8]
    0x800682:    lea    rsi,[rsp+0x10]
    0x800687:    lea    rdx,[rsp+rdi*8+0x18]
    0x80068c:    mov    QWORD PTR ds:0x600560,rdx
    0x800694:    call   0x80067c
    0x800699:    mov    rsi,rdx
    0x80069c:    call   0x80067c
    0x8006a1:    mov    rdi,rax
    0x8006a4:    mov    rsi,rdx
    0x8006a7:    call   0x80067c
    0x8006ac:    pop    rcx
    0x8006ad:    ret    
    0x8006ae:    nop    DWORD PTR [rax+0x0]
    0x8006b2:    nop    WORD PTR cs:[rax+rax*1+0x0]
    0x8006bc:    push   rbp
    0x8006bd:    mov    rbp,rsp
    0x8006c0:    mov    rax,rsp
    0x8006c3:    lea    rcx,[rax-0x10]
    0x8006c7:    mov    rsp,rcx
    0x8006ca:    mov    DWORD PTR [rax-0x10],0x0
    0x8006d1:    xor    eax,eax
    0x8006d3:    mov    rsp,rbp
    0x8006d6:    pop    rbp
    0x8006d7:    ret    
    0x8006d8:    nop    DWORD PTR [rax+0x0]
    0x8006dc:    push   rbp
    0x8006dd:    mov    rbp,rsp
    0x8006e0:    call   0x80067c
    0x8006e5:    mov    rsp,rbp
    0x8006e8:    pop    rbp
    0x8006e9:    ret    
    0x8006ea:    xchg   ax,ax
    0x8006ec:    sub    rsp,0x18
    0x8006f0:    mov    QWORD PTR [rsp],0x0
    0x8006f8:    call   0x80067c
    0x8006fd:    cmp    rax,0xffffffffffffff7c
    0x800703:    jbe    0x80071d
    0x800705:    neg    eax
    0x800707:    mov    QWORD PTR [rsp+0x10],rax
    0x80070c:    call   0x80067c
    0x800711:    mov    ecx,DWORD PTR [rsp+0x10]
    0x800715:    mov    DWORD PTR [rax],ecx
    0x800717:    mov    rdi,rax
    0x80071a:    mov    rsi,rdx
    0x80071d:    call   0x80067c
    0x800722:    add    rsp,0x18
    0x800726:    ret    
    0x800727:    nop    DWORD PTR [rax+rax*1+0x0]
    0x80072c:    ret    
    0x80072d:    nop    DWORD PTR [rax+rax*1+0x0]
    0x800732:    nop    WORD PTR cs:[rax+rax*1+0x0]
    0x80073c:    mov    eax,0x600568
    0x800741:    ret    
    0x800742:    nop    WORD PTR cs:[rax+rax*1+0x0]
    0x80074c:    push   rbp
    0x80074d:    mov    rbp,rsp
    0x800750:    mov    rax,rdi
    0x800753:    mov    QWORD PTR [rax],rax
    0x800756:    mov    QWORD PTR [rax+0x8],0x0
    0x80075e:    mov    QWORD PTR [rax+0x10],0x0
    0x800766:    mov    DWORD PTR [rax+0x18],0x0
    0x80076d:    mov    edi,0x1002
    0x800772:    mov    rsi,rax
    0x800775:    call   0x80067c

The code and loop for `./empty_main_no_args_diet_gcc.reopt` is very
similar to the previous loop in the `clang` version (the loopy
function here is 0x801124):

    0x801124:    push   rax
    0x801125:    mov    rdi,QWORD PTR [rsp+0x8]
    0x80112a:    lea    rsi,[rsp+0x10]
    0x80112f:    lea    rdx,[rsp+rdi*8+0x18]
    0x801134:    mov    QWORD PTR ds:0x601008,rdx
    0x80113c:    call   0x801124
    0x801141:    mov    rsi,rdx
    0x801144:    call   0x801124
    0x801149:    mov    rdi,rax
    0x80114c:    mov    rsi,rdx
    0x80114f:    call   0x801124
    0x801154:    pop    rcx
    0x801155:    ret    
    0x801156:    nop    DWORD PTR [rax+0x0]
    0x80115a:    nop    WORD PTR cs:[rax+rax*1+0x0]
    0x801164:    push   rbp
    0x801165:    mov    rbp,rsp
    0x801168:    mov    rax,rsp
    0x80116b:    add    rax,0xfffffffffffffff0
    0x80116f:    mov    rsp,rax
    0x801172:    xor    eax,eax
    0x801174:    mov    rsp,rbp
    0x801177:    pop    rbp
    0x801178:    ret    
    0x801179:    nop
    0x80117a:    nop    WORD PTR cs:[rax+rax*1+0x0]
    0x801184:    push   rbp
    0x801185:    mov    rbp,rsp
    0x801188:    call   0x801124
    0x80118d:    mov    rsp,rbp
    0x801190:    pop    rbp
    0x801191:    ret    
    0x801192:    xchg   ax,ax
    0x801194:    sub    rsp,0x18
    0x801198:    mov    QWORD PTR [rsp],0x0
    0x8011a0:    call   0x801124
    0x8011a5:    cmp    rax,0xffffffffffffff7c
    0x8011ab:    jbe    0x8011c5
    0x8011ad:    neg    eax
    0x8011af:    mov    QWORD PTR [rsp+0x10],rax
    0x8011b4:    call   0x801124
    0x8011b9:    mov    ecx,DWORD PTR [rsp+0x10]
    0x8011bd:    mov    DWORD PTR [rax],ecx
    0x8011bf:    mov    rdi,rax
    0x8011c2:    mov    rsi,rdx
    0x8011c5:    call   0x801124
    0x8011ca:    add    rsp,0x18
    0x8011ce:    ret    
    0x8011cf:    nop    DWORD PTR [rax+rax*1+0x0]
    0x8011d4:    ret    
    0x8011d5:    nop    DWORD PTR [rax+rax*1+0x0]
    0x8011da:    nop    WORD PTR cs:[rax+rax*1+0x0]
    0x8011e4:    mov    eax,0x601010
    0x8011e9:    ret    
    0x8011ea:    nop    WORD PTR cs:[rax+rax*1+0x0]
    0x8011f4:    push   rbp
    0x8011f5:    mov    rbp,rsp
    0x8011f8:    mov    rax,rdi
    0x8011fb:    mov    QWORD PTR [rax],rax
    0x8011fe:    mov    QWORD PTR [rax+0x8],0x0
    0x801206:    mov    QWORD PTR [rax+0x10],0x0
    0x80120e:    mov    DWORD PTR [rax+0x18],0x0
    0x801215:    mov    edi,0x1002
    0x80121a:    mov    rsi,rax
    0x80121d:    call   0x801124

And now the similar `empty_main_args_diet_clang.reopt`:

    0x800684:    push   rax
    0x800685:    mov    rdi,QWORD PTR [rsp+0x8]
    0x80068a:    lea    rsi,[rsp+0x10]
    0x80068f:    lea    rdx,[rsp+rdi*8+0x18]
    0x800694:    mov    QWORD PTR ds:0x600568,rdx
    0x80069c:    call   0x800684
    0x8006a1:    mov    rsi,rdx
    0x8006a4:    call   0x800684
    0x8006a9:    mov    rdi,rax
    0x8006ac:    mov    rsi,rdx
    0x8006af:    call   0x800684
    0x8006b4:    pop    rcx
    0x8006b5:    ret    
    0x8006b6:    nop    DWORD PTR [rax+0x0]
    0x8006ba:    nop    WORD PTR cs:[rax+rax*1+0x0]
    0x8006c4:    push   rbp
    0x8006c5:    mov    rbp,rsp
    0x8006c8:    mov    rax,rsp
    0x8006cb:    lea    rcx,[rax-0x20]
    0x8006cf:    mov    rsp,rcx
    0x8006d2:    mov    DWORD PTR [rax-0x14],0x0
    0x8006d9:    mov    DWORD PTR [rax-0x18],edi
    0x8006dc:    mov    QWORD PTR [rax-0x20],rsi
    0x8006e0:    xor    eax,eax
    0x8006e2:    mov    rsp,rbp
    0x8006e5:    pop    rbp
    0x8006e6:    ret    
    0x8006e7:    nop    DWORD PTR [rax]
    0x8006ea:    nop    WORD PTR cs:[rax+rax*1+0x0]
    0x8006f4:    push   rbp
    0x8006f5:    mov    rbp,rsp
    0x8006f8:    call   0x800684
    0x8006fd:    mov    rsp,rbp
    0x800700:    pop    rbp
    0x800701:    ret    
    0x800702:    xchg   ax,ax
    0x800704:    sub    rsp,0x18
    0x800708:    mov    QWORD PTR [rsp],0x0
    0x800710:    call   0x800684
    0x800715:    cmp    rax,0xffffffffffffff7c
    0x80071b:    jbe    0x800735
    0x80071d:    neg    eax
    0x80071f:    mov    QWORD PTR [rsp+0x10],rax
    0x800724:    call   0x800684
    0x800729:    mov    ecx,DWORD PTR [rsp+0x10]
    0x80072d:    mov    DWORD PTR [rax],ecx
    0x80072f:    mov    rdi,rax
    0x800732:    mov    rsi,rdx
    0x800735:    call   0x800684
    0x80073a:    add    rsp,0x18
    0x80073e:    ret    
    0x80073f:    nop    DWORD PTR [rax+rax*1+0x0]
    0x800744:    ret    
    0x800745:    nop    DWORD PTR [rax+rax*1+0x0]
    0x80074a:    nop    WORD PTR cs:[rax+rax*1+0x0]
    0x800754:    mov    eax,0x600570
    0x800759:    ret    
    0x80075a:    nop    WORD PTR cs:[rax+rax*1+0x0]
    0x800764:    push   rbp
    0x800765:    mov    rbp,rsp
    0x800768:    mov    rax,rdi
    0x80076b:    mov    QWORD PTR [rax],rax
    0x80076e:    mov    QWORD PTR [rax+0x8],0x0
    0x800776:    mov    QWORD PTR [rax+0x10],0x0
    0x80077e:    mov    DWORD PTR [rax+0x18],0x0
    0x800785:    mov    edi,0x1002
    0x80078a:    mov    rsi,rax
    0x80078d:    call   0x800684

And finally the similar `empty_main_args_diet_gcc.reopt`:

    0x801124:    push   rax
    0x801125:    mov    rdi,QWORD PTR [rsp+0x8]
    0x80112a:    lea    rsi,[rsp+0x10]
    0x80112f:    lea    rdx,[rsp+rdi*8+0x18]
    0x801134:    mov    QWORD PTR ds:0x601008,rdx
    0x80113c:    call   0x801124
    0x801141:    mov    rsi,rdx
    0x801144:    call   0x801124
    0x801149:    mov    rdi,rax
    0x80114c:    mov    rsi,rdx
    0x80114f:    call   0x801124
    0x801154:    pop    rcx
    0x801155:    ret    
    0x801156:    nop    DWORD PTR [rax+0x0]
    0x80115a:    nop    WORD PTR cs:[rax+rax*1+0x0]
    0x801164:    push   rbp
    0x801165:    mov    rbp,rsp
    0x801168:    mov    rax,rsp
    0x80116b:    lea    rcx,[rax-0x20]
    0x80116f:    mov    rsp,rcx
    0x801172:    mov    DWORD PTR [rax-0x14],edi
    0x801175:    mov    QWORD PTR [rax-0x20],rsi
    0x801179:    xor    eax,eax
    0x80117b:    mov    rsp,rbp
    0x80117e:    pop    rbp
    0x80117f:    ret    
    0x801180:    nop    DWORD PTR [rax+0x0]
    0x801184:    push   rbp
    0x801185:    mov    rbp,rsp
    0x801188:    call   0x801124
    0x80118d:    mov    rsp,rbp
    0x801190:    pop    rbp
    0x801191:    ret    
    0x801192:    xchg   ax,ax
    0x801194:    sub    rsp,0x18
    0x801198:    mov    QWORD PTR [rsp],0x0
    0x8011a0:    call   0x801124
    0x8011a5:    cmp    rax,0xffffffffffffff7c
    0x8011ab:    jbe    0x8011c5
    0x8011ad:    neg    eax
    0x8011af:    mov    QWORD PTR [rsp+0x10],rax
    0x8011b4:    call   0x801124
    0x8011b9:    mov    ecx,DWORD PTR [rsp+0x10]
    0x8011bd:    mov    DWORD PTR [rax],ecx
    0x8011bf:    mov    rdi,rax
    0x8011c2:    mov    rsi,rdx
    0x8011c5:    call   0x801124
    0x8011ca:    add    rsp,0x18
    0x8011ce:    ret    
    0x8011cf:    nop    DWORD PTR [rax+rax*1+0x0]
    0x8011d4:    ret    
    0x8011d5:    nop    DWORD PTR [rax+rax*1+0x0]
    0x8011da:    nop    WORD PTR cs:[rax+rax*1+0x0]
    0x8011e4:    mov    eax,0x601010
    0x8011e9:    ret    
    0x8011ea:    nop    WORD PTR cs:[rax+rax*1+0x0]
    0x8011f4:    push   rbp
    0x8011f5:    mov    rbp,rsp
    0x8011f8:    mov    rax,rdi
    0x8011fb:    mov    QWORD PTR [rax],rax
    0x8011fe:    mov    QWORD PTR [rax+0x8],0x0
    0x801206:    mov    QWORD PTR [rax+0x10],0x0
    0x80120e:    mov    DWORD PTR [rax+0x18],0x0
    0x801215:    mov    edi,0x1002
    0x80121a:    mov    rsi,rax
    0x80121d:    call   0x801124
