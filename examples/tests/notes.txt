# Using `radss_compare` to prove equivalence between variants

Run `radss_compare --sym-exec`:

    radss_compare --sym-exec <FILENAME1> --addr1 <FUNCTIONADDR1> \
      <FILENAME2> --addr2 <FUNCTIONADDR2> \
      --rip <RIPMAPFILE> --gpr <GPRMAPFILE>

It produces an SMT file `/tmp/sat_trans_*` for each entry in
`<RIPMAPFILE>` that is an SMT formula satisfiable if there is an
assignment of initial register values obeying `<GPRMAPFILE>` such that
the final values do not obey `<GPRMAPFILE>`. You can do `cvc4 --lang
smt <SMTFILE>` to check if the SMT file is SAT or UNSAT. The
verification is successful if all SMT files are UNSAT.

If some SMT files are SAT, this doesn't necessarily mean the programs
are not equivalent. Rather, it just means the programs are not
equivalent w.r.t. the given `<RIPMAPFILE>` and `<GPRMAPFILE>`. In
particular, the same mapping of registers, as specified in
`<GPRMAPFILE>` is used for all comparison points specified in
`<RIPMAPFILE>`, but in some cases different register equivalence
mappings are needed at different comparison points, especially near
the beginning and end. However, a varying register map is not
currently supported.

The RIP map file relates single addresses on the LHS to potentially
multiple addresses on the RHS. However, in general, we may need to
relate many on left to many on the right, but this is not currently
supported.

## Detailed example: `examples/tests/xorshift.*`

The executable variants `examples/tests/xorshift.1234` and
`examples/tests/xorshift.123456` were presumably produced using the
diversifying compiler, but the details are not documented
anywhere. The C source is `examples/tests/xorshift.c`.

The `<FUNCTIONADDR*>` arguments can be deduced using `objdump -d`:

    $ objdump -d -M intel examples/tests/xorshift.1234 | grep '<main>'
    0000000000401000 <main>:

    $ objdump -d -M intel examples/tests/xorshift.123456 | grep '<main>'
    0000000000401000 <main>:

We can then run `radss_compare --sym-exec`:

    $ rm -f /tmp/sat_trans*
    $ stack exec -- radss_compare --sym-exec \
      examples/tests/xorshift.1234 --addr1 401000 \
      examples/tests/xorshift.123456 --addr2 401001 \
      --rip examples/tests/xorshift.1234_123456.rip_map \
      --gpr examples/tests/xorshift.1234_123456.gpr_map
    $ for f in /tmp/sat_trans*; do echo $f; cvc4 --lang smt $f; done
    /tmp/sat_trans_40101b
    unsat
    /tmp/sat_trans_401036
    unsat
    /tmp/sat_trans_401073
    sat

So, the first two comparisons succeeded, but the the third failed. As
noted above, we expect that this just means we need a non-constant GPR
map here.

The addresses `<ADDR>` in the SMT file names `/tmp/sat_trans_<ADDR>`
come from the LHS addresses in the RIP map:

    $ cat examples/tests/xorshift.1234_123456.rip_map
    fromList [(4198515,[4198520]),(4198454,[4198468]),(4198427,[4198437])]

    $ for n in 4198515  4198520    4198454  4198468    4198427  4198437; do \
      python -c "print '$n |-> ', hex($n)"; done
    4198515 |->  0x401073
    4198520 |->  0x401078
    4198454 |->  0x401036
    4198468 |->  0x401044
    4198427 |->  0x40101b
    4198437 |->  0x401025

Presumably the example RIP maps were constructed by hand, but this is
not documented anywhere. In the future, we expect to derive them from
the breadcrumbs.

Presumably GPR map was also constructed by hand, but again this is not
documented. As a first step going forward, we plan to only handle
cases were the GPR map is constant and the identity, which we can
support as a special case when the `--gpr` argument is not
specified. The format of the GPR map in the `xorshift` example is

    $ cat examples/tests/xorshift.1234_123456.gpr_map
    fromList [(0,0),(8,9),(6,6),(7,7),(2,8),(1,2),(4,4),(5,5)]

## Detailed example: `examples/tests/fib_test.*`

See the `examples/tests/xorshift.*` example above for motivation and
interpretation.

Entry points:

    $ objdump -d -M intel examples/tests/fib_test.1234 | grep '<main>'
    0000000000401000 <main>:
    $ objdump -d -M intel examples/tests/fib_test.123456 | grep '<main>'
    0000000000401000 <main>:

Test:

    $ rm -f /tmp/sat_trans*
    $ stack exec -- radss_compare --sym-exec \
      examples/tests/fib_test.1234 --addr1 401000 \
      examples/tests/fib_test.123456 --addr2 401000 \
      --rip examples/tests/fib_test.1234_123456.rip_map \
      --gpr examples/tests/fib_test.1234_123456.gpr_map
    $ for f in /tmp/sat_trans*; do echo $f; cvc4 --lang smt $f; done
    /tmp/sat_trans_401054
    unsat
    /tmp/sat_trans_40106f
    sat
