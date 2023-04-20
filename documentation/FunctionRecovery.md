# Function recovery

The overall goal of the function recovery is to lift the Macaw intermediate
representation to a slightly higher-level representation, named `FnRep`.  The
main difference seems to be that machine registers have been replaced with
actual values.

Note that there is a lot of code there that is solely for the purpose of Reopt
VCG, and does not actually help at all in lifting and converting to LLVM.

## Inputs
- Macaw system call personality
- Macaw memory
- Macaw function discovery information
- Inferred Macaw block invariants
- Name of the function being recovered
- X86 types for the arguments to the function
- X86 types for the return values

## Outputs
- Recovery error, or
- `RecoveredFunction`

## How it works

1.  First we recover the entry block of the function via `evalRecover`, which
    calls `recoverBlock` after some setup.

2.  To recover a block, we first recover all statements of the block, then we
    deal with the terminator of the block.

3.  To recover block statements, we dispatch on the type of statement.  This is
    done in the `Recover` monad, a state monad with state `RecoverState`,
    gathering information we recover.

    Macaw `Value`s are systematically turned into `FnValues`.  There are three
    cases.

    1.  A Macaw `CValue` is a constant value, for basic types, we turn it into the
        corresponding `FnValue`, for most other values, we fail.

    2.  A Macaw `Initial` value means "the initial value of the given register".
        We ask Macaw what the block invariant tells us about this value.

        If it is an initialized, bound location, we can inspect its value and
        convert it appropriately.

        If it's supposed to be the value of a callee-saved register, we fail.

        If it's some unknown value on the stack, we also fail.

    3.  A Macaw `AssignedValue` means the value returned by a previous
        assignment.  We keep track of what values are assigned for each
        assignment already, so this is just a lookup.

    When the recovered statement is an assignment, and only when its return
    value is actually used, we record the value assigned for the given
    `AssignId`.

    When the recovered statement is a memory write, and the value written is
    live, we convert the value and register the write in `rsWriteMap`.

    Architecture-specific statements are conserved as-is, only their values are
    converted.

    Other statements are either trivial or not supported.

4.  We now handle the terminator of the block we were recovering.  For calls we
    actually parsed, this involves recovering the call target, then doing
    something different for tail calls and non-tail calls.

5.  To recover a call target, we just grab it from the block invariants map.
    Note that thanks to the `ArchFunType` type family, those already contain our
    `FnValue`s and X86 types.  This gives us the address of the target (as an
    `FnValue`), and a description of what registers are passed as arguments and
    returned.  We can evaluate what these register values are to know what
    actual values are passed (again, as an `FnValue`).

6.  If the call is a tail call (`Nothing` in the return address `Maybe`), we
    check that the actual returned registers correspond to the wanted registers
    of the current function.

    If it is true, we produce a `FnTailCall` with the computer call target and
    actual arguments.

    If it is false, something complicated happens that I haven't fully
    understood yet.

7.  If the call is **not** a tail call, we build the map of values being
    returned, add a `FnCall` statement to the current function (in our state
    monad).  Finally, we return the termination statement, a `FnJump`.

8.  There are also other cases for jumps, branches, lookup tables, returns.
    They are of a similar nature, though much simpler than calls.  Essentially
    we compute `FnValue`s wherever we have Macaw values, and return the
    appropriate control flow constructor of the `FnTermStmt` data type.

8.  Now we recover all other (than the entry block) blocks of the function, via
    `recoverInnerBlock`.  This is almost the same as the recovery for the entry
    block, except this time around the blocks have predecessors.
