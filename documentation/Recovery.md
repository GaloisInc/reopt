# X86 recovery

## Inputs
- Bytestring prefix to use when generating new functions in the output
    (usually `reopt_`)
- Syscall personality?
- Symbolic address map
- Function type map
- Discovery state

## Outputs
The contents of a `RecoverX86Output` struct, which contains:
- `RecoveredModule` structure
- `MergeRelations` structure
- A log of the events that happened during recovery
- A summary of recovery failures

## How it works

1.  A known function type map is built, mapping function names to their address
    and return type.  This is built from two pieces.

    First, we include functions for all addresses in the `addrTypeMap` of the
    `debugTypeMap` for which we can turn their "Reopt function type" into an X86
    function type.  Those are addresses of functions in our binary.  It may fail
    because the return type is unsupported (e.g. integral types of width > 64),
    or because we do not currently support floats (why?).

    Second, we include functions for all names in the `nameTypeMap` of the
    `debugTypeMap` for which we can both turn the "Reopt" function type into an
    X86 function type, and resolve the name to a single address.  Entries in
    this map are supposedly external, undefined symbols for which we have a
    declared type.

    Note: The "X86 function type" is really just a description of what registers
    go in and what registers come out.  In a nutshell, the type of each
    argument, as well as the order of the arguments, will dictate what register
    each argument ends up in.  Doubles draw in from a list of so-called XMM
    registers, while 64-bit values draw from a list of general-purpose
    registers.

    TODO: how will this be used?

2.  A `FunUseMap` is computed for all regions.  This is quite poorly documented,
    but it seems like it stores, for each region, a bunch of non-overlapping
    offset ranges and what functions live in those ranges.

    It seems like it may be used when Reopt wants to overwrite the code for a
    function, likely as a way of ensuring that the rewrite fits in place where
    the old function code was.

3.  A "function name" map is built, mapping addresses to a name to use for a
    function at that address.

    TODO: from what is it created?

    TODO: why is it useful?

4.  The [X86 argument analysis](./ArgumentAnalysis.md) happens over all
    functions found during discovery.  Essentially, it computes what registers
    each function takes in as inputs or returns as output.

    See the related section for details.

5.  The known function type map is expanded with functions successfully found
    during discovery, for which we ensure a name is available, and to which we
    give an "X86 function type" as witnessed by the argument analysis.

6.  Recovery truly begins.  For each function explored during discovery, we
    attempt recovery, unless either:

    - it has "printf"-style type,
    - it has the type of the "open" function,
    - not all reachable paths have a valid termination statement (likely because
    discovery failed for some termination statement?),
    - the function is incomplete (also a discovery failure?),
    - the function is behind a Procedure Linkage Table (PLT).

7.  For those valid functions, we run Macaw's block invariant inference.  On the
    happy path, this returns a Macaw `BlockInvariantMap`.  This is used all
    throughout function recovery, as it contains all the information about
    symbolic values and whether they are used or discarded, at all block
    boundaries.

8.  If we successfully got block invariants for the function blocks, we now
    perform [function recovery](./FunctionRecovery.md).  On the happy path, this
    returns a `RecoveredFunction`.

9.  Reopt prepares the list of all functions for which we have recovered a
    definition.  Then, for all functions referenced, but not defined in the
    current module, it creates a list of function declarations, with their name,
    X86 function type, and whether they return or are entry points.  This is all
    the data needed to create a `RecoveredModule` structure.

10. Finally, the `MergeRelations` structure is computed, a data structure
    listing all functions locally defined, along with their offset in the object
    file and the size of their code, as well as all external referenced
    functions, with their name and their expected offset in the binary.

    Supposedly this information will help when creating the new binary, allowing
    us to either overwrite the old code if the new code fits, or possibly write
    a new definition elsewhere and update all references to point to that new
    version.
