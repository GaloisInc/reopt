# X86 argument analysis

This is just a very thin error-handling wrapper over Macaw's function demand
analysis.  The idea is that we want to know what registers every functions uses
or sets, but we only get this information easily when we have access to C
headers, ELF debug information, etc.  In the absence of such information, we use
Macaw's function demand analysis to compute what registers a function demands.

We need this information to run Macaw's "register use" analysis (also called
"block invariant inference") during [recovery](./Recovery.md), as Macaw wants an
oracle for register usage by functions in order to compute register usage for
every single block.

## Inputs
- System call personality
- A map resolving addresses to function names
- A map resolving function names to their X86 function type
- Final discovery state

## Outputs
- A map from addresses to X86 function types (for those we suceeded)
- A map from addresses to explanations for argument analysis failure

## How it works

1.  For every function whose type we must infer, for every block of such function,
    `x86CallRegs` computes the registers that the block execution depends upon,
    relying on Macaw's `functionDemands` analysis.

2.  We infer an X86 function type from the results of the demand analysis.
    Essentially this just follows the registers computed in the previous step,
    but it computes the necessary prefixes of contiguous registers necessary.
    For instance, if a function demands registers 0 and 3, we actually need to
    give it type (r0 -> r1 -> r2 -> r3 -> ret), supposedly to follow the calling
    convention.
