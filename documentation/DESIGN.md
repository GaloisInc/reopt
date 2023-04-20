# Reopt explained

The overall goal of Reopt is to be able to lift a binary to some high-level
representation, onto which one may perform program transformations, then to be
able to recompile it.

Basic blocks and the general control-flow graph are obtained through symbolic
simulation via Macaw.  One trick Reopt employs to succeed frequently is to just
copy all blocks it failed to lift as is in the result binary.  This way, failing
to simulate some part of the program does not jeopardize the whole process.

## Reopt workflow

Here is the very high-level view of what happens when we run Reopt with the
expressed intent of producing an output object file.

1.  C Headers are resolved, see [Header Resolution](./HeaderResolution.md)

2.  We initialize Macaw's discovery with X86 support.

3.  We make sure that no symbol in the binary starts with the prefix we will use
    to mark anonymous functions we recovered (usually it's just `reopt_`).

4.  We proceed with [Discovery](./Discovery.md) of the code in the binary,
    leveraging Macaw's facilities.

5.  We continue with [Recovery](./Recovery.md) of functions from the basic
    blocks we obtained through discovery and their pre- and post-execution
    symbolic states.

6.  Module constraints are generated in order for us to do type
    inference/recovery, see [Module Constraints
    Generation](./ModuleConstraintsGeneration.md)

7.  Modules are [compiled into LLVM bitcode](./LLVMBitcodeCompilation.md) based
    on all the information gathered.

8.  An output object file is generated from the LLVM bitcode.
