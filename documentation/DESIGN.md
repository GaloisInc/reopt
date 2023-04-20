# Reopt explained

## Reopt workflow

1.  Headers are resolved, see [Header Resolution](./HeaderResolution.md)

2.  We initialize Macaw's discovery for X86.

3.  We make sure that no symbol in the binary starts with the prefix we will use
    to mark anonymous functions we recovered.

4.  We proceed with [Discovery](./Discovery.md).

5.  We continue with [Recovery](./Recovery.md).

6.  Module constraints are generated, see [Module Constraints Generation](./ModuleConstraintsGeneration.md)

7.  Modules are compiled into LLVM bitcode.

8.  An output object file is generated from the LLVM bitcode.
