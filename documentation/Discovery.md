# Discovery

## Inputs
- Types from "Header resolution"
- ELF header information
- Architecture information
- Initial discovery state
- Reopt options

## Outputs
- Map of function types
- Final discovery state

## How it works

1. Parses the dynamic dependencies of the ELF header info, and computes the
  function type of external symbols. (`findDynamicDependencyDebugInfo`)

2. Builds the annotated function type map. (`headerTypeMap`)

    This includes:
    - types for functions found from the ELF header,
    - types from dynamic dependencies,
    -
    - and some hardcoded types for some standard functions (as a crutch,
      particularly for printf-style functions).

    The map is actually made of multiple maps:
    - a bidirectional map between symbols names and addresses,
    - a map from external, undefined symbol names to their expected type,
    - a map from addresses that are function entries to their type,
    - something called a "no return map" in the code, that seems to be initialized
      with addresses that are believed to be function entry points.

3.  Computes more function types using the ELF debug information.
    (`resolveDebugFunTypes`)

4.  At this point, Reopt calls Macaw to run its incremental discovery algorithm
    (`incCompleteDiscovery`).  From this comes back the Macaw `DiscoveryState`,
    which is the remaining piece for the final output.
