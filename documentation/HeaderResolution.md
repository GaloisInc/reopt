# Header resolution

Reopt can be helped by providing a C header file (for now, just one).  This is
because we can immediately tell what registers a function will need when called
and use when returning just looking at the C signature of a function symbol.

Without it, we must rely on successfully exploring all symbolic paths through
the function execution to witness what registers are used, which may fail for a
variety of reasons (see [Discovery](./Discovery.md)).

The header resolution step builds an `AnnDeclarations` data type, which contains
two maps:

-   `typeDefs`, a map resolving C `typedef`s to their C type (an `AnnType`),

-   `funDecls`, a map resolving function names to their C function type (an
    `AnnFunType`).

The former is simply used to resolve the type of `typedef`-ed type aliases at
their use sites.

The latter is used during [discovery](./Discovery.md) to tell what registers a
function will use.
