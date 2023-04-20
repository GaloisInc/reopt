# Header resolution

Reopt can be helped by providing a C header file (for now, just one).

This builds an `AnnDeclarations` data type, which contains two maps:

- `typeDefs`, a map resolving C `typedef`s to their type (an `AnnType`),

- `funDecls`, a map resolving function names to their function type (an
  `AnnFunType`).

The former is simply used to resolve the type of `typedef`-ed types at their use
sites.

The latter is used to build a `FunTypeMaps` in `headerTypeMap`, during
discovery.
