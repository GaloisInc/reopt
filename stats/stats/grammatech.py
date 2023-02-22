from enum import Enum
from typing import List, Optional


class Binary(Enum):
    HELLO = "hello"
    ANOPE = "anope"
    SAMBA = "samba"
    QMAIL = "qmail"
    LEAFNODE = "leafnode"
    ZIP = "zip"
    POSTFIX = "postfix"
    MEMCACHED = "memcached"


class Compiler(Enum):
    CLANG = "clang"
    GCC = "gcc"
    ICX = "icx"
    OLLVM_BCF = "ollvm.bcf"
    OLLVM_FLA = "ollvm.fla"
    OLLVM_SUB = "ollvm.sub"


class OptimizationLevel(Enum):
    O0 = "O0"
    O1 = "O1"
    O2 = "O2"
    O3 = "O3"
    OF = "Of"
    OS = "Os"
    NoOptimizationLevel = ""


class PIE(Enum):
    Yes = True
    No = False


class Strip(Enum):
    Yes = True
    No = False


def optimization_levels_for_compiler(compiler: Compiler) -> List[OptimizationLevel]:
    """
    Returns the valid optimization levels for the given compiler.
    """
    match compiler:
        case Compiler.OLLVM_BCF:
            return [OptimizationLevel.NoOptimizationLevel]
        case Compiler.OLLVM_FLA:
            return [OptimizationLevel.NoOptimizationLevel]
        case Compiler.OLLVM_SUB:
            return [OptimizationLevel.NoOptimizationLevel]
        case _:
            return [
                o
                for o in OptimizationLevel
                if o != OptimizationLevel.NoOptimizationLevel
            ]


ALL_CONFIGURATIONS = [
    (c, o, p, s)
    for c in Compiler
    for o in optimization_levels_for_compiler(c)
    for p in PIE
    for s in Strip
]


# Hardcoding this, we can make it dynamic if it ends up changing frequently
# Note: it's very easy to accidentally send the values of the enum instead of
# their tags, so I am programming very defensively in here.
def get_column_for_options(
    # binary: Binary,
    compiler: Compiler,
    optimization: OptimizationLevel,
    pie: PIE,
    strip: Strip,
    initial_column_offset: int = 0,  # Reserved columns
    columns_per_configuration: int = 1,  # Can reserve more than 1 column if needed
) -> Optional[int]:

    # Defensive measures
    if compiler not in Compiler:
        raise Exception(f"Unknown compiler: {compiler}")
    if optimization not in OptimizationLevel:
        raise Exception(f"Unknown optimization level: {optimization}")
    if pie not in PIE:
        raise Exception(f"Expected boolean for PIE, got: {pie}")
    if strip not in Strip:
        raise Exception(f"Expected boolean for strip, got: {pie}")

    configuration_index = ALL_CONFIGURATIONS.index((compiler, optimization, pie, strip))

    # Columns start at 1
    return 1 + initial_column_offset + configuration_index * columns_per_configuration
