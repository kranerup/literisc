# LiteRISC C Compiler

A minimal C compiler written in Common Lisp targeting the liteRISC 32-bit processor architecture.

## Project Structure

```
literisc/
├── c-compiler.lisp      # Main compiler infrastructure, types, symbol tables
├── c-lexer.lisp         # Tokenizer/lexer for C source
├── c-parser.lisp        # Recursive descent parser, AST construction
├── c-optimizer.lisp     # AST-level optimizations (constant folding, propagation, DCE)
├── c-peephole.lisp      # Peephole optimization on generated assembly
├── c-regalloc.lisp      # Register allocation
├── c-codegen.lisp       # Code generation (AST -> liteRISC assembly)
├── c-compiler-tests.lisp # Comprehensive test suite
├── assembler.lisp       # liteRISC assembler (S-expressions -> machine code)
├── disassembler.lisp    # Machine code disassembly
├── emulator.lisp        # liteRISC CPU emulator for testing
├── lrcc.lisp            # Command-line interface
├── literisc.asd         # ASDF system definition
└── tests/               # C testsuite (symlinks to external tests)
```

## Quick Commands

```bash
# Run all tests (unit tests + C compiler tests)
./run-tests.lisp

# Run specific test
./run-tests.lisp test-name

# List available tests
./run-tests.lisp --list

# Compile and run a C file
./lrcc.lisp -r source.c

# Output assembly only
./lrcc.lisp -S source.c

# Optimize for speed (inline mul/div/mod)
./lrcc.lisp -O -r source.c

# Optimize for size (library mul/div/mod)
./lrcc.lisp -Os -r source.c

# Run C testsuite only
cd tests && bash run_tests.sh
```

## Architecture

### Compilation Pipeline
1. **Lexer** (`c-lexer.lisp`): Source → Tokens
2. **Parser** (`c-parser.lisp`): Tokens → AST
3. **Optimizer** (`c-optimizer.lisp`): AST → Optimized AST
4. **Code Generator** (`c-codegen.lisp`): AST → Assembly (S-expressions)
5. **Assembler** (`assembler.lisp`): Assembly → Machine code
6. **Emulator** (`emulator.lisp`): Execute machine code (for testing)

### Key Data Structures

**type-desc** (c-compiler.lisp): Represents C types
- `base`: int, char, void, struct, union
- `pointer-level`: 0 = value, 1 = *, 2 = **, etc.
- `array-size`: nil or integer
- `struct-tag`, `struct-scope`: For struct/union types
- `return-type`: For function pointer types

**ast-node** (c-parser.lisp): AST nodes
- `type`: Node type (e.g., 'binary-op, 'var-ref, 'call, 'if, 'while)
- `value`: Associated value (operator, variable name, etc.)
- `children`: Child nodes
- `result-type`: Computed type (type-desc)

**sym-entry** (c-compiler.lisp): Symbol table entries
- `name`, `type`: Variable/function name and type
- `storage`: :local, :global, :register, :parameter, :function, :static-local
- `offset`: Stack offset, register index, or label

## Supported C Features

- Basic types: int, char, void, short, long, unsigned
- Pointers (including multi-level), arrays, pointer arithmetic
- Structs, unions, anonymous structs/unions
- Enums, typedefs
- Control flow: if/else, while, for, do-while, switch/case, goto
- Operators: arithmetic, bitwise, logical, comparison, compound assignment
- Function calls, function pointers
- Global and local variables, static locals
- Initializer lists with designated initializers
- String literals
- Preprocessor: #define, #include (basic)

## Testing

Tests use the `unit` framework defined in `unit.lisp`:

```lisp
;; In REPL after loading system
(asdf:load-system :literisc)
(c-compiler::test-c-compiler)  ; Run all C compiler tests
```

Test phases are organized by feature area (see `c-compiler-tests.lisp`).

## Common Development Tasks

### Adding a new C feature
1. Update lexer if new tokens needed (`c-lexer.lisp`)
2. Add parsing logic (`c-parser.lisp`)
3. Add code generation (`c-codegen.lisp`)
4. Add tests (`c-compiler-tests.lisp`)

### Debugging compilation
```lisp
;; In REPL
(compile-c "int main() { return 42; }" :annotate t)  ; Show annotated assembly
(run-c-program "int main() { return 42; }")          ; Compile and run
```

### Type system notes
- Arrays of pointers: `int* a[2]` has pointer-level=1, array-size=2
- Check `type-size` for size calculation (arrays checked before pointers)
- Struct scoping: `struct-scope` field tracks definition scope level

## liteRISC Architecture Notes

- 32-bit RISC processor
- 16 general-purpose registers (R0-R15)
- A register (accumulator) for arithmetic
- SP (stack pointer), SRP (subroutine return pointer)
- Stack grows downward
- Word size: 4 bytes, aligned access
