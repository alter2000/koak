# Language definitions

- AST: ./src/Types/AST.hs
- Parser: ./src/Parser/AST.hs
- tests: ./test/Parser (pls)

# IR definitions

- Types + algebra: ./src/Types/Codegen.hs
- impure: ./src/Lib/Codegen.hs
- JIT: ./src/Lib/JIT.hs

# Tests

- algebras/pure comps: Hedgehog
- parsing/IO: HSpec
