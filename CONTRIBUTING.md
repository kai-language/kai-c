# Contributing

## Data Structures
TODO: a list and overview of all the data structures in the project.

## Compilation Overview
The stages of compilation the Kai compiler goes through.

### macOS
1) Read file
2) Lexer
3) Parser
4) Checker
5) LLVM codegen
6) ld - linker
7) dsymutil - debug symbol linker (if debug/-g)