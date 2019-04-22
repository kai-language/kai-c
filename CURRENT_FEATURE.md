
# Packages with many files

You can think of the files in a package as being concatenated into 1 big file. This means if 2 symbols have the same name in a single package they may collide.

## Current Status
SourceFile is a new type that contains the details of a file in a package.
A package contains multiple SourceFiles, the parser should be able to parse a file storing the statements into the package that owns it.
Checking may proceed after all files in a package have been parsed. This is because the parser collects the top level statements to allow any order declarations.

## Unsolved
I still want each file to be it's own namespace ... but there are some issues with that ...
- If in a single namespace files `a.kai` & `b.kai` both declare symbol `do_math` then they would simply collide.
- In order to support keeping a concept of having each file as it's own namespace we would need to do something to make symbols _export_ into a parent scope ... ie: `do_math` in `a.kai` would need it's symbol visible in the scope of package `math` and in `a.kai` but not in `b.kai`

```
math
├── add.kai
│   └── add
└── mul.kai
    └── mul
```

In the above within `add.kai` `mul` should not be visible but within `math` both `mul` and `add` should be.

