- [ ] Error on files over 4GB. This allows us to use a u32 instead of u64
- [ ] Make (Aggregate|Enum)Item names into Ident's instead of `const char *`
- [ ] Make parseIdent return an AstNode
- [ ] `Token start = p->tok; nextToken()` -> `Token start = eatToken()`
- [ ] Check the defer stack is actually having pops


TODO's
- Mask inline union tags on matching?
- Fix file and package scope name mangling
- Fix file position abstraction
- Description of all Nodes
- Update token terminology around terminators
- They aren’t always semicolons and I think we call them that
- Allow array literals of known length to have 0 values in their literals
- Limit selector implicit dereferencing on pointers 
- Make the caller provide an Operand allowing constraints to be propigated downwards (this is particularly important for union selectors as lvalues)


TBD's
- Checkup on UnionType.tag it is supported in part, not sure if in full it seems to be a member with name Tag that can be used to override settings for the union tag
- What implications does this have ie: for switching (one would expect if the custom tag type is an enum that you would match the enum cases of the tag instead of union member names)
- This seems like a bad idea for a feature
- Instead we should allow type extraction via the types TypeInfo
- Workspace support
- Compile time statements
- Type assertions for any and union 
- Support builtin packages
- Change LLVM backend to generate a single object file
- .h file generation
- Context system
- Allocators
- Store preamble on nodes to reprint them exactly
- Consider generation of specialisation files
- Use these to reference
- Change SourcePackage abstraction to align with Workspace
- When there is no single top level file generate one that imports all top level files and directories
- Reenable multithreading, when we have an unresolved entity we can return all the way back up to the thread pooling loop. Here we would reQueue the entity to be re-checked

