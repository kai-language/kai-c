# 10 Apr
- Decided the project needs even more refactoring so we can use clang tools easier.
- I want to shift everything to a single header generated using makeheaders.
- Declare all structures in .c files in `#if INTERFACE` blocks
- Include as the first line of make headers (or alter makeheaders) a #pragma once

# 9 Apr
- Refactoring the parser to use the new lexer, and make positions single u32 values
- The new lexer will be decoupled from string interning by using a callback onname. 
    - It is now up to the parser to intern strings, as such the lexer has no idea about keywords...
    - Keywords will be identified in the onname callback handled by the parser. Within this callback
        the parser will intern names after which it will check if the name is a keyword in the 
        current context. If so it will alter the token that the lexer will return to indicate this.
    - onstr callback will provide a pointer and len into the string text, this will save an allocation
        but will not be zero terminated. We will need to store the length somewhere for this.

# 20 Feb
- [ ] Make independent functions for parsing a file vs parsing misc code
    - The misc code can be used for testing ...
    - The file parser will be required in order to make packages work properly.

# 19 Feb
- [x] Make all nodes completely generic
    - No more `Stmt_Block` in `Stmt_For` for example

This was a bad goal.
- [ ] Remove 5% of code
    - 10085 LOC Initial
    - 9500 LOC Target

# 18 Feb
- [ ] Make a test harness for Kai code
    - Try and hook into the existing C code

# 17 Feb
- [x] Print lines before errors with pointers to the source range.
- [?] Map file names to mmap handles so they can be closed / reused
- [x] Find approach to preventing errors for <invalid>
