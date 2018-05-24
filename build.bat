dir "C:\Program Files\LLVM\bin"

llvm-config-5.0

set TARGET=kai.exe

set CFLAGS= -nologo -Oi -MP -FC -GS- -EHsc- -GR-
set CFLAGS=%CFLAGS% -Od -MDd -Z7 /TC /c

set LFLAGS= kernel32.lib -incremental:no -opt:ref -subsystem:console -debug

cl %CFLAGS% "src\main.c" /link %LFLAGS% -OUT:%TARGET%
