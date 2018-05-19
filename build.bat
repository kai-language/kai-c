@echo off

set TARGET=kai.exe

set CFLAGS= -nologo -Oi -TP -Gm -MP -FC -GC- -EHsc -GR-
set CFLAGS=%CFLAGS% -Od -MDd -Z7

set LFLAGS= kernel32.lib -incremental:no -opt:ref -subsystem:console -debug

cl %CFLAGS% "src\main.c" /link %LFLAGS% -OUT:%TARGET%
