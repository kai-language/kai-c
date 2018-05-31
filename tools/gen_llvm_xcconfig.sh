#!/bin/bash

PATH=${PATH}:/usr/local/opt/llvm/bin

echo
echo    '//:configuration = Debug'
echo -n 'LLVM_CXXFLAGS = '
llvm-config --cxxflags
echo
echo    '//:configuration = Debug'
echo -n 'LLVM_CXXLDFLAGS = '
llvm-config --ldflags --link-static --system-libs --libs | tr '\n' ' '
echo
echo
echo    '//:configuration = Release'
echo -n 'LLVM_CXXFLAGS = '
llvm-config --cxxflags
echo
echo    '//:configuration = Release'
echo -n 'LLVM_CXXLDFLAGS = '
llvm-config --ldflags --link-static --system-libs --libs | tr '\n' ' '
