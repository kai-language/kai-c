<p align="center">
  <img src="https://user-images.githubusercontent.com/1977704/40034764-627d71d8-5839-11e8-8049-59d932345e21.png" alt="Kai" height="128px"></img>
  <br>
  <br>
  <a href="https://circleci.com/gh/kai-language/kai-c/tree/master">
    <img src="https://circleci.com/gh/kai-language/kai-c/tree/master.svg?style=shield"></img>
  </a>
  <a href="https://travis-ci.org/kai-language/kai-c">
    <img src="https://travis-ci.org/kai-language/kai-c.svg?branch=master"></img>
  </a>
  <a href="https://ci.appveyor.com/project/BrettRToomey/kai-c">
    <img src="https://ci.appveyor.com/api/projects/status/github/kai-language/kai-c?branch=master&svg=true"></img>
  </a>
  <a href="https://discord.gg/jevNkRd">
    <img src="https://img.shields.io/discord/443582991898378240.svg"></img>
  </a>
</p>

An expressive low level programming language.

## Community
Join our community on [Discord](https://discord.gg/jevNkRd).

## Getting Started
In the future, we plan to have binary releases for all 3 platforms. Until then, you will have to install Kai from source.

### System Requirements
Currently, the actively supported platforms are macOS, Windows and Ubuntu (18.04 LTS).
#### macOS
To build on macOS it's recommended that you have the latest [Xcode](https://developer.apple.com/xcode/downloads/) and have installed the command-line tools.

You will also need LLVM 6.0, which can be installed via a package manager:

**[Homebrew](https://brew.sh/)**
```
brew install llvm@6.0
```
and then add the following to your `~/.bash_profile`:
```
export PATH="/usr/local/opt/llvm/bin:$PATH"
```

#### Linux
To build on Ubuntu Linux you will need to have LLVM 6.0, Clang and libz installed. All of which can be installed using apt:

```
apt-get install llvm-6.0-dev clang libz-dev 
```

## Working on Kai

### Building

#### Commandline
```
make
make tests
make release
```

#### Xcode
If you wish to use Xcode to work on Kai on macOS there is an included `.xcodeproj` file configured to build in the same manner as the Makefile. If you add any files make sure you do not include them in the target directly in xcode, instead go to `main.c` and `#include` them there. We do this to keep the number of object files clang produces to a minimum. Less compiler work means quicker compile times ​👍​.

You may need to make changes tot he scheme in order to point the compiler to the files you wish to compile.

#### Windows

On Windows there is `build.bat`. At the moment, the script does not support anything other than building a debug build.

