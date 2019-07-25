CC = clang
CXX = clang++

target = kai

c_sources := $(wildcard src/*.c)
cpp_sources := $(wildcard src/*.cpp)
disasms := $(pathsubst src/%.c, src/%.S, $(c_sources))

objdir = .build
c_objects := $(patsubst src/%.c, $(objdir)/%.o, $(c_sources))
# cpp_objects := $(patsubst src/%.cpp, $(objdir)/%.o, $(sources))
cpp_objects := $(patsubst src/%.cpp, $(objdir)/%.o, $(cpp_sources))

includes = -Iincludes/ -Ideps/uu.spdr/include

ignored = -Wno-writable-strings -Wno-switch -Wno-c11-extensions -Wno-c99-extensions
cflags = -g -O0 -std=c11 $(includes) -DDEBUG $(ignored) $(CFLAGS)
cxxflags = -std=c++11 -stdlib=libc++ $(includes) -DDEBUG $(ignored)

LLVM_VERSION := 8.0

LLVM_CONFIG := llvm-config

LLVM_CXXFLAGS = $(shell llvm-config --cxxflags)
LLVM_CXXLFLAGS = $(shell llvm-config --ldflags --link-static --system-libs --libs)
# --libs X86AsmParser X86CodeGen Core Support BitReader AsmParser Analysis TransformUtils ScalarOpts Target

test_main = test_main.c
test_log = tests.log

release: cflags = -O3 -std=c11 $(includes) -march=native -DRELEASE $(CFLAGS)

all: mkdirs $(target)

release: clean all

# compile c_objects
$(objdir)/%.o: src/%.c
	$(CC) $(cflags) -o $@ -c $<

# compile cpp_objects
$(objdir)/%.o: src/%.cpp
	$(CXX) $(cxxflags) $(LLVM_CXXFLAGS) -o $@ -c $<

# link
$(target): $(c_objects) $(cpp_objects)
	$(CXX) $(cflags) $(LLVM_CXXLFLAGS) -o $@ $^

# disassembly
src/%.S: src/%.c
	$(CC) $(cflags) -o $@ -S $<
	
disasm: $(disasms)

unity:
	$(CC) $(cflags) -o $(target) unity.c

mkdirs:
	@mkdir -p $(objdir)

mktests: tools/mktests
	./tools/mktests src > test_main.c

mkxctests: tools/mkxctests
	./tools/mkxctests src > test_xcmain.c

tests: clean tools
	@./tools/mktests $(shell find src -maxdepth 1 -type d) > $(test_main)
	@$(CC) -o $@ $(cflags) -DTEST $(test_main)
	@./$@ 2>&1 $(test_log)
	@rm $@ $(test_main)

tools: tools/mktests tools/mkxctests

tools/mktests:
	$(CXX) $(cxxflags) -o $@ tools/mktests.cpp

tools/mkxctests:
	$(CXX) $(cxxflags) -o $@ tools/mkxctests.cpp

generate_db: clean
	intercept-build --override-compiler make CC=intercept-cc CXX=intercept-c++ all

.PHONY: clean

clean:
	rm -rf $(objdir) $(test_log) $(test_main)
 
# Debugging
## allows you to print makefile variables with make-VARIABLE
print-% : ; $(info $* is a $(flavor $*) variable set to [$($*)]) @true

